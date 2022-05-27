{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AnaCompiler.Compile (compile, AnaCompilerException (..), calcTyp, check, TypValidated (..), buildDefEnv, calcProgTyp, buildTypAliasEnv) where

import AnaCompiler.Asm (Arg (Const, Label, Reg, RegOffset), Instruction (..), Reg (R10, R15, RAX, RBX, RCX, RDI, RSP), toAsm)
import AnaCompiler.Expr
import Control.Exception (Exception, throw)
import Control.Monad.State.Lazy
import Data.Bits (Bits (shift))
import Data.IORef
import Data.List
import qualified Data.Maybe
import Data.Validation
import Text.Printf (printf)
import Data.Functor

stackloc :: Int -> Arg
stackloc i = RegOffset (-8 * i) RSP

constTrue :: Int
constTrue = 0xFFFFFFFF

constFalse :: Int
constFalse = 0x7FFFFFFF

constNull :: Int
constNull = 0x00000000

type Eval a = StateT TEnv IO a

findDef :: [Def] -> String -> Maybe Def
findDef defs nameToFind =
  case defs of
    [] -> Nothing
    d@(DFun name _ _ _) : rest ->
      if name == nameToFind then Just d else findDef rest nameToFind

type StackIndex = Int

insertVal :: (String, Int) -> TEnv -> TEnv
insertVal (x, si) env =
  case lookup x env of
    Nothing -> (x, si) : env
    _ -> env

newtype ExprValidated
  = ExprValidated Prog
  deriving (Show)

newtype TypValidated
  = TypValidated Typ
  deriving (Eq, Show)

newtype Error = Error {errors :: [String]}
  deriving (Semigroup, Show)

calcDefTyp :: DefTypEnv -> TypAliasEnv -> Def -> IO TypValidated
calcDefTyp defEnv typAlias (DFun _ args ret body) =
  let lastBodyExpr = last body
      lastBodyExprTyp =
        calcTyp lastBodyExpr args defEnv typAlias
          >>= ( \(TypValidated lastExpr) ->
                  let resolveRetTypeAlias =
                        case ret of
                          TName typAliasArg ->
                            Data.Maybe.fromMaybe ret (lookup typAliasArg typAlias)
                          _ -> ret
                      resolveLastTypeAlias =
                        case lastExpr of
                          TName typAliasArg ->
                            Data.Maybe.fromMaybe ret (lookup typAliasArg typAlias)
                          _ -> lastExpr
                   in if resolveLastTypeAlias == resolveRetTypeAlias
                        then pure $ TypValidated ret
                        else throw $ AnaCompilerException ["Type mismatch: body end is not the same as return typ " ++ show resolveLastTypeAlias ++ " " ++ show resolveRetTypeAlias]
              )
   in lastBodyExprTyp

calcProgTyp :: Prog -> TypEnv -> DefTypEnv -> TypAliasEnv -> IO TypValidated
calcProgTyp (defs, typs, main) typEnv defEnv typAliasEnv =
  mapM_ (calcDefTyp defEnv typAliasEnv) defs
    *> calcTyp main typEnv defEnv typAliasEnv

checkTNumType :: Expr -> TypEnv -> DefTypEnv -> TypAliasEnv -> IO TypValidated
checkTNumType expr typEnv defTypEnv typAlias = do
  TypValidated tpy <- calcTyp expr typEnv defTypEnv typAlias
  case tpy of
    TNum -> pure $ TypValidated TNum
    invalidType -> throw $ AnaCompilerException ["Type mismatch: op must take a number as an argument: " ++ show invalidType]

calcTyp :: Expr -> TypEnv -> DefTypEnv -> TypAliasEnv -> IO TypValidated
calcTyp expr typEnv defTypEnv typAlias =
  case expr of
    ENum _ -> pure $ TypValidated TNum
    EBool _ -> pure $ TypValidated TBool
    EId x ->
      case lookup x typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" x]
        Just typ -> pure $ TypValidated typ
    EPrim1 op e ->
      case op of
        Add1 -> checkTNumType e typEnv defTypEnv typAlias
        Sub1 -> checkTNumType e typEnv defTypEnv typAlias
        IsNum -> pure $ TypValidated TBool
        IsBool -> pure $ TypValidated TBool
        Print -> calcTyp e typEnv defTypEnv typAlias
    EWhile cond _ -> do
      TypValidated tpy <- calcTyp cond typEnv defTypEnv typAlias
      case tpy of
        TBool -> pure $ TypValidated TBool --head $ foldl (\acc e -> calcTyp e typEnv : acc) [] body
        _ -> throw $ AnaCompilerException ["Type mismatch: while condition must take a bool as an argument"]
    ESet _ e -> calcTyp e typEnv defTypEnv typAlias
    ELet bindList bodyList ->
      let localTypEnvIO =
            foldM
              ( \acc (valName, e) -> do
                  TypValidated typ <- calcTyp e acc defTypEnv typAlias
                  return $ (valName, typ) : acc
              )
              typEnv
              bindList
          bodyTypList = do
            localTypEnv <- localTypEnvIO
            foldM
              ( \acc e -> do
                  res <- calcTyp e localTypEnv defTypEnv typAlias
                  return (res : acc)
              )
              []
              bodyList
       in -- the last element of the exprs on the body
          -- for inner functions it would necessary to generate another def typ env???
          fmap last bodyTypList
    EIf condExpr thnExpr elsExpr -> do
      TypValidated tpy <- calcTyp condExpr typEnv defTypEnv typAlias
      case tpy of
        TBool -> do
          TypValidated thnExprType <- calcTyp thnExpr typEnv defTypEnv typAlias
          TypValidated elsExprType <- calcTyp elsExpr typEnv defTypEnv typAlias
          if thnExprType == elsExprType
            then pure $ TypValidated thnExprType
            else throw $ AnaCompilerException ["Type mismatch: if branches must agree on type"]
        _ -> throw $ AnaCompilerException ["Type mismatch: if expects a boolean in conditional position"]
    EPrim2 op e1 e2 ->
      case op of
        Plus -> checkTNumType e1 typEnv defTypEnv typAlias *> checkTNumType e2 typEnv defTypEnv typAlias
        Minus -> checkTNumType e1 typEnv defTypEnv typAlias *> checkTNumType e2 typEnv defTypEnv typAlias
        Times -> checkTNumType e1 typEnv defTypEnv typAlias *> checkTNumType e2 typEnv defTypEnv typAlias
        Less -> (checkTNumType e1 typEnv defTypEnv typAlias *> checkTNumType e2 typEnv defTypEnv typAlias) $> TypValidated TBool
        Greater -> (checkTNumType e1 typEnv defTypEnv typAlias *> checkTNumType e2 typEnv defTypEnv typAlias) $> TypValidated TBool
        Equal -> do
          TypValidated e1Type <- calcTyp e1 typEnv defTypEnv typAlias
          TypValidated e2Type <- calcTyp e2 typEnv defTypEnv typAlias
          if e1Type == e2Type
            then pure $ TypValidated TBool
            else throw $ AnaCompilerException ["Type mismatch: equal sides must agree on type"]
    ENil typ -> pure $ TypValidated typ
    ETuple _ _ finalType -> do
      -- print $ show exp2
      -- TypValidated tailPair <- calcTyp exp2 typEnv defTypEnv typAlias
      -- let typResoluction = case finalType of
      --       (TName customType) ->
      --         case lookup customType typAlias of
      --           Just typ -> typ
      --           Nothing ->
      --             throw $ AnaCompilerException [printf "Type mismatch: unknown pair %s type" (show customType)]
      --       typ -> typ
      pure $ TypValidated finalType
    -- TypValidated headPair <- calcTyp exp1 typEnv defTypEnv typAlias
    -- TypValidated tailPair <- calcTyp exp2 typEnv defTypEnv typAlias
    -- let a = case tailPair of
    --       (TPair tailTyp) -> tailTyp
    --       _ -> tailPair
    -- if headPair == a
    --   then pure $ TypValidated (TPair a)
    --   else throw $ AnaCompilerException ["Type mismatch: pair elements must agree on type"]
    EHead name ->
      case lookup name typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" name]
        Just (TTuple typ) -> pure $ TypValidated typ
        Just (TName customType) ->
          case lookup customType typAlias of
            Just (TTuple finalType) -> pure $ TypValidated finalType
            Just finalType -> pure $ TypValidated finalType
            Nothing -> pure $ throw $ AnaCompilerException [printf "Type mismatch: unknown pair %s type" (show customType)]
        -- Just unkownType -> pure $ throw $ AnaCompilerException [printf "Type mismatch: unknown pair %s type" (show unkownType)]
        Just typ -> pure $ TypValidated typ
    ETail name ->
      case lookup name typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" name]
        Just tupleTail@(TTuple _) -> pure $ TypValidated tupleTail
        Just finalTyp@(TName _) -> pure $ TypValidated finalTyp
        -- Just unkownType -> pure $ throw $ AnaCompilerException [printf "Type mismatch: unknown pair %s type" (show unkownType)]
        Just typ -> pure $ TypValidated typ
    EVector exprs ->
      let exprsTypList = foldl (\acc e -> calcTyp e typEnv defTypEnv typAlias : acc) [] exprs
          headTyp = head exprsTypList
          vecType = do
            res <-
              and
                <$> mapM
                  ( \t1 -> do
                      TypValidated t <- t1
                      TypValidated t2 <- headTyp
                      return (t == t2)
                  )
                  exprsTypList
            if res
              then fmap (\(TypValidated typ) -> TypValidated (TVec typ)) headTyp
              else pure $ throw $ AnaCompilerException [printf "Type mismatch: all elements of vector have to be the same"]
       in vecType
    EGet vec _ ->
      case lookup vec typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" vec]
        Just (TVec typ) -> pure $ TypValidated typ
        Just (TName customType) ->
          case lookup customType typAlias of
            Just (TVec finalType) -> pure $ TypValidated finalType
            Just invalidTyp ->
              throw $ AnaCompilerException [printf "Type mismatch: invalid vec type variable identifier %s unbound" (show invalidTyp)]
            Nothing -> pure $ throw $ AnaCompilerException [printf "Type mismatch: unknown pair %s type" (show customType)]
        Just typ -> pure $ TypValidated typ
    EVecSet vec _ exprVecSet ->
      case lookup vec typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" vec]
        Just vecTyp -> do
          TypValidated newTyp <- calcTyp exprVecSet typEnv defTypEnv typAlias
          if vecTyp == newTyp
            then pure $ TypValidated vecTyp
            else throw $ AnaCompilerException ["Type mismatch: new vec element has to be the same as vec typ"]
    EApp name listParams ->
      case lookup name defTypEnv of
        Just (typ, args) ->
          let resolveTypeAlias =
                map
                  ( \arg@(name, typ) ->
                      case typ of
                        TName typAliasArg ->
                          case lookup typAliasArg typAlias of
                            Just t -> (name, t)
                            Nothing -> arg
                        _ -> arg
                  )
                  args
              paramsCheck =
                sequence (reverse (foldl (\acc e -> calcTyp e typEnv defTypEnv typAlias : acc) [] listParams))
                  >>= ( \paramsTyps ->
                          -- error $ show $ null (paramsTyps \\ map (TypValidated . snd) resolveTypeAlias)
                          if null (paramsTyps \\ map (TypValidated . snd) args)
                            then pure ()
                            else
                              throw $
                                AnaCompilerException
                                  ["Type mismatch:: params type is not the same as argument type " ++ show paramsTyps ++ " " ++ show args]
                      )
           in paramsCheck $> TypValidated typ
        Nothing -> throw $ AnaCompilerException ["Type mismatch: def is not defined"]

wellFormedExprListBody :: [Def] -> [Expr] -> TEnv -> Validation Error ()
wellFormedExprListBody defs exprs localEnv =
  let localErrs =
        foldl
          ( \a b ->
              let vIns = wellFormedE defs b localEnv
                  fE = case vIns of
                    Failure errs -> a <> errs
                    Success _ -> a
               in fE
          )
          (Error [])
          exprs
   in if null (errors localErrs)
        then Success ()
        else Failure localErrs

wellFormedELetExpr :: [Def] -> [(String, Expr)] -> StackIndex -> Error -> TEnv -> (TEnv, Error, StackIndex)
wellFormedELetExpr defs list si accError env =
  foldl
    ( \(accEnv, accErr, si') (x, value) ->
        let vIns = wellFormedE defs value accEnv
            (b, c, d) = case lookup x accEnv of
              Nothing -> ((x, si') : accEnv, accErr, si' + 1)
              _ -> (accEnv, Error [printf "Multiple bindings for variable identifier %s" x] <> accErr, si')
            fE = case vIns of
              Failure errs -> c <> errs
              Success _ -> c
         in (b, fE, d)
    )
    (env, accError, si)
    list

wellFormedE :: [Def] -> Expr -> TEnv -> Validation Error ()
wellFormedE defs expr env =
  case expr of
    ENum _ -> Success ()
    EBool _ -> Success ()
    EId x ->
      case lookup x env of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" x]
        Just _ -> Success ()
    EPrim2 _ e1 e2 ->
      let c1 = wellFormedE defs e1 env
          c2 = wellFormedE defs e2 env
       in c1 *> c2
    EPrim1 _ e1 -> wellFormedE defs e1 env
    EWhile cond body ->
      case wellFormedE defs cond env of
        Success _ -> wellFormedExprListBody defs body env
        Failure errs ->
          case wellFormedExprListBody defs body env of
            Success _ -> Failure errs
            Failure bodyErrs -> Failure $ bodyErrs <> errs
    ESet name e ->
      case lookup name env of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" name]
        Just _ -> wellFormedE defs e env
    ELet list body ->
      let shadowEnv =
            filter
              ( \(name, _) ->
                  case lookup name list of
                    Just _ -> False
                    Nothing -> True
              )
              env
          (localEnv, localErrs, _) = wellFormedELetExpr defs list 0 (Error []) shadowEnv
          bodyC = wellFormedExprListBody defs body localEnv
          c2 = case bodyC of
            Success _ ->
              if null (errors localErrs)
                then Success ()
                else Failure localErrs
            Failure errs -> Failure $ localErrs <> errs
       in c2 --error ("env: " ++ show env ++ "\n list: " ++ show list ++ "\n shadowEnv: " ++ show shadowEnv)
    EIf exp1 exp2 exp3 ->
      wellFormedE defs exp1 env *> wellFormedE defs exp2 env *> wellFormedE defs exp3 env
    ENil typ -> Success ()
    ETuple exp1 exp2 _ ->
      wellFormedE defs exp1 env
        *> case exp2 of
          ETuple {} -> wellFormedE defs exp2 env
          ENil _ -> Success ()
          EId x ->
            case lookup x env of
              Nothing -> Failure $ Error [printf "variable identifier %s unbound" x]
              Just _ -> Success ()
          _ -> Failure $ Error [printf "Invalid tuple form: %s " (show exp2)]
    EHead name ->
      case lookup name env of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" name]
        Just _ -> Success ()
    ETail name ->
      case lookup name env of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" name]
        Just _ -> Success ()
    EVector exprsVec -> wellFormedExprListBody defs exprsVec env
    EGet vec _ ->
      case lookup vec env of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" vec]
        Just _ -> Success ()
    EVecSet vec _ exprSet ->
      case lookup vec env of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" vec]
        Just _ -> wellFormedE defs exprSet env
    EApp nameDef listParams ->
      case findDef defs nameDef of
        Just (DFun _ args _ _) ->
          let appErrors =
                if length listParams == length args
                  then wellFormedExprListBody defs listParams env
                  else case wellFormedExprListBody defs listParams env of
                    Success _ -> Failure $ Error ["Invalid function call with wrong number of arguments"]
                    Failure errs -> Failure $ errs <> Error ["Invalid function call with wrong number of arguments"]
           in appErrors
        Nothing -> Failure $ Error [printf "funtion indentifer %s unbound" nameDef]

newtype AnaCompilerException
  = AnaCompilerException [String]
  deriving (Eq)

instance Show AnaCompilerException where
  show (AnaCompilerException errs) = unwords . words $ unlines errs

instance Exception AnaCompilerException

wellFormedDef :: [Def] -> Def -> Validation Error ()
wellFormedDef defs (DFun _ args _ body) = wellFormedExprListBody defs body (map (\(name, _) -> (name, 1)) args)

wellFormedProg :: Prog -> Validation Error ()
wellFormedProg (defs, typs, main) =
  let defsValidations =
        foldl
          ( \acc def ->
              case wellFormedDef defs def of
                Failure errs -> acc <> errs
                Success _ -> acc
          )
          (Error [])
          defs
      mainValidation = wellFormedE defs main [("input", 1)]
      finalValidation = case mainValidation of
        Failure errs ->
          if null (errors defsValidations)
            then Failure errs
            else Failure $ defsValidations <> errs
        Success _ ->
          if null (errors defsValidations)
            then Success ()
            else Failure defsValidations
   in finalValidation

check :: Prog -> IO ExprValidated
check prog =
  case wellFormedProg prog of
    Success _ -> pure $ ExprValidated prog
    Failure errs -> throw $ AnaCompilerException $ errors errs

type Counter = Int -> IO Int

makeCounter :: IO Counter
makeCounter = do
  r <- newIORef 0
  return
    ( \i -> do
        modifyIORef r (+ i)
        readIORef r
    )

makeLabel :: String -> Counter -> IO String
makeLabel label counter = do
  c <- counter 1
  return $ printf "%s_%s" label (show c)

exprToInstrs :: Expr -> StackIndex -> Counter -> Bool -> [Def] -> Eval [Instruction]
exprToInstrs expr si counter isTailPosition defs =
  case expr of
    EId x -> do
      s <- get
      let a = case lookup x s of
            Nothing -> []
            Just i -> [IMov (Reg RAX) (stackloc i)]
       in pure a
    ENum n -> pure [IMov (Reg RAX) (Const (shift n 1))]
    EBool f ->
      if f
        then pure [IMov (Reg RAX) (Const constTrue)]
        else pure [IMov (Reg RAX) (Const constFalse)]
    EPrim2 prim exp1 exp2 ->
      let exp1InsIO = exprToInstrs exp1 si counter False defs
          exp2InsIO = exprToInstrs exp2 (si + 1) counter False defs
          opForNumIO = do
            exp1Ins <- exp1InsIO
            exp2Ins <- exp2InsIO
            return $
              exp1Ins
                ++ [IMov (stackloc si) (Reg RAX)]
                ++ exp2Ins
                ++ [IMov (stackloc $ si + 1) (Reg RAX)]
                ++ [IMov (Reg RAX) (stackloc si)]
          final_op =
            case prim of
              Plus -> do
                op <- opForNumIO
                return $ op ++ [IAdd (Reg RAX) (stackloc $ si + 1)] -- : [ISub (Reg RAX) (Const 1)]
              Minus -> do
                op <- opForNumIO
                return $ op ++ [ISub (Reg RAX) (stackloc $ si + 1)] -- : [IAdd (Reg RAX) (Const 1)]
              Times -> do
                op <- opForNumIO
                return $
                  op
                    ++ [IXor (Reg RAX) (Const 1)]
                    ++ [ISar (Reg RAX) (Const 1)]
                    -- ++ [IMov (stackloc si) (Reg RAX)] It is not necessary to replace this value. It is only used for this operation
                    ++ [IMul (Reg RAX) (stackloc $ si + 1)]
              -- ++ [ISub (Reg RAX) (stackloc si)]
              -- ++ [IXor (Reg RAX) (Const 1)]
              Equal -> do
                exp1Ins <- exp1InsIO
                exp2Ins <- exp2InsIO
                noEqualBranchLabel <- liftIO $ makeLabel "no_eq" counter
                endCmpBranchLabel <- liftIO $ makeLabel "end_cmp" counter
                return $
                  exp1Ins
                    ++ [IMov (stackloc si) (Reg RAX)]
                    ++ exp2Ins
                    ++ [IMov (stackloc $ si + 1) (Reg RAX)]
                    ++ [ICmp (Reg RAX) (stackloc si)]
                    ++ [IJne noEqualBranchLabel]
                    ++ [IMov (Reg RAX) (Const constTrue)]
                    ++ [IJmp endCmpBranchLabel]
                    ++ [ILabel noEqualBranchLabel]
                    ++ [IMov (Reg RAX) (Const constFalse)]
                    ++ [ILabel endCmpBranchLabel]
              Less -> do
                op <- opForNumIO
                lessBranchLabel <- liftIO $ makeLabel "less_than" counter
                endCmpBranchLabel <- liftIO $ makeLabel "end_cmp" counter
                return $
                  op
                    ++ [ICmp (Reg RAX) (stackloc $ si + 1)]
                    ++ [IJl lessBranchLabel]
                    ++ [IMov (Reg RAX) (Const constFalse)]
                    ++ [IJmp endCmpBranchLabel]
                    ++ [ILabel lessBranchLabel]
                    ++ [IMov (Reg RAX) (Const constTrue)]
                    ++ [ILabel endCmpBranchLabel]
              Greater -> do
                op <- opForNumIO
                greaterBranchLabel <- liftIO $ makeLabel "greater_than" counter
                endCmpBranchLabel <- liftIO $ makeLabel "end_cmp" counter
                return $
                  op
                    ++ [ICmp (Reg RAX) (stackloc $ si + 1)]
                    ++ [IJg greaterBranchLabel]
                    ++ [IMov (Reg RAX) (Const constFalse)]
                    ++ [IJmp endCmpBranchLabel]
                    ++ [ILabel greaterBranchLabel]
                    ++ [IMov (Reg RAX) (Const constTrue)]
                    ++ [ILabel endCmpBranchLabel]
       in final_op
    EIf e1 e2 e3 ->
      let e1isIO = exprToInstrs e1 si counter False defs
          e2isIO = exprToInstrs e2 (si + 1) counter isTailPosition defs -- TODO  this env keeps record of let variables. Is valid to repeat let variables inside blocks?
          e3isIO = exprToInstrs e3 (si + 2) counter isTailPosition defs
          op = do
            e1is <- e1isIO
            e2is <- e2isIO
            e3is <- e3isIO
            elseBranchLabel <- liftIO $ makeLabel "else_branch" counter
            endIfLabel <- liftIO $ makeLabel "end_of_if" counter
            return $
              e1is
                ++ [ICmp (Reg RAX) (Const constFalse)]
                ++ [IJe elseBranchLabel]
                ++ e2is
                ++ [IJmp endIfLabel]
                ++ [ILabel elseBranchLabel]
                ++ e3is
                ++ [ILabel endIfLabel]
       in op
    EPrim1 prim1 exp1 ->
      let expInsIO = exprToInstrs exp1 si counter False defs
          opForNumIO = do
            expIns <- expInsIO
            return $
              expIns
                ++ [IMov (stackloc si) (Reg RAX)]
                ++ [IMov (Reg RAX) (stackloc si)]
          finalOp = case prim1 of
            Add1 -> do
              opForNum <- opForNumIO
              return $ opForNum ++ [IAdd (Reg RAX) (Const (1 * 2 + 1))] ++ [ISub (Reg RAX) (Const 1)]
            Sub1 -> do
              opForNum <- opForNumIO
              return $ opForNum ++ [ISub (Reg RAX) (Const (1 * 2 + 1))] ++ [IAdd (Reg RAX) (Const 1)]
            IsNum -> do
              expIns <- expInsIO
              noNumBranchLabel <- liftIO $ makeLabel "non_num" counter
              endCmpBranchLabel <- liftIO $ makeLabel "end_cmp" counter
              return $
                expIns
                  ++ [IAnd (Reg RAX) (Const 1)]
                  ++ [IOr (Reg RAX) (Const 1)]
                  ++ [ICmp (Reg RAX) (Const 1)]
                  ++ [IJne noNumBranchLabel]
                  ++ [IMov (Reg RAX) (Const constTrue)]
                  ++ [IJmp endCmpBranchLabel]
                  ++ [ILabel noNumBranchLabel]
                  ++ [IMov (Reg RAX) (Const constFalse)]
                  ++ [ILabel endCmpBranchLabel]
            IsBool -> do
              expIns <- expInsIO
              noBoolBranchLabel <- liftIO $ makeLabel "non_bool" counter
              endCmpBranchLabel <- liftIO $ makeLabel "end_cmp" counter
              return $
                expIns
                  ++ [IAnd (Reg RAX) (Const 1)]
                  ++ [ICmp (Reg RAX) (Const 1)]
                  ++ [IJne noBoolBranchLabel]
                  ++ [IMov (Reg RAX) (Const constTrue)]
                  ++ [IJmp endCmpBranchLabel]
                  ++ [ILabel noBoolBranchLabel]
                  ++ [IMov (Reg RAX) (Const constFalse)]
                  ++ [ILabel endCmpBranchLabel]
            Print -> do
              opForNum <- opForNumIO
              let stackAlignment = checkStackAligment si
              pure $
                opForNum
                  ++ [IMov (Reg RDI) (Reg RAX)]
                  ++ [ISub (Reg RSP) (Const (stackAlignment * 8))]
                  ++ [ICall "print"]
                  ++ [IAdd (Reg RSP) (Const (stackAlignment * 8))]
       in finalOp
    EWhile cond bodyExprs ->
      let condInsIO = exprToInstrs cond si counter False defs
          opForNumIO = do
            condIns <- condInsIO
            env <- get
            bodyIns <- liftIO $ compileLetBody bodyExprs si counter env False defs
            startBranchLabel <- liftIO $ makeLabel "start" counter
            endBranchLabel <- liftIO $ makeLabel "end" counter
            return $
              ILabel startBranchLabel :
              condIns
                ++ [ICmp (Reg RAX) (Const constFalse)]
                ++ [IJe endBranchLabel]
                ++ concat (reverse bodyIns)
                ++ [IJmp startBranchLabel]
                ++ [ILabel endBranchLabel]
       in opForNumIO
    ESet name e -> do
      s <- get
      exprIO <- exprToInstrs e si counter False defs
      let updateVariable = case lookup name s of
            Nothing -> []
            Just i ->
              IMov (Reg RAX) (stackloc i) :
              exprIO
                ++ [IMov (stackloc i) (Reg RAX)]
       in pure updateVariable
    ELet listBindings listExpr -> do
      ev <- get
      let shadowEnv =
            filter
              ( \(name, _) ->
                  case lookup name listBindings of
                    Just _ -> False
                    Nothing -> True
              )
              ev
      -- _ <- liftIO $ putStrLn $ show ev
      -- _ <- liftIO $ putStrLn $ show shadowEnv
      -- _ <- liftIO $ putStrLn $ show listBindings
      ((ins, si'), localEnv) <- liftIO $ runStateT (compileLetExpr listBindings si counter defs) shadowEnv
      -- _ <- liftIO $ putStrLn $ show ins
      -- _ <- liftIO $ putStrLn $ show localEnv
      -- _ <- liftIO $ putStrLn $ show listExpr
      b_is <- liftIO $ compileLetBody listExpr si' counter localEnv isTailPosition defs
      -- _ <- liftIO $ putStrLn $ show (reverse b_is)
      return $ ins ++ concat (reverse b_is)
    ENil typ -> pure [IMov (Reg RAX) (Const constNull)]
    ETuple exp1 (EId x) _ -> do
      s <- get
      e1is <- exprToInstrs exp1 si counter False defs
      let a = case lookup x s of
            Nothing -> []
            Just i ->
              [IAdd (Reg RCX) (Const 16)]
                ++ [IMov (Reg RAX) (stackloc i)]
                ++ [IMov (Reg R10) (Reg RAX)]
                ++ [IMov (Reg RAX) (Const 1)]
                ++ [IMov (RegOffset 0 RCX) (Reg RAX)]
                ++ e1is
                ++ [IMov (RegOffset 8 RCX) (Reg RAX)]
                ++ [IMov (RegOffset 16 RCX) (Reg R10)]
                ++ [IMov (Reg RAX) (Reg RCX)]
                ++ [IAdd (Reg RAX) (Const 1)] -- TAGGING
       in pure a
    ETuple exp1 exp2 _ ->
      let e1isIO = exprToInstrs exp1 (si + 3) counter False defs
          e2isIO = exprToInstrs exp2 (si + 4) counter False defs
          op = do
            e1is <- e1isIO
            e2is <- e2isIO
            return $
              [ IMov (Reg RAX) (Reg RCX),
                IAdd (Reg RCX) (Const 32),
                IAdd (Reg RAX) (Const 32),
                IMov (stackloc si) (Reg RAX),
                IMov (Reg RAX) (Const 1),
                IMov (RegOffset 0 RCX) (Reg RAX) -- type
              ]
                ++ e1is
                ++ [IMov (RegOffset 8 RCX) (Reg RAX)] -- val
                ++ e2is --pointer
                ++ [IMov (Reg R10) (Reg RAX)]
                ++ [IMov (Reg RAX) (stackloc si)]
                ++ [IMov (RegOffset 16 RAX) (Reg R10)] -- move pointer to next spot
                ++ [IMov (Reg RAX) (stackloc si)]
                ++ [IAdd (Reg RAX) (Const 1)] -- TAGGING and returning pointer
       in op
    EHead name -> do
      s <- get
      let a = case lookup name s of
            Nothing -> []
            Just e ->
              --TODO check that e is a pointer
              [IMov (Reg RAX) (stackloc e)]
                ++ [ISub (Reg RAX) (Const 1)] -- UNTAGGING
                ++ [IMov (Reg RAX) (RegOffset 8 RAX)]
       in pure a
    ETail name -> do
      s <- get
      let a = case lookup name s of
            Nothing -> []
            Just e ->
              --TODO check that e is a pointer
              [IMov (Reg RAX) (stackloc e)]
                ++ [ISub (Reg RAX) (Const 1)] -- UNTAGGING
                ++ [IMov (Reg RAX) (RegOffset 16 RAX)]
       in pure a
    EVector exprs ->
      let numElement = length exprs
          -- eNum = ENum numElement
          -- eNumInsIO = exprToInstrs eNum si counter False defs
          res = do
            -- e1is <- eNumInsIO
            env <- get
            bodyIns <- liftIO $ compileLetBody exprs si counter env False defs
            labelVecMake <- liftIO $ makeLabel ";make_vec" counter
            let vecElements = bodyIns
            return $
              [ ILabel labelVecMake,
                IAdd (Reg RCX) (Const 8),
                IMov (Reg RAX) (Reg RCX),
                IMov (Reg R10) (Reg RAX)
              ]
                ++ [IMov (Reg RAX) (Const 2)]
                ++ [IMov (RegOffset 0 RCX) (Reg RAX)]
                ++ [IMov (Reg RAX) (Const numElement)]
                ++ [IAdd (Reg RCX) (Const 8)]
                ++ [IMov (RegOffset 0 RCX) (Reg RAX)]
                ++ [IAdd (Reg RCX) (Const 8)]
                ++ concatMap (\ins -> ins ++ [IMov (RegOffset 0 RCX) (Reg RAX), IAdd (Reg RCX) (Const 8)]) vecElements
                ++ [IMov (Reg RAX) (Reg R10)]
                ++ [IAdd (Reg RAX) (Const 1)] -- TAGGING
       in res
    EGet vec item -> do
      s <- get
      overflowIndex <- liftIO $ makeLabel "overflow_index" counter
      endLabel <- liftIO $ makeLabel "end_get_vec" counter
      let a = case lookup vec s of
            Nothing -> []
            Just i ->
              [ IMov (Reg RAX) (stackloc i),
                ISub (Reg RAX) (Const 1), --UNTAGGING
                IMov (Reg R15) (Const item),
                ICmp (Reg R15) (Const 0),
                IJl overflowIndex,
                ICmp (Reg R15) (RegOffset 8 RAX),
                IJge overflowIndex,
                IMov (Reg RAX) (RegOffset (8 * (item + 2)) RAX),
                IJmp endLabel,
                ILabel overflowIndex,
                ICall "error_index_out_of_bounds",
                -- IMov (Reg RAX) (Const 0),
                ILabel endLabel
              ]
       in pure a
    EVecSet vec item exprSet -> do
      s <- get
      exprIO <- exprToInstrs exprSet si counter False defs
      overflowIndex <- liftIO $ makeLabel "overflow_index" counter
      endLabel <- liftIO $ makeLabel "end_get_vec" counter
      let a = case lookup vec s of
            Nothing -> []
            Just i ->
              [ IMov (Reg RAX) (stackloc i),
                ISub (Reg RAX) (Const 1), --UNTAGGING
                IMov (Reg R10) (Const item),
                ICmp (Reg R10) (Const 0),
                IJl overflowIndex,
                ICmp (Reg R10) (RegOffset 0 RAX),
                IJge overflowIndex
              ]
                ++ exprIO
                ++ [ IMov (Reg R10) (stackloc i),
                     ISub (Reg R10) (Const 1), --UNTAGGING
                     IMov (RegOffset (8 * (item + 1)) R10) (Reg RAX),
                     IJmp endLabel,
                     ILabel overflowIndex,
                     ICall "error_index_out_of_bounds",
                     IMov (Reg RAX) (Const 0),
                     ILabel endLabel
                   ]
       in pure a
    EApp nameDef listParams -> do
      currentVarEnv <- get
      afterCallLabel <- liftIO $ makeLabel "after_call" counter
      (paramsIns, _) <- liftIO $ compileEAppParams listParams (si + 2) counter defs currentVarEnv
      let headerIns =
            if isTailPosition
              then
                paramsIns
                  ++ [ILabel ";start tail call -> overwrite args"]
                  ++ reverse
                    ( foldl
                        ( \acc i ->
                            [IMov (stackloc (i + 1)) (Reg RAX), IMov (Reg RAX) (stackloc (si + 1 + i))] ++ acc
                        )
                        []
                        [1 .. (length listParams)]
                    )
                  ++ [IJmp nameDef]
              else
                IMov (Reg RBX) (Label afterCallLabel) :
                [IMov (stackloc si) (Reg RBX)]
                  ++ [IMov (stackloc (si + 1)) (Reg RSP)]
                  ++ paramsIns
                  ++ [ISub (Reg RSP) (Const (si * 8))]
                  ++ [IJmp nameDef]
                  ++ [ILabel afterCallLabel]
                  ++ [IMov (Reg RSP) (RegOffset (-16) RSP)]
      pure headerIns

checkStackAligment :: Int -> Int
checkStackAligment si =
  if si * 8 `mod` 16 == 0 then si else checkStackAligment (si + 1)

compileLetBody :: [Expr] -> StackIndex -> Counter -> TEnv -> Bool -> [Def] -> IO [[Instruction]]
compileLetBody exprs si counter localEnv isTailPosition defs =
  foldl
    ( \a b -> do
        -- _ <- putStrLn (show localEnv)
        c <- a
        d <- evalStateT (exprToInstrs b si counter isTailPosition defs) localEnv
        -- print b
        -- _ <- putStrLn " into "
        -- print d
        return $ d : c
    )
    (pure [])
    exprs

compileEAppParams :: [Expr] -> StackIndex -> Counter -> [Def] -> TEnv -> IO ([Instruction], StackIndex)
compileEAppParams list si counter defs currentVarEnv =
  foldM
    ( \(acc, si') expr ->
        let vInstIO = evalStateT (exprToInstrs expr si' counter False defs) currentVarEnv
            store = IMov (stackloc si') (Reg RAX)
         in do
              vIns <- vInstIO
              pure (acc ++ vIns ++ [store], si' + 1)
    )
    ([], si)
    list

compileLetExpr :: [(String, Expr)] -> StackIndex -> Counter -> [Def] -> Eval ([Instruction], StackIndex)
compileLetExpr list si counter defs =
  foldM
    ( \(acc, si') (x, value) -> do
        lEv <- get
        let vInstIO = runStateT (exprToInstrs value si' counter False defs) lEv
            store = IMov (stackloc si') (Reg RAX)
         in do
              (vIns, ev) <- liftIO vInstIO
              state (\s -> ((acc ++ vIns ++ [store], si' + 1), ev ++ insertVal (x, si') s))
    )
    ([], si)
    list

-- local variables enviroment depends on the number of args
compileDef :: Counter -> [Def] -> Def -> IO [Instruction]
compileDef counter defs (DFun name args _ body) =
  let (localSi, localVarEnv) =
        mapAccumL (\acc (argName, _) -> (acc + 1, (argName, acc))) 2 args -- (zip ([0..1]::[Int]) args)
      compiledBody = compileLetBody body localSi counter localVarEnv True defs
      compiledFunction = (\b -> [ILabel name] ++ concat (reverse b) ++ [IRet]) <$> compiledBody
   in compiledFunction

buildDefEnv :: [Def] -> DefTypEnv
buildDefEnv =
  foldl (\acc (DFun name args ret _) -> (name, (ret, args)) : acc) []

buildTypAliasEnv :: [TypAlias] -> TypAliasEnv
buildTypAliasEnv =
  foldl (\acc (TypAlias name typ) -> (name, typ) : acc) []

compile :: Prog -> IO String
compile prog = do
  counter <- makeCounter
  let prelude =
        " section .text\n\
        \extern error\n\
        \extern error_non_number\n\
        \extern error_index_out_of_bounds\n\
        \extern print\n\
        \global our_code_starts_here\n"
      result = do
        (ExprValidated validProg@(validDefs, validTyps, validMain)) <- check prog
        let defEnv = buildDefEnv validDefs
        let typAliasEnv = buildTypAliasEnv validTyps
        _ <- calcProgTyp validProg [("input", TNum)] defEnv typAliasEnv
        compiledDefs <- concat <$> mapM (compileDef counter validDefs) validDefs
        compiledMain <- evalStateT (exprToInstrs validMain 2 counter False validDefs) [("input", 1)]
        let kickoff =
              "our_code_starts_here:\n\
              \push rbx\n\
              \mov rcx, rsi\n\
              \mov [rsp - 8], rdi"
                ++ toAsm compiledMain
                ++ "\n pop rbx\nret\n"
        let postlude = []
        let asAssemblyString = toAsm (compiledDefs ++ postlude)
        return (kickoff, asAssemblyString)
   in do
        body <- result
        pure $ printf "%s%s\n%s\n" prelude (snd body) (fst body)
