{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AnaCompiler.Compile (compile, AnaCompilerException (..), calcTyp, check, TypValidated (..), buildDefEnv, calcProgTyp, buildTypAliasEnv) where

import AnaCompiler.Asm (Arg (Const, Label, Reg, RegOffset), Instruction (..), Reg (R10, R15, RAX, RBX, RCX, RDI, RSI, RSP), toAsm)
import AnaCompiler.Expr
import AnaCompiler.Utils (lookUp)
import Control.Exception (Exception, throw)
import Control.Monad.State.Lazy
import Data.Bits (Bits (shift))
import Data.Functor
import Data.IORef
import Data.List
import qualified Data.Maybe
import Data.Validation
import Text.Printf (printf)

stackloc :: Int -> Arg
stackloc i = RegOffset (-8 * i) RSP

constTrue :: Int
constTrue = 0xFFFFFFFF

constFalse :: Int
constFalse = 0x7FFFFFFF

constNull :: Int
constNull = 0x00000000

type Scope = [(String, TypEnv)]

type CompilerEnv = (TEnv, Scope, String, TypAliasEnv)

type Eval a = StateT CompilerEnv IO a

type TypState a = StateT (String, TypEnv) IO a

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

calcDefTyp :: DefTypEnv -> TypAliasEnv -> Def -> IO (TypValidated, (String, TypEnv))
calcDefTyp defEnv typAlias (DFun scope args ret body) =
  let lastBodyExpr = last body
      lastBodyExprTyp =
        runStateT (calcTyp lastBodyExpr defEnv typAlias) (scope, args)
          >>= ( \(TypValidated lastExpr, resolvedScope) ->
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
                        then pure $ (TypValidated ret, resolvedScope)
                        else throw $ AnaCompilerException ["Type mismatch: body end is not the same as return typ " ++ show resolveLastTypeAlias ++ " " ++ show resolveRetTypeAlias]
              )
   in lastBodyExprTyp

calcProgTyp :: Prog -> TypEnv -> DefTypEnv -> TypAliasEnv -> IO (TypValidated, [(String, TypEnv)])
calcProgTyp (defs, _, main) typEnv defEnv typAliasEnv = do
  sTypEnvDefs <- mapM (calcDefTyp defEnv typAliasEnv) defs
  let scopeDefs = fmap snd sTypEnvDefs
  (returnTypBody, scopeBody) <- runStateT (calcTyp main defEnv typAliasEnv) ("main", typEnv)
  return (returnTypBody, scopeBody : scopeDefs)

checkTNumType :: Expr -> (String, TypEnv) -> DefTypEnv -> TypAliasEnv -> IO TypValidated
checkTNumType expr typEnv defTypEnv typAlias = do
  TypValidated tpy <- evalStateT (calcTyp expr defTypEnv typAlias) typEnv
  case tpy of
    TNum -> pure $ TypValidated TNum
    _ -> throw $ AnaCompilerException ["Type mismatch: op must take a number as an argument"]

calcTyp :: Expr -> DefTypEnv -> TypAliasEnv -> TypState TypValidated
calcTyp expr defTypEnv typAlias =
  case expr of
    ENum _ -> pure $ TypValidated TNum
    EBool _ -> pure $ TypValidated TBool
    EId x -> do
      (_, typEnv) <- get
      case lookup x typEnv of
        Nothing -> throw $ AnaCompilerException [printf "Type checking error: variable identifier %s unbound" x]
        Just typ -> pure $ TypValidated typ
    EPrim1 op e -> do
      typEnv <- get
      case op of
        Add1 -> liftIO $ checkTNumType e typEnv defTypEnv typAlias
        Sub1 -> liftIO $ checkTNumType e typEnv defTypEnv typAlias
        IsNum -> pure $ TypValidated TBool
        IsBool -> pure $ TypValidated TBool
        Print -> calcTyp e defTypEnv typAlias
    EWhile cond _ -> do
      typEnv <- get
      TypValidated tpy <- liftIO $ evalStateT (calcTyp cond defTypEnv typAlias) typEnv
      case tpy of
        TBool -> pure $ TypValidated TBool --head $ foldl (\acc e -> calcTyp e typEnv : acc) [] body
        _ -> throw $ AnaCompilerException ["Type mismatch: while condition must take a bool as an argument"]
    ESet _ e -> calcTyp e defTypEnv typAlias
    ELet bindList bodyList -> do
      (scope, typEnv) <- get
      let localTypEnvIO =
            foldM
              ( \acc (valName, e) -> do
                  -- it is necessary to use the acc becuase bindings may use previous references
                  (TypValidated typ) <- evalStateT (calcTyp e defTypEnv typAlias) (scope, acc)
                  return $ (valName, typ) : acc
              )
              typEnv
              bindList
          bodyTypList = do
            localTypEnv <- localTypEnvIO
            foldM
              ( \acc e -> do
                  res <- evalStateT (calcTyp e defTypEnv typAlias) (scope, localTypEnv)
                  return (res : acc)
              )
              []
              bodyList
       in do
            a <- liftIO bodyTypList
            localTypEnv <- liftIO localTypEnvIO
            _ <- state (\(innerScope, _) -> (scope, (innerScope, localTypEnv {- ++ t -}))) -- TODO why t is not necessary anymore
            -- b <- localTypEnvIO
            return (last $ reverse a)
    EIf condExpr thnExpr elsExpr -> do
      typEnv <- get
      (TypValidated tpy) <- liftIO $ evalStateT (calcTyp condExpr defTypEnv typAlias) typEnv
      case tpy of
        TBool -> do
          (TypValidated thnExprType) <- liftIO $ evalStateT (calcTyp thnExpr defTypEnv typAlias) typEnv
          (TypValidated elsExprType) <- liftIO $ evalStateT (calcTyp elsExpr defTypEnv typAlias) typEnv
          if thnExprType == elsExprType
            then pure $ TypValidated thnExprType
            else throw $ AnaCompilerException ["Type mismatch: if branches must agree on type"]
        _ -> throw $ AnaCompilerException ["Type mismatch: if expects a boolean in conditional position"]
    EPrim2 op e1 e2 -> do
      typEnv <- get
      case op of
        Plus -> liftIO $ checkTNumType e1 typEnv defTypEnv typAlias *> checkTNumType e2 typEnv defTypEnv typAlias
        Minus -> liftIO $ checkTNumType e1 typEnv defTypEnv typAlias *> checkTNumType e2 typEnv defTypEnv typAlias
        Times -> liftIO $ checkTNumType e1 typEnv defTypEnv typAlias *> checkTNumType e2 typEnv defTypEnv typAlias
        Less ->
          liftIO $
            (checkTNumType e1 typEnv defTypEnv typAlias *> checkTNumType e2 typEnv defTypEnv typAlias)
              $> TypValidated TBool
        Greater ->
          liftIO $
            (checkTNumType e1 typEnv defTypEnv typAlias *> checkTNumType e2 typEnv defTypEnv typAlias)
              $> TypValidated TBool
        Equal -> do
          typEnvLocal <- get
          (TypValidated e1Type) <- liftIO $ evalStateT (calcTyp e1 defTypEnv typAlias) typEnvLocal
          (TypValidated e2Type) <- liftIO $ evalStateT (calcTyp e2 defTypEnv typAlias) typEnvLocal
          if e1Type == e2Type
            then pure $ TypValidated TBool
            else throw $ AnaCompilerException ["Type mismatch: equal sides must agree on type"]
    ENil typ -> pure $ TypValidated typ
    ETuple _ _ finalType -> do
      pure $ TypValidated finalType
    EHead name -> do
      (_, typEnv) <- get
      case lookup name typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" name]
        Just (TTuple typ) -> pure $ TypValidated typ
        Just (TName customType) ->
          case lookup customType typAlias of
            Just (TTuple finalType) -> pure $ TypValidated finalType
            Just finalType -> pure $ TypValidated finalType
            Nothing -> pure $ throw $ AnaCompilerException [printf "Type mismatch: unknown pair %s type" (show customType)]
        Just typ -> pure $ TypValidated typ
    ETail name -> do
      (_, typEnv) <- get
      case lookup name typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" name]
        Just tupleTail@(TTuple _) -> pure $ TypValidated tupleTail
        Just finalTyp@(TName _) -> pure $ TypValidated finalTyp
        Just typ -> pure $ TypValidated typ
    EVector exprs -> do
      typEnv <- get
      let exprsTypList =
            foldl
              ( \acc e ->
                  let t = evalStateT (calcTyp e defTypEnv typAlias) typEnv
                   in t : acc
              )
              []
              exprs
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
       in liftIO vecType
    EDictGet dict value -> do
      (_, typEnv) <- get
      case lookup dict typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" dict]
        Just (TDict typs) ->
          case lookup value typs of
            Just typ -> pure $ TypValidated typ
            Nothing -> throw $ AnaCompilerException [printf "dict %s unbound key %s" dict value]
        Just (TName typ) ->
          case lookup typ typAlias of
            Just (TDict typs) ->
              case lookup value typs of
                Just currentType -> pure $ TypValidated currentType
                Nothing -> throw $ AnaCompilerException [printf "dict %s unbound key %s" dict value]
            Just invalid -> throw $ AnaCompilerException ["Type mismatch: new dict element has to be the same as current dict typ"]
            Nothing -> undefined
        Just typ -> pure $ TypValidated typ
    EGet vec _ -> do
      (_, typEnv) <- get
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
    EVecSet vec _ exprVecSet -> do
      sTypEnv@(_, typEnv) <- get
      case lookup vec typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" vec]
        Just vecTyp -> do
          TypValidated newTyp <- liftIO $ evalStateT (calcTyp exprVecSet defTypEnv typAlias) sTypEnv
          if vecTyp == newTyp
            then pure $ TypValidated vecTyp
            else throw $ AnaCompilerException ["Type mismatch: new vec element has to be the same as vec typ"]
    EDictSet dict value exprDictSet -> do
      sTypEnv@(_, typEnv) <- get
      case lookup dict typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" dict]
        Just (TDict typs) ->
          case lookup value typs of
            Just currentType -> do
              TypValidated newTyp <- liftIO $ evalStateT (calcTyp exprDictSet defTypEnv typAlias) sTypEnv
              if currentType == newTyp
                then pure $ TypValidated currentType
                else throw $ AnaCompilerException ["Type mismatch: new dict element has to be the same as current dict typ"]
            Nothing -> throw $ AnaCompilerException [printf "dict %s unbound key %s" dict value]
        Just (TName typ) ->
          case lookup typ typAlias of
            Just (TDict typs) ->
              case lookup value typs of
                Just currentType -> do
                  TypValidated newTyp <- liftIO $ evalStateT (calcTyp exprDictSet defTypEnv typAlias) sTypEnv
                  if currentType == newTyp
                    then pure $ TypValidated currentType
                    else throw $ AnaCompilerException ["Type mismatch: new dict element has to be the same as current dict typ"]
                Nothing -> throw $ AnaCompilerException [printf "dict %s unbound key %s" dict value]
            Just invalid -> throw $ AnaCompilerException ["Type mismatch: new dict element has to be the same as current dict typ"]
            Nothing -> undefined
        Just invalidType -> throw $ AnaCompilerException ["Type mismatch: new dict element has to be the same as current dict typ"]
    EDict listExprs -> do
      typScope <- get
      let startLocalEnv = []
          binds =
            foldM
              ( \acc (valName, e) -> do
                  (TypValidated typ) <- evalStateT (calcTyp e defTypEnv typAlias) typScope
                  return $ (valName, typ) : acc
              )
              startLocalEnv
              listExprs
       in liftIO $ fmap (TypValidated . TDict) binds
    EApp name listParams -> do
      sTypEnv <- get
      case lookup name defTypEnv of
        Just (typ, args) ->
          let paramsCheck =
                sequence (reverse (foldl (\acc e -> evalStateT (calcTyp e defTypEnv typAlias) sTypEnv : acc) [] listParams))
                  >>= ( \paramsTyps ->
                          -- error $ show $ null (paramsTyps \\ map (TypValidated . snd) resolveTypeAlias)
                          if null (paramsTyps \\ map (TypValidated . snd) args)
                            then pure ()
                            else
                              throw $
                                AnaCompilerException
                                  ["Type mismatch:: params type is not the same as argument type " ++ show paramsTyps ++ " " ++ show args]
                      )
           in liftIO $ paramsCheck $> TypValidated typ
        Nothing -> throw $ AnaCompilerException ["Type mismatch: def is not defined: " ++ name]

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
    ENil _ -> Success ()
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
    EDictGet dict _ ->
      case lookup dict env of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" dict]
        Just _ -> Success ()
    EVecSet vec _ exprSet ->
      case lookup vec env of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" vec]
        Just _ -> wellFormedE defs exprSet env
    EDictSet dict _ exprDictSet ->
      case lookup dict env of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" dict]
        Just _ -> wellFormedE defs exprDictSet env
    EDict listExprs -> wellFormedExprListBody defs (fmap snd listExprs) env
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
wellFormedProg (defs, _, main) =
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
      (s, _, _, _) <- get
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
                  ++ [IMov (Reg RAX) (Const 1)]
                  ++ [IMov (Reg RSI) (Reg RAX)]
                  ++ [ISub (Reg RSP) (Const (stackAlignment * 8))]
                  ++ [ICall "print"]
                  ++ [IAdd (Reg RSP) (Const (stackAlignment * 8))]
                  ++ [IMov (Reg RAX) (Const (0))]
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
      (s, _, _, _) <- get
      exprIO <- exprToInstrs e si counter False defs
      let updateVariable = case lookup name s of
            Nothing -> []
            Just i ->
              IMov (Reg RAX) (stackloc i) :
              exprIO
                ++ [IMov (stackloc i) (Reg RAX)]
       in pure updateVariable
    ELet listBindings listExpr -> do
      (ev, typScope, scopeName, typAliasEnv) <- get
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
      ((ins, si'), (localEnv, localScope, _, _)) <- liftIO $ runStateT (compileLetExpr listBindings si counter defs) (shadowEnv, typScope, scopeName, typAliasEnv)
      -- _ <- liftIO $ putStrLn $ show ins
      -- _ <- liftIO $ putStrLn $ show localEnv
      -- _ <- liftIO $ putStrLn $ show listExpr
      b_is <- liftIO $ compileLetBody listExpr si' counter (localEnv, localScope, scopeName, typAliasEnv) isTailPosition defs
      -- _ <- liftIO $ putStrLn $ show (reverse b_is)
      return $ ins ++ concat (reverse b_is)
    ENil _ -> pure [IMov (Reg RAX) (Const constNull)]
    ETuple exp1 (EId x) _ -> do
      (s, _, _, _) <- get
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
      (s, _, _, _) <- get
      let a = case lookup name s of
            Nothing -> []
            Just e ->
              --TODO check that e is a pointer
              [IMov (Reg RAX) (stackloc e)]
                ++ [ISub (Reg RAX) (Const 1)] -- UNTAGGING
                ++ [IMov (Reg RAX) (RegOffset 8 RAX)]
       in pure a
    ETail name -> do
      (s, _, _, _) <- get
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
          res = do
            env <- get
            bodyIns <- liftIO $ compileLetBody exprs si counter env False defs
            labelVecMake <- liftIO $ makeLabel ";make_vec" counter
            labelVecMakeEnd <- liftIO $ makeLabel ";end_make_vec" counter
            let vecElements = bodyIns
            return $
              [ ILabel labelVecMake,
                IAdd (Reg RCX) (Const 8),
                IMov (Reg R10) (Reg RCX),
                IAdd (Reg RCX) (Const (8 * (numElement + 1))) -- save space for the next items
              ]
                ++ [IMov (Reg RAX) (Const 2)]
                ++ [IMov (RegOffset 0 R10) (Reg RAX)]
                ++ [IMov (Reg RAX) (Const numElement)]
                ++ [IMov (RegOffset 8 R10) (Reg RAX)]
                ++ concatMap
                  ( \(index, ins) ->
                      ins ++ [IMov (RegOffset (8 * index) R10) (Reg RAX)]
                  )
                  (zip [2 ..] vecElements)
                ++ [IMov (Reg RAX) (Reg R10)]
                ++ [IAdd (Reg RAX) (Const 1)] -- TAGGING
                ++ [ILabel labelVecMakeEnd]
       in res
    EDictGet dictRef key -> do
      (s, typScope, scopeName, _) <- get
      case lookup scopeName typScope of
        Just mainScope ->
          case lookup dictRef mainScope of
            Just (TDict elms) ->
              case lookUp (fmap fst elms) key of
                Just heapPos ->
                  return $ case lookup dictRef s of
                    Nothing -> []
                    Just i ->
                      [ ILabel ";get dict element",
                        IMov (Reg RAX) (stackloc i),
                        ISub (Reg RAX) (Const 1), --UNTAGGING
                        IMov (Reg RAX) (RegOffset (8 * (heapPos + 2)) RAX)
                      ]
                Nothing -> return []
            Just _ -> return []
            Nothing -> return []
        Nothing -> return []
    EGet vec item -> do
      (s, _, _, _) <- get
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
      (s, _, _, _) <- get
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
                     IMov (RegOffset (8 * (item + 2)) R10) (Reg RAX),
                     IJmp endLabel,
                     ILabel overflowIndex,
                     ICall "error_index_out_of_bounds",
                     IMov (Reg RAX) (Const 0),
                     ILabel endLabel
                   ]
       in pure a
    EDictSet dictRef key exprDictSet -> do
      (s, typScope, scopeName, typAliasEnv) <- get
      -- _ <- liftIO $ print typScope
      -- _ <- liftIO $ print scopeName
      -- _ <- liftIO $ print dictRef
      -- _ <- liftIO $ print typAliasEnv
      exprIO <- exprToInstrs exprDictSet si counter False defs
      case lookup scopeName typScope of
        Just mainScope ->
          case lookup dictRef mainScope of
            Just (TDict elms) -> do
              case lookUp (fmap fst elms) key of
                Just heapPos -> do
                  -- _ <- liftIO $ print heapPos
                  return $ case lookup dictRef s of
                    Nothing -> []
                    Just i ->
                      exprIO
                        ++ [ IMov (Reg R10) (stackloc i),
                             ISub (Reg R10) (Const 1), --UNTAGGING
                             IMov (RegOffset (8 * (heapPos + 2)) R10) (Reg RAX),
                             IMov (Reg RAX) (stackloc i)
                           ]
                Nothing -> return []
            Just (TName typ) -> do
              -- _ <- liftIO $ print ("here " ++ show typ)
              case lookup typ typAliasEnv of
                Just (TDict elms) ->
                  case lookUp (fmap fst elms) key of
                    Just heapPos -> do
                      -- _ <- liftIO $ print heapPos
                      return $ case lookup dictRef s of
                        Nothing -> []
                        Just i ->
                          exprIO
                            ++ [ IMov (Reg R10) (stackloc i),
                                 ISub (Reg R10) (Const 1), --UNTAGGING
                                 IMov (RegOffset (8 * (heapPos + 2)) R10) (Reg RAX),
                                 IMov (Reg RAX) (stackloc i)
                               ]
                    Nothing -> return []
                Just _ -> return []
                Nothing -> return []
            Just _ -> return []
            Nothing -> return []
        Nothing -> return []
    EDict listExprs -> do
      env <- get
      let numElements = length listExprs
      dictElements <- liftIO $ compileLetBody (fmap snd listExprs) si counter env False defs
      -- typsExps <- liftIO $ mapM (\e -> fmap fst (runStateT (calcTyp e [] []) ("main", []))) (fmap snd listExprs)
      labelDictMake <- liftIO $ makeLabel ";make_dict" counter
      return $
        [ ILabel labelDictMake,
          IAdd (Reg RCX) (Const 8), -- bumping for using an empty space
          IMov (Reg R15) (Reg RCX), -- save the starting place
          IAdd (Reg RCX) (Const (8 * (numElements + 1))) -- save space for the next items
        ]
          ++ [IMov (Reg RAX) (Const 3)]
          ++ [IMov (RegOffset 0 R15) (Reg RAX)]
          ++ [IMov (Reg RAX) (Const numElements)]
          ++ [IMov (RegOffset 8 R15) (Reg RAX)]
          ++ concatMap
            ( \(index, ins) ->
                ins
                  ++ [IMov (RegOffset (8 * index) R15) (Reg RAX)]
            )
            (zip [2 ..] dictElements)
          ++ [IMov (Reg RAX) (Reg R15)]
          ++ [IAdd (Reg RAX) (Const 1)] -- TAGGING
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

compileLetBody :: [Expr] -> StackIndex -> Counter -> CompilerEnv -> Bool -> [Def] -> IO [[Instruction]]
compileLetBody exprs si counter env isTailPosition defs =
  foldl
    ( \a b -> do
        -- _ <- putStrLn (show localEnv)
        c <- a
        d <- evalStateT (exprToInstrs b si counter isTailPosition defs) env
        -- print b
        -- _ <- putStrLn " into "
        -- print d
        return $ d : c
    )
    (pure [])
    exprs

compileEAppParams :: [Expr] -> StackIndex -> Counter -> [Def] -> CompilerEnv -> IO ([Instruction], StackIndex)
compileEAppParams list si counter defs env =
  foldM
    ( \(acc, si') expr ->
        let vInstIO = evalStateT (exprToInstrs expr si' counter False defs) env
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
              (vIns, (ev, scope1, _, _)) <- liftIO vInstIO
              state (\(s, scope2, name, typAliasEnv) -> ((acc ++ vIns ++ [store], si' + 1), (ev ++ insertVal (x, si') s, scope1 ++ scope2, name, typAliasEnv)))
    )
    ([], si)
    list

-- local variables enviroment depends on the number of args
compileDef :: Counter -> [Def] -> Def -> IO [Instruction]
compileDef counter defs (DFun name args _ body) =
  let (localSi, localVarEnv) =
        mapAccumL (\acc (argName, _) -> (acc + 1, (argName, acc))) 2 args -- (zip ([0..1]::[Int]) args)
      compiledBody = compileLetBody body localSi counter (localVarEnv, [], name, []) True defs
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
        (_, scopeProg) <- calcProgTyp validProg [("input", TNum)] defEnv typAliasEnv
        compiledDefs <- concat <$> mapM (compileDef counter validDefs) validDefs
        compiledMain <- evalStateT (exprToInstrs validMain 2 counter False validDefs) ([("input", 1)], scopeProg, "main", typAliasEnv)
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
