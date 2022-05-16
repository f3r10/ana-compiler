{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AnaCompiler.Compile (compile, AnaCompilerException (..), calcTyp, check, TypValidated(..), buildDefEnv, calcProgTyp) where

import AnaCompiler.Asm (Arg (Const, Reg, RegOffset, Label, Size), Instruction (..), Reg (RAX, RDI, RSP, RBX, RCX), toAsm, Size (DWORD_PTR, WORD_PTR))
import AnaCompiler.Expr
import AnaCompiler.Parser (Sexp, sexpToExpr)
import Control.Exception (Exception, throw)
import Control.Monad.State.Lazy
import Data.Bits (Bits (setBit, shiftL, shift))
import Data.IORef
import Data.Validation
import Text.Printf (printf)
import Data.List

stackloc :: Int -> Arg
stackloc i = RegOffset (-8 * i) RSP

constTrue = 0xFFFFFFFF

constFalse = 0x7FFFFFFF

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

calcDefTyp :: DefTypEnv -> Def -> IO TypValidated
calcDefTyp defEnv (DFun _ args ret body)  = 
  let
    lastBodyExpr = last body
    lastBodyExprTyp = calcTyp lastBodyExpr args defEnv >>= ( \(TypValidated lastExpr) ->
                    if lastExpr == ret
                      then pure $ TypValidated ret
                      else throw $ AnaCompilerException ["Type mismatch: body end is not the same as return typ"]
                )
   in lastBodyExprTyp


calcProgTyp :: Prog -> TypEnv -> DefTypEnv -> IO TypValidated
calcProgTyp (defs, main) typEnv defEnv =
  mapM_ (calcDefTyp defEnv) defs *>
  calcTyp main typEnv defEnv

checkTNumType :: Expr -> TypEnv -> DefTypEnv -> IO TypValidated
checkTNumType expr typEnv defTypEnv = do
  TypValidated tpy <- calcTyp expr typEnv defTypEnv
  case tpy of
    TNum -> pure $ TypValidated TNum
    _ -> throw $ AnaCompilerException ["Type mismatch: op must take a number as an argument"]

calcTyp :: Expr -> TypEnv -> DefTypEnv -> IO TypValidated
calcTyp  expr typEnv defTypEnv =
  case expr of
    ENum _ -> pure $ TypValidated TNum
    EBool _ -> pure $ TypValidated TBool
    EId x ->
      case lookup x typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" x]
        Just typ -> pure $ TypValidated typ
    EPrim1 op e ->
      case op of
        Add1 -> checkTNumType e typEnv defTypEnv
        Sub1 -> checkTNumType e typEnv defTypEnv
        IsNum -> pure $ TypValidated TBool
        IsBool -> pure $ TypValidated TBool
        Print -> calcTyp e typEnv defTypEnv
    EWhile cond _ -> do
      TypValidated tpy <- calcTyp cond typEnv defTypEnv
      case tpy of
        TBool -> pure $ TypValidated TBool --head $ foldl (\acc e -> calcTyp e typEnv : acc) [] body
        _ -> throw $ AnaCompilerException ["Type mismatch: while condition must take a bool as an argument"]
    ESet _ e -> calcTyp e typEnv defTypEnv 
    ELet bindList bodyList ->
      let localTypEnvIO =
            foldM
              ( \acc (valName, e) -> do
                  TypValidated typ <- calcTyp e acc defTypEnv
                  return $ (valName, typ) : acc
              )
              typEnv
              bindList
          bodyTypList = do
            localTypEnv <- localTypEnvIO
            head $ foldl (\acc e -> calcTyp e localTypEnv defTypEnv : acc) [] bodyList 
            -- the last element of the exprs on the body
            -- for inner functions it would necessary to generate another def typ env???
       in bodyTypList
    EIf condExpr thnExpr elsExpr -> do
      TypValidated tpy <- calcTyp condExpr typEnv defTypEnv
      case tpy of
        TBool -> do
          TypValidated thnExprType <- calcTyp thnExpr typEnv defTypEnv
          TypValidated elsExprType <- calcTyp elsExpr typEnv defTypEnv
          if thnExprType == elsExprType
            then pure $ TypValidated thnExprType
            else throw $ AnaCompilerException ["Type mismatch: if branches must agree on type"]
        _ -> throw $ AnaCompilerException ["Type mismatch: if expects a boolean in conditional position"]
    EPrim2 op e1 e2 ->
      case op of
        Plus -> checkTNumType e1 typEnv defTypEnv *> checkTNumType e2 typEnv defTypEnv
        Minus -> checkTNumType e1 typEnv defTypEnv *> checkTNumType e2 typEnv defTypEnv
        Times -> checkTNumType e1 typEnv defTypEnv *> checkTNumType e2 typEnv defTypEnv
        Less -> checkTNumType e1 typEnv defTypEnv *> checkTNumType e2 typEnv defTypEnv *> pure (TypValidated TBool)
        Greater -> checkTNumType e1 typEnv defTypEnv *> checkTNumType e2 typEnv defTypEnv *> pure (TypValidated TBool)
        Equal -> do
          TypValidated e1Type <- calcTyp e1 typEnv defTypEnv
          TypValidated e2Type <- calcTyp e2 typEnv defTypEnv
          if e1Type == e2Type
            then pure $ TypValidated TBool
            else throw $ AnaCompilerException ["Type mismatch: equal sides must agree on type"]
    EPair exp1 exp2 -> do
          TypValidated headPair <- calcTyp exp1 typEnv defTypEnv
          TypValidated tailPair  <- calcTyp exp2 typEnv defTypEnv
          if headPair == tailPair
            then pure $ TypValidated (TPair tailPair)
            else throw $ AnaCompilerException ["Type mismatch: pair elements must agree on type"]
    EFst name ->
      case lookup name typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" name]
        Just (TPair typ) -> pure $ TypValidated typ
        Just _ -> pure $ throw $ AnaCompilerException [printf "Type mismatch: unknown pair %s type" name]
    ESnd name ->
      case lookup name typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" name]
        Just (TPair typ) -> pure $ TypValidated typ
        Just _ -> pure $ throw $ AnaCompilerException [printf "Type mismatch: unknown pair %s type" name]
    EApp name listParams -> 
      case lookup name defTypEnv of
        Just (typ, args) ->
          let 
            paramsCheck = sequence(reverse (foldl (\acc e -> calcTyp e typEnv defTypEnv : acc) [] listParams)) 
              >>= (\paramsTyps -> 
                -- error $ show $ null (paramsTyps \\ (map (TypValidated . snd) args))
                if null (paramsTyps \\ map (TypValidated . snd) args)
                   then pure ()
                   else throw $ AnaCompilerException ["Type mismatch:: params type is not the same as argument type"]
                  )
           in  paramsCheck *> pure (TypValidated typ)
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
      let shadowEnv = filter (\(name, _) -> 
            case lookup name list of
              Just _ -> False
              Nothing -> True
            ) env
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
    EPair exp1 exp2 -> wellFormedE defs exp1 env *> wellFormedE defs exp2 env
    EFst name ->
      case lookup name env of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" name]
        Just _ -> Success ()
    ESnd name ->
      case lookup name env of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" name]
        Just _ -> Success ()
    EApp nameDef listParams ->
      case findDef defs nameDef of
        Just (DFun _ args _ _) -> 
          let
            appErrors = 
              if length listParams == length args
                 then wellFormedExprListBody defs listParams env
                 else
                  case wellFormedExprListBody defs listParams env of
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
wellFormedProg (defs, main) = 
  let defsValidations = 
        foldl (\acc def -> 
          case wellFormedDef defs def of
              Failure errs -> acc <> errs
              Success _ -> acc
          ) (Error []) defs 
      mainValidation = wellFormedE defs main [("input", 1)] 
      finalValidation = case mainValidation of
                          Failure errs ->
                            if null (errors defsValidations)
                              then Failure errs 
                              else Failure $ defsValidations <> errs
                          Success _ -> 
                            if null (errors defsValidations)
                              then Success ()
                              else Failure  defsValidations
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
    ENum n -> pure [IMov (Reg RAX) (Const (shift n 1) )]
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
                ++ [ICmp (Reg RAX) (Const 0)]
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
                  ++ [IJe noBoolBranchLabel]
                  ++ [IMov (Reg RAX) (Const constTrue)]
                  ++ [IJmp endCmpBranchLabel]
                  ++ [ILabel noBoolBranchLabel]
                  ++ [IMov (Reg RAX) (Const constFalse)]
                  ++ [ILabel endCmpBranchLabel]
            Print -> do
              opForNum <- opForNumIO
              let stackAlignment = checkStackAligment si
              pure $ 
                opForNum ++ 
                [IMov (Reg RDI) (Reg RAX)] ++
                [ISub (Reg RSP) (Const (stackAlignment * 8)) ] ++
                [ICall "print"] ++
                [IAdd (Reg RSP) (Const (stackAlignment * 8))]
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
      let shadowEnv = filter (\(name, _) -> 
            case lookup name listBindings of
              Just _ -> False
              Nothing -> True
            ) ev
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
    EPair exp1 exp2 -> 
      let e1isIO = exprToInstrs exp1 si counter False defs
          e2isIO = exprToInstrs exp2 (si + 1) counter False defs 
          op = do
            e1is <- e1isIO
            e2is <- e2isIO
            return $
              e1is
                ++ [IMov (RegOffset 0 RCX) (Reg RAX)]
                ++ e2is
                ++ [IMov (RegOffset 8 RCX) (Reg RAX)]
                ++ [IMov (Reg RAX) (Reg RCX)]
                ++ [IAdd (Reg RAX) (Const 1)] -- TAGGING
                ++ [IAdd (Reg RCX) (Const 16)]
       in op
    EFst name -> do
      s <- get
      let a = case lookup name s of
            Nothing -> []
            Just e ->
              --TODO check that e is a pointer 
              [IMov (Reg RAX) (stackloc e)]
                ++ [ISub (Reg RAX) (Const 1)] -- UNTAGGING
                ++ [IMov (Reg RAX) (RegOffset 0 RAX)]
       in pure a
    ESnd name -> do
      s <- get
      let a = case lookup name s of
            Nothing -> []
            Just e ->
              --TODO check that e is a pointer 
              [IMov (Reg RAX) (stackloc e)]
                ++ [ISub (Reg RAX) (Const 1)] -- UNTAGGING
                ++ [IMov (Reg RAX) (RegOffset 8 RAX)]
       in pure a
    EApp nameDef listParams -> do
      currentVarEnv <- get
      afterCallLabel <- liftIO $ makeLabel "after_call" counter
      (paramsIns, _) <- liftIO $ compileEAppParams listParams (si+2) counter defs currentVarEnv 
      let headerIns = 
            if isTailPosition then
              paramsIns ++
              [ILabel ";start tail call -> overwrite args"] ++
              reverse (foldl (\acc i -> 
                [IMov (stackloc (i+1)) (Reg RAX), IMov (Reg RAX) (stackloc (si+1+i))] ++ acc ) [] [1.. (length listParams)]) ++
              [IJmp nameDef]
            else
            IMov (Reg RBX) (Label afterCallLabel) :
            [IMov (stackloc si) (Reg RBX)] ++
            [IMov (stackloc (si+1)) (Reg RSP)] ++ 
            paramsIns ++ 
            [ISub (Reg RSP) (Const (si * 8)) ] ++
            [IJmp nameDef] ++ 
            [ILabel afterCallLabel] ++ 
            [IMov (Reg RSP) (RegOffset (-16) RSP)]
      pure headerIns

checkStackAligment :: Int -> Int
checkStackAligment si = 
  if (si * 8 `mod` 16 == 0) then si else checkStackAligment (si + 1)

compileLetBody :: [Expr] -> StackIndex -> Counter -> TEnv -> Bool -> [Def] -> IO [[Instruction]]
compileLetBody exprs si counter localEnv isTailPosition defs =
  foldl
    ( \a b -> do
        -- _ <- putStrLn (show localEnv)
        c <- a
        d <- evalStateT (exprToInstrs b si counter isTailPosition defs) localEnv
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
compileDef :: Counter -> [Def] -> Def  -> IO [Instruction]
compileDef counter defs (DFun name args _ body) = 
  let
    (localSi, localVarEnv) 
      = mapAccumL (\acc (argName, _) -> (acc+1, (argName, acc) )) 2 args -- (zip ([0..1]::[Int]) args)
    compiledBody = compileLetBody body localSi counter localVarEnv True defs  
    compiledFunction = (\b -> [ILabel name] ++ concat (reverse b) ++ [IRet] ) <$> compiledBody 
   in compiledFunction


buildDefEnv :: [Def] -> DefTypEnv
buildDefEnv = 
  foldl (\acc (DFun name args ret _) -> (name, (ret, args)): acc) []

compile :: Prog -> IO String
compile prog = do
  counter <- makeCounter
  let prelude =
        " section .text\n\
          \extern error\n\
          \extern error_non_number\n\
          \extern print\n\
          \global our_code_starts_here\n"
      result = do
        (ExprValidated validProg@(validDefs, validMain)) <- check prog
        let defEnv = buildDefEnv validDefs
        _ <- calcProgTyp validProg [("input", TNum)] defEnv
        compiledDefs <- concat <$> mapM (compileDef counter validDefs) validDefs
        compiledMain <- evalStateT (exprToInstrs validMain 2 counter False validDefs) [("input", 1)]
        let kickoff = 
              "our_code_starts_here:\n\
              \push rbx\n\
               \mov rcx, rdi" ++
              toAsm compiledMain ++ "\n pop rbx\nret\n"
        let postlude = []
        let asAssemblyString = toAsm (compiledDefs ++ postlude) 
        return (kickoff, asAssemblyString)
   in do
        body <- result
        pure $ printf "%s%s\n%s\n" prelude (snd body) (fst body)
