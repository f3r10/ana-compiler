{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AnaCompiler.Compile (compile, AnaCompilerException (..), calcTyp, check) where

import AnaCompiler.Asm (Arg (Const, Reg, RegOffset), Instruction (..), Reg (RAX, RDI, RSP), toAsm)
import AnaCompiler.Expr
import AnaCompiler.Parser (Sexp, sexpToExpr)
import Control.Exception (Exception, throw)
import Control.Monad.State.Lazy
import Data.Bits (Bits (setBit, shiftL))
import Data.IORef
import Data.Validation
import Text.Printf (printf)

stackloc :: Int -> Arg
stackloc i = RegOffset (-8 * i) RSP

constTrue = 0x0000000000000002

constFalse = 0x0000000000000000

type VariableName = String

type VariablePosition = Int

data Typ
  = TNum
  | TBool
  deriving (Eq, Show)

type TEnv = [(VariableName, VariablePosition)]

type TypEnv = [(VariableName, Typ)]

type Eval a = StateT TEnv IO a

find :: String -> TEnv -> Maybe Int
find = lookup

type StackIndex = Int

insertVal :: (String, Int) -> TEnv -> TEnv
insertVal (x, si) env =
  case find x env of
    Nothing -> (x, si) : env
    _ -> env

newtype ExprValidated 
  = ExprValidated Expr
  deriving (Show)

newtype TypValidated
  = TypValidated Typ
  deriving (Show)

newtype Error = Error {errors :: [String]}
  deriving (Semigroup, Show)

checkTNumType :: Expr -> TypEnv -> IO TypValidated
checkTNumType expr typEnv = do
  TypValidated tpy <- calcTyp expr typEnv
  case tpy of
    TNum -> pure $ TypValidated TNum
    TBool -> throw $ AnaCompilerException ["Type mismatch: op must take a number as an argument"]

calcTyp :: Expr -> TypEnv -> IO TypValidated
calcTyp expr typEnv =
  case expr of
    ENum _ -> pure $ TypValidated TNum
    EBool _ -> pure $ TypValidated TBool
    EId x ->
      case lookup x typEnv of
        Nothing -> throw $ AnaCompilerException [printf "variable identifier %s unbound" x]
        Just typ -> pure $ TypValidated typ
    EPrim1 op e ->
      case op of
        Add1 -> checkTNumType e typEnv
        Sub1 -> checkTNumType e typEnv
        IsNum -> pure $ TypValidated TBool
        IsBool -> pure $ TypValidated TBool
    ESet _ e -> calcTyp e typEnv
    ELet bindList bodyList ->
      let localTypEnvIO =
            foldM
              ( \acc (valName, e) -> do
                  TypValidated typ <- calcTyp e acc
                  return $ (valName, typ) : acc
              )
              typEnv
              bindList
          bodyTypList = do
            localTypEnv <- localTypEnvIO
            head $ foldl (\acc e -> calcTyp e localTypEnv : acc) [] bodyList -- the last element of the exprs on the body
       in bodyTypList
    EIf condExpr thnExpr elsExpr -> do
      TypValidated tpy <- calcTyp condExpr typEnv
      case tpy of
        TNum -> throw $ AnaCompilerException ["Type mismatch: if expects a boolean in conditional position"]
        TBool -> do
          TypValidated thnExprType <- calcTyp thnExpr typEnv
          TypValidated elsExprType <- calcTyp elsExpr typEnv
          if thnExprType == elsExprType
            then pure $ TypValidated thnExprType
            else throw $ AnaCompilerException ["Type mismatch: if branches must agree on type"]
    EPrim2 op e1 e2 ->
      case op of
        Plus -> checkTNumType e1 typEnv *> checkTNumType e2 typEnv
        Minus -> checkTNumType e1 typEnv *> checkTNumType e2 typEnv
        Times -> checkTNumType e1 typEnv *> checkTNumType e2 typEnv
        Less -> checkTNumType e1 typEnv *> checkTNumType e2 typEnv *> pure (TypValidated TBool)
        Greater -> checkTNumType e1 typEnv *> checkTNumType e2 typEnv *> pure (TypValidated TBool)
        Equal -> do
          TypValidated e1Type <- calcTyp e1 typEnv
          TypValidated e2Type <- calcTyp e2 typEnv
          if e1Type == e2Type
            then pure $ TypValidated TBool
            else throw $ AnaCompilerException ["Type mismatch: equal sides must agree on type"]

wellFormedELetBody :: [Expr] -> TEnv -> Validation Error ()
wellFormedELetBody exprs localEnv =
  let localErrs =
        foldl
          ( \a b ->
              let vIns = wellFormedE b localEnv
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

wellFormedELetExpr :: [(String, Expr)] -> StackIndex -> Error -> TEnv -> (TEnv, Error, StackIndex)
wellFormedELetExpr list si accError env =
  foldl
    ( \(accEnv, accErr, si') (x, value) ->
        let vIns = wellFormedE value accEnv
            (b, c, d) = case find x accEnv of
              Nothing -> ((x, si') : accEnv, accErr, si' + 1)
              _ -> (accEnv, Error [printf "Multiple bindings for variable identifier %s" x] <> accErr, si')
            fE = case vIns of
              Failure errs -> c <> errs
              Success _ -> c
         in (b, fE, d)
    )
    (env, accError, si)
    list

wellFormedE :: Expr -> TEnv -> Validation Error ()
wellFormedE expr env =
  case expr of
    ENum _ -> Success ()
    EBool _ -> Success ()
    EId x ->
      case find x env of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" x]
        Just _ -> Success ()
    EPrim2 _ e1 e2 ->
      let c1 = wellFormedE e1 env
          c2 = wellFormedE e2 env
       in c1 *> c2
    EPrim1 _ e1 -> wellFormedE e1 env
    ESet name e -> 
      case find name env of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" name]
        Just _ -> wellFormedE e env
    ELet list body ->
      let (localEnv, localErrs, _) = wellFormedELetExpr list 0 (Error []) env
          bodyC = wellFormedELetBody body localEnv
          c2 = case bodyC of
            Success _ ->
              if null (errors localErrs)
                then Success ()
                else Failure localErrs
            Failure errs -> Failure $ localErrs <> errs
       in c2
    EIf exp1 exp2 exp3 ->
      wellFormedE exp1 env *> wellFormedE exp2 env *> wellFormedE exp3 env

newtype AnaCompilerException
  = AnaCompilerException [String]
  deriving (Eq)

instance Show AnaCompilerException where
  show (AnaCompilerException errs) = unwords . words $ unlines errs

instance Exception AnaCompilerException

check :: Expr -> TEnv -> IO ExprValidated
check expr env =
  case wellFormedE expr env of
    Success _ -> pure $ ExprValidated expr
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

exprToInstrs :: Expr -> StackIndex -> Counter -> Eval [Instruction]
exprToInstrs expr si counter =
  case expr of
    EId x -> do
      s <- get
      let a = case find x s of
            Nothing -> []
            Just i -> [IMov (Reg RAX) (stackloc i)]
       in pure a
    ENum n -> pure [IMov (Reg RAX) (Const (n * 2 + 1 {- (Const (setBit (shiftL n 1) 0)) -}))]
    EBool f ->
      if f
        then pure [IMov (Reg RAX) (Const constTrue)]
        else pure [IMov (Reg RAX) (Const constFalse)]
    EPrim2 prim exp1 exp2 ->
      let exp1InsIO = exprToInstrs exp1 si counter --env
          exp2InsIO = exprToInstrs exp2 (si + 1) counter -- env
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
                return $ op ++ IAdd (Reg RAX) (stackloc $ si + 1) : [ISub (Reg RAX) (Const 1)]
              Minus -> do
                op <- opForNumIO
                return $ op ++ ISub (Reg RAX) (stackloc $ si + 1) : [IAdd (Reg RAX) (Const 1)]
              Times -> do
                op <- opForNumIO
                return $
                  op
                    ++ [IXor (Reg RAX) (Const 1)]
                    ++ [ISar (Reg RAX) (Const 1)]
                    ++ [IMov (stackloc si) (Reg RAX)]
                    ++ [IMul (Reg RAX) (stackloc $ si + 1)]
                    ++ [ISub (Reg RAX) (stackloc si)]
                    ++ [IXor (Reg RAX) (Const 1)]
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
      let e1isIO = exprToInstrs e1 si counter --env
          e2isIO = exprToInstrs e2 (si + 1) counter --env -- TODO  this env keeps record of let variables. Is valid to repeat let variables inside blocks?
          e3isIO = exprToInstrs e3 (si + 2) counter --env
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
      let expInsIO = exprToInstrs exp1 si counter --env
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
       in finalOp
    ESet name e -> do
      s <- get
      exprIO <- exprToInstrs e si counter
      let 
        updateVariable = case find name s of
            Nothing -> []
            Just i -> 
              IMov (Reg RAX) (stackloc i)
              : exprIO
              ++ [IMov (stackloc i) (Reg RAX)]
        in pure updateVariable
    ELet listBindings listExpr -> do
      -- ev <- get
      -- _ <- liftIO $ putStrLn $ show ev
      ((ins, si'), localEnv) <- liftIO $ runStateT (compileLetExpr listBindings si counter) []
      -- _ <- liftIO $ putStrLn $ show ins
      -- _ <- liftIO $ putStrLn $ show localEnv
      -- _ <- liftIO $ putStrLn $ show listExpr
      b_is <- liftIO $ compileLetBody listExpr si' counter localEnv
      -- _ <- liftIO $ putStrLn $ show (reverse b_is)
      return $ ins ++ concat (reverse b_is)

compileLetBody :: [Expr] -> StackIndex -> Counter -> TEnv -> IO [[Instruction]]
compileLetBody exprs si counter localEnv =
  foldl
    ( \a b -> do
        c <- a
        d <- evalStateT (exprToInstrs b si counter) localEnv
        return $ d : c
    )
    (pure [])
    exprs

compileLetExpr :: [(String, Expr)] -> StackIndex -> Counter -> Eval ([Instruction], StackIndex)
compileLetExpr list si counter =
  foldM
    ( \(acc, si') (x, value) ->
        let vInstIO = runStateT (exprToInstrs value si' counter) []
            store = IMov (stackloc si') (Reg RAX)
         in do
              (vIns, ev) <- liftIO vInstIO
              state (\s -> ((acc ++ vIns ++ [store], si' + 1), ev ++ insertVal (x, si') s))
    )
    ([], si)
    list

compile :: Sexp -> IO String
compile sexEp = do
  counter <- makeCounter
  let header =
        "section .text\n\
        \extern error\n\
        \extern error_non_number\n\
        \global our_code_starts_here\n\
        \our_code_starts_here:\n\
        \mov [rsp - 8], rdi"
      expr = sexpToExpr sexEp
      bodyIO = do
        TypValidated typ <- calcTyp expr [("input", TNum)]
        (ExprValidated validatedExpr) <- check expr [("input", 1)]
        compiled <- evalStateT (exprToInstrs validatedExpr 2 counter) [("input", 1)]
        return $ toAsm $ compiled ++ [IRet] 
   in do
        body <- bodyIO
        pure $ body `seq` header ++ body
