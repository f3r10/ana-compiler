{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module AnaCompiler.Compile (compile) where

import AnaCompiler.Asm (Arg (Const, Reg, RegOffset), Instruction (..), Reg (RAX, RDI, RSP), toAsm)
import AnaCompiler.Expr
import AnaCompiler.Parser (Sexp, sexpToExpr)
import Data.Bits (Bits (setBit, shiftL))
import Data.IORef
import Text.Printf (printf)
import Data.Validation
import Control.Monad.State.Lazy

stackloc :: Int -> Arg
stackloc i = RegOffset (-8 * i) RSP

constTrue = 0x0000000000000002

constFalse = 0x0000000000000000

type VariableName = String
type VariablePosition = Int

type TEnv = [(VariableName, VariablePosition)]

type Eval a = StateT TEnv IO a

find :: String -> TEnv ->  Maybe Int
find = lookup
{- do
  db <- get
  return $ lookup x db -}
  -- case db of
  --   [] -> Nothing
  --   (y, i) : rest ->
  --     if y == x
  --       then Just i
  --       else find rest x

type StackIndex = Int

insertVal :: (String, Int) -> TEnv -> TEnv
insertVal (x, si) env =
  case find x env of
    Nothing -> (x, si) : env
    _ -> error "Compile error: Duplicate binding"


-- data ExprValidated (a :: Expr) where
--   MkExprValidated :: ExprValidated a

newtype ExprValidated = ExprValidated Expr

newtype Error = Error [String]
  deriving (Semigroup, Show)

{- wellFormedELetExpr :: [(String, Expr)] -> Error -> TEnv -> Validation Error TEnv
wellFormedELetExpr list accError env =
  case list of
    [] -> 
      case accError of
        Error [] -> Success env
        errs -> Failure errs
    [(x, value)] ->
      let v_is = wellFormedE value env
          new_env = case find x of
            Nothing -> Success $ (x, 1) : env
            _ -> Failure $ Error [printf "Multiple bindings for variable identifier %s" x]
          valResult = v_is *> new_env
          finalError = case valResult of
            Failure errors -> Failure $ errors <> accError
            Success _ -> new_env
       in finalError
    (x, value) : rest ->
      let v_is = wellFormedE value env
          new_env = case find env x of
            Nothing -> Success $ (x, 1) : env -- TODO is necessary to update the env?
            _ -> Failure $ Error [printf "Multiple bindings for variable identifier %s" x]
          finalError = case v_is *> new_env of
                         Success _ -> accError
                         Failure errors -> accError <> errors
       in wellFormedELetExpr rest finalError env

wellFormedE :: Expr -> TEnv -> Validation Error ()
wellFormedE expr env =
  case expr of
    ENum _ -> Success ()
    EBool _ -> Success ()
    EId x ->
      case find env x of
        Nothing -> Failure $ Error [printf "variable identifier %s unbound" x]
        Just _ -> Success ()
    EPrim2 _ e1 e2 ->
      let c1 = wellFormedE e1 env
          c2 = wellFormedE e2 env
       in c1 *> c2
    EPrim1 _ e1 -> wellFormedE e1 env
    ELet list body ->
      let c1 = wellFormedELetExpr list (Error []) env
          c2 = wellFormedE body env
       in c2
    EIf exp1 exp2 exp3 -> 
      wellFormedE exp1 env *> wellFormedE exp2 env *> wellFormedE exp3 env

check :: Expr -> ExprValidated
check expr =
  case wellFormedE expr [("input", -1)] of
    Success _ -> ExprValidated expr
    Failure errs -> error $ show errs -- TODO concat but with an newline -}

checkIfIsNumberOnRuntime :: [Instruction]
checkIfIsNumberOnRuntime =
  [IAnd (Reg RAX) (Const 1)]
    ++ [ICmp (Reg RAX) (Const 1)]
    ++ [IJne "internal_error_non_number"]

internalErrorNonNumber :: [Instruction]
internalErrorNonNumber =
  [ ILabel "internal_error_non_number",
    IMov (Reg RDI) (Reg RAX),
    IPush (Const 0),
    ICall "error_non_number"
  ]

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
      let 
       a = case find x s of
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
                ++ checkIfIsNumberOnRuntime
                ++ exp2Ins
                ++ [IMov (stackloc $ si + 1) (Reg RAX)]
                ++ checkIfIsNumberOnRuntime
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
                endCmpBranchLabel <- liftIO $  makeLabel "end_cmp" counter
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
                ++ checkIfIsNumberOnRuntime
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
              IsBool ->  do
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
    ELet list body -> do
      -- ev <- get
      -- _ <- liftIO $ putStrLn $ show ev
      ( (ins, si' ), localEnv ) <- liftIO $ runStateT (compileLetExpr list si counter) []
      -- _ <- liftIO $ putStrLn $ show localEnv
      b_is <- liftIO $ evalStateT (exprToInstrs body (si' + 1) counter) localEnv
      return $ ins ++ b_is

compileLetExpr :: [(String, Expr)] -> StackIndex -> Counter -> Eval ([Instruction], StackIndex)
compileLetExpr list si counter =
  foldM (\(acc, si') (x, value) ->
    let
      vInstIO = runStateT (exprToInstrs value si' counter) []
      store = IMov (stackloc si') (Reg RAX)
      in do
        (vIns, ev) <- liftIO vInstIO
        state (\s -> ((acc ++ vIns ++ [store], si' + 1), ev ++ insertVal (x, si') s))
        ) ([], si) list

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
      -- ExprValidated validatedExpr = check expr
      compiledIO = runStateT (exprToInstrs expr 2 counter) [("input", 1)]
      bodyIO = do
        (compiled, _ ) <- compiledIO
        return $ toAsm $ compiled ++ [IRet] ++ internalErrorNonNumber
   in do
        body <- bodyIO
        pure $ body `seq` header ++ body
