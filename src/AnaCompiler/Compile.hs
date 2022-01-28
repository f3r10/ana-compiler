module AnaCompiler.Compile (compile) where

import AnaCompiler.Asm (Arg (Const, Reg, RegOffset), Instruction (..), Reg (RAX, RDI, RSP), toAsm)
import AnaCompiler.Expr
import AnaCompiler.Parser (Sexp, sexpToExpr)
import Data.Bits (Bits (setBit, shiftL))
import Data.IORef
import Text.Printf (printf)

stackloc :: Int -> Arg
stackloc i = RegOffset (-8 * i) RSP

constTrue = 0x0000000000000002

constFalse = 0x0000000000000000

type TEnv = [(String, Int)]

find :: TEnv -> String -> Maybe Int
find env x =
  case env of
    [] -> Nothing
    (y, i) : rest ->
      if y == x
        then Just i
        else find rest x

type StackIndex = Int

insertVal :: (String, Int) -> TEnv -> TEnv
insertVal (x, si) env =
  case find env x of
    Nothing -> (x, si) : env
    _ -> error "Compile error: Duplicate binding"

{- wellFormedELetExpr :: [(String, Expr)] -> [String] -> TEnv -> [String]
wellFormedELetExpr list accError env =
  case list of
    [] -> accError
    [(x, value)] ->
      let v_is = wellFormedE value env
          new_env = case find env x of
            Nothing -> [""]
            _ -> [printf "Multiple bindings for variable identifier %s" x]
       in accError ++ v_is ++ new_env
    (x, value) : rest ->
      let v_is = wellFormedE value env
          new_env = case find env x of
            Nothing -> [""] -- TODO is necessary to update the env?
            _ -> [printf "Multiple bindings for variable identifier %s" x]
       in wellFormedELetExpr rest (accError ++ (v_is ++ new_env)) env

wellFormedE :: Expr -> TEnv -> [String]
wellFormedE expr env =
  case expr of
    ENum _ -> [""]
    EBool _ -> [""]
    EId x ->
      case find env x of
        Nothing -> [printf "variable identifier %s unbound" x]
        Just _ -> []
    EPrim2 _ e1 e2 ->
      let c1 = check e1
          c2 = check e2
       in c1 ++ c2
    EPrim1 _ e1 -> check e1
    ELet list body ->
      let c1 = wellFormedELetExpr list [] env
          c2 = check body
       in c1 ++ c2

check :: Expr -> [String]
check expr =
  case wellFormedE expr [("input", -1)] of
    [] -> []
    errs -> error $ concat errs -- TODO concat but with an newline -}

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

exprToInstrs :: Expr -> StackIndex -> Counter -> TEnv -> IO [Instruction]
exprToInstrs expr si counter env =
  case expr of
    EId x ->
      case find env x of
        Nothing -> error $ "Compile error: Unbound variable identifier " ++ x
        Just i -> pure [IMov (Reg RAX) (stackloc i)]
    ENum n -> pure [IMov (Reg RAX) (Const (n * 2 + 1 {- (Const (setBit (shiftL n 1) 0)) -}))]
    EBool f ->
      if f
        then pure [IMov (Reg RAX) (Const constTrue)]
        else pure [IMov (Reg RAX) (Const constFalse)]
    EPrim2 prim exp1 exp2 ->
      let exp1InsIO = exprToInstrs exp1 si counter env
          exp2InsIO = exprToInstrs exp2 (si + 1) counter env
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
                noEqualBranchLabel <- makeLabel "no_eq" counter
                endCmpBranchLabel <- makeLabel "end_cmp" counter
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
                lessBranchLabel <- makeLabel "less_than" counter
                endCmpBranchLabel <- makeLabel "end_cmp" counter
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
                greaterBranchLabel <- makeLabel "greater_than" counter
                endCmpBranchLabel <- makeLabel "end_cmp" counter
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
      let e1isIO = exprToInstrs e1 si counter env
          e2isIO = exprToInstrs e2 (si + 1) counter env -- TODO  this env keeps record of let variables. Is valid to repeat let variables inside blocks?
          e3isIO = exprToInstrs e3 (si + 2) counter env
          op = do
            e1is <- e1isIO
            e2is <- e2isIO
            e3is <- e3isIO
            elseBranchLabel <- makeLabel "else_branch" counter
            endIfLabel <- makeLabel "end_of_if" counter
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
      let expInsIO = exprToInstrs exp1 si counter env
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
                noNumBranchLabel <- makeLabel "non_num" counter
                endCmpBranchLabel <- makeLabel "end_cmp" counter
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
                noBoolBranchLabel <- makeLabel "non_bool" counter
                endCmpBranchLabel <- makeLabel "end_cmp" counter
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
      compileLetExprIO <- compileLetExpr list [] si [] counter
      let (ins, si', localEnv) = compileLetExprIO
          b_isIO = exprToInstrs body (si' + 1) counter localEnv
       in do
            b_is <- b_isIO
            return $ ins ++ b_is

compileLetExpr :: [(String, Expr)] -> [Instruction] -> StackIndex -> TEnv -> Counter -> IO ([Instruction], StackIndex, TEnv)
compileLetExpr list accInstruction si env counter =
  case list of
    [] -> pure (accInstruction, si, env)
    [(x, value)] ->
      let v_isIO = exprToInstrs value si counter env
          new_env = insertVal (x, si) env
          store = IMov (stackloc si) (Reg RAX)
       in do
            v_is <- v_isIO
            return (accInstruction ++ v_is ++ [store], si + 1, new_env)
    (x, value) : rest ->
      let v_isIO = exprToInstrs value si counter env
          new_env = insertVal (x, si) env
          store = IMov (stackloc si) (Reg RAX)
       in do
            v_is <- v_isIO
            compileLetExpr rest (accInstruction ++ (v_is ++ [store])) (si + 1) new_env counter

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
      compiledIO = exprToInstrs expr 2 counter [("input", 1)]
      bodyIO = do
        compiled <- compiledIO
        return $ toAsm $ compiled ++ [IRet] ++ internalErrorNonNumber
   in do
        body <- bodyIO
        return $ body `seq` header ++ body
