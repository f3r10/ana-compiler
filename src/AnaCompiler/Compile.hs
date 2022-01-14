module AnaCompiler.Compile (compile) where

import AnaCompiler.Asm (Arg (Const, Reg, RegOffset), Instruction (..), Reg (RAX, RSP), toAsm)
import AnaCompiler.Expr
import AnaCompiler.Parser (Sexp, sexpToExpr)

stackloc :: Int -> Arg
stackloc i = RegOffset (-8 * i) RSP

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

exprToInstrs :: Expr -> StackIndex -> TEnv -> [Instruction]
exprToInstrs expr si env =
  case expr of
    EId x ->
      case find env x of
        Nothing -> error $ "Compile error: Unbound variable identifier " ++ x
        Just i -> [IMov (Reg RAX) (stackloc i)]
    ENum n -> [IMov (Reg RAX) (Const n)]
    EPrim2 prim e1 e2 ->
      let e1is = exprToInstrs e1 si env
          e2is = exprToInstrs e2 (si + 1) env
          op = e1is ++ [IMov (stackloc si) (Reg RAX)] ++ e2is ++ [IMov (stackloc $ si + 1) (Reg RAX)] ++ [IMov (Reg RAX) (stackloc si)]
          final_op =
            case prim of
              Plus -> [IAdd (Reg RAX) (stackloc $ si + 1)]
              Minus -> [ISub (Reg RAX) (stackloc $ si + 1)]
       in op ++ final_op
    EPrim1 prim1 e1 -> 
      let e1is = exprToInstrs e1 si env
       in case prim1 of
            Add1 -> e1is ++ [IAdd (Reg RAX) (Const 1)]
            Sub1 -> e1is ++ [ISub (Reg RAX) (Const 1)]
    ELet list body -> 
      let
        (ins, si', localEnv) = compileLetExpr list [] si []
        b_is = exprToInstrs body (si' + 1) localEnv
      in ins ++ b_is
      -- let 
      --   lets = foldl (\acc (x, value) -> 
      --     let
      --       v_is = exprToInstrs value si env
      --       store = IMov (stackloc si) (Reg RAX) 
      --
      --     ) [] list
      -- in undefined

compileLetExpr :: [(String, Expr)] -> [Instruction] -> StackIndex -> TEnv -> ([Instruction], StackIndex, TEnv)
compileLetExpr list accInstruction si env =
  case list of
    [] -> (accInstruction, si, env)
    [(x, value)] ->
      let
        v_is = exprToInstrs value si env
        new_env = insertVal (x, si)  env
        store = IMov (stackloc si) (Reg RAX)
      in (accInstruction ++ v_is ++ [store], si + 1, new_env)
    (x, value): rest -> 
      let 
        v_is = exprToInstrs value si env
        new_env = insertVal (x, si) env
        store = IMov (stackloc si) (Reg RAX)
      in compileLetExpr rest ( accInstruction ++ (v_is ++ [store] )) (si + 1) new_env

compile :: Sexp -> String
compile sexEp =
  let header = "section .text\nglobal our_code_starts_here\nour_code_starts_here:\n"
      expr = sexpToExpr sexEp
      compiled = exprToInstrs expr 1 []
      body = toAsm $ compiled ++ [IRet]
   in body `seq` header ++ body
