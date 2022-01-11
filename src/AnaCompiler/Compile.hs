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

exprToInstrs :: Expr -> StackIndex -> TEnv -> [Instruction]
exprToInstrs expr si env =
  case expr of
    EId x ->
      case find env x of
        Nothing -> error "Unbound id"
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
    ELet x value body ->
      let v_is = exprToInstrs value si env
          new_env = (x, si) : env
          store = IMov (stackloc si) (Reg RAX)
          b_is = exprToInstrs body (si + 1) new_env
       in v_is ++ [store] ++ b_is

compile :: Sexp -> String
compile sexEp =
  let header = "section .text\nglobal our_code_starts_here\nour_code_starts_here:\n"
      expr = sexpToExpr sexEp
      compiled = exprToInstrs expr 1 []
      body = toAsm $ compiled ++ [IRet]
   in body `seq` header ++ body
