module AnaCompiler.Compile (compile) where


import AnaCompiler.Expr
import AnaCompiler.Parser (Sexp, sexpToExpr)
import Text.Printf (printf)


stackloc :: Int -> Int
stackloc i = i * 8

stackval :: Int -> String
stackval i = "[rsp - " ++ show (stackloc i) ++ "]"

type TEnv = [(String, Int)]

find :: TEnv -> String -> Maybe Int
find env x =
  case env of
    [] -> Nothing
    (y, i) : rest ->
      if y == x
        then Just i
        else find rest x

-- si -> stack index
exprToInstrs :: Expr -> Int -> TEnv -> [String]
exprToInstrs expr si env =
  case expr of
    EId x ->
      case find env x of
        Nothing -> error "Unbound id"
        Just i -> ["mov rax, [rsp - " ++ show (stackloc i) ++ "]"]
    ENum n -> ["mov rax, " ++ show n]
    EPlus e1 e2 -> 
      let
        e1is = exprToInstrs e1 si env
        e2is = exprToInstrs e2 (si + 1) env
        in
        e1is ++ ["mov " ++ stackval si ++ ", rax"] 
        ++ e2is ++ [ "mov " ++ stackval (si + 1) ++ ", rax"] 
        ++ ["mov rax, " ++ stackval si
           , "add rax, " ++ stackval (si + 1)]
    ELet x value body ->
      let v_is = exprToInstrs value si env
          new_env = (x, si) : env
          store = "mov [rsp - " ++ show (stackloc si) ++ "], rax"
          b_is = exprToInstrs body (si + 1) new_env
       in v_is ++ [store] ++ b_is

compile :: Sexp -> String
compile sexEp =
  let header = "section .text\nglobal our_code_starts_here\nour_code_starts_here:\n"
      expr = sexpToExpr sexEp
      body = concatMap (printf " %s\n") $ exprToInstrs expr 1 []
      footer = " ret\n"
   in body `seq` header ++ body ++ footer
