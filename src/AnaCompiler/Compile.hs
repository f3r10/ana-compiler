module AnaCompiler.Compile (compile) where

import AnaCompiler.Asm (Arg (Const, Reg, RegOffset), Instruction (..), Reg (RAX, RSP), toAsm)
import AnaCompiler.Expr
import AnaCompiler.Parser (Sexp, sexpToExpr)
import Data.Bits (Bits(setBit, shiftL))
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

wellFormedELetExpr :: [(String, Expr)] -> [String] -> TEnv -> [String]
wellFormedELetExpr list accError env =
  case list of
    [] -> accError
    [(x, value)] ->
      let
        v_is = wellFormedE value env
        new_env = case find env x of
                    Nothing -> [""]
                    _ -> [ printf "Multiple bindings for variable identifier %s" x]

      in accError ++ v_is ++ new_env
    (x, value): rest -> 
      let 
        v_is = wellFormedE value env
        new_env = case find env x of
                    Nothing -> [""] -- TODO is necessary to update the env?
                    _ -> [printf "Multiple bindings for variable identifier %s" x]
      in wellFormedELetExpr rest ( accError ++ (v_is ++ new_env )) env

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
      let
       c1 = check e1
       c2 = check e2
       in c1 ++ c2
    EPrim1 _ e1 -> check e1
    ELet list body -> 
      let
       c1 = wellFormedELetExpr list [] env
       c2 = check body
       in c1 ++ c2 


check :: Expr -> [String]
check expr = 
  case wellFormedE expr [("input", -1)] of
    [] -> []
    errs -> error $ concat errs -- TODO concat but with an newline

exprToInstrs :: Expr -> StackIndex -> TEnv -> [Instruction]
exprToInstrs expr si env =
  case check expr of
    [] ->
      case expr of
        EId x ->
          case find env x of
            Nothing -> error $ "Compile error: Unbound variable identifier " ++ x
            Just i -> [IMov (Reg RAX) (stackloc i)]
        ENum n -> [IMov (Reg RAX) (Const (setBit (shiftL n 1) 0))]
        EBool f -> 
          if f
             then [IMov (Reg RAX) (Const constTrue)]
             else [IMov (Reg RAX) (Const constFalse)]
        EPrim2 prim e1 e2 ->
          let e1is = exprToInstrs e1 si env
              e2is = exprToInstrs e2 (si + 1) env
              op = e1is ++ [IMov (stackloc si) (Reg RAX)] ++ e2is ++ [IMov (stackloc $ si + 1) (Reg RAX)] ++ [IMov (Reg RAX) (stackloc si)]
              final_op =
                case prim of
                  Plus -> [IAdd (Reg RAX) (stackloc $ si + 1)]
                  Minus -> [ISub (Reg RAX) (stackloc $ si + 1)]
                  Times -> [IMul (Reg RAX) (stackloc $ si + 1)]
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
              -- in undefined
    _ -> error "error" 


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
  let header = "section .text\nextern error\nglobal our_code_starts_here\nour_code_starts_here:\n mov [rsp - 8], rdi"
      expr = sexpToExpr sexEp
      compiled = exprToInstrs expr 2 [("input", 1)]
      body = toAsm $ compiled ++ [IRet]
   in body `seq` header ++ body
