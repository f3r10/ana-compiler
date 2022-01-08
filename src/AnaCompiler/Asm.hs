module AnaCompiler.Asm (Reg(..)) where


data Reg = RAX {- the register where we place answers -}

data Arg =
  Const Int
    | Reg Reg
    | RegOffset Int Reg

data Instruction =
  IMov Arg Arg
