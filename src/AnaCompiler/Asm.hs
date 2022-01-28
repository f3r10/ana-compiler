module AnaCompiler.Asm (Reg (..), Instruction (..), Arg (..), toAsm) where
import Text.Printf (printf)

data Reg
  = RAX {- the register where we place answers -}
  | RSP
  | RDI

data Arg
  = Const Int
  | Reg Reg
  | RegOffset Int Reg

data Instruction
  = IMov Arg Arg

  | IAdd Arg Arg
  | ISub Arg Arg
  | IMul Arg Arg

  | IShr Arg Arg -- logical right shit
  | ISar Arg Arg -- arithmetic right shift
  | IShl Arg Arg -- left shift

  | IAnd Arg Arg
  | IOr Arg Arg
  | IXor Arg Arg

  | ILabel String
  | IPush Arg
  | IPop Arg
  | ICall String
  | IRet

  | ICmp Arg Arg
  | IJne String -- if cmp equal do nothing else execute label
  | IJe String
  | IJmp String
  | IJno String
  | IJl String
  | IJg String
  | IJo String


rToAsm :: Reg -> String
rToAsm r =
  case r of
    RAX -> "rax"
    RSP -> "rsp"
    RDI -> "rdi"

argToAsm :: Arg -> String
argToAsm arg =
  case arg of
    Const n -> show n
    Reg r -> rToAsm r
    RegOffset n reg -> printf "[%s %s]" (rToAsm reg) (show n)

iToAsm :: Instruction -> String 
iToAsm ins = 
  case ins of
    IMov dest value -> printf " mov %s, %s" (argToAsm dest) (argToAsm value)
    IAdd dest toAdd -> printf " add %s, %s" (argToAsm dest) (argToAsm toAdd) 
    ISub dest toAdd -> printf " sub %s, %s" (argToAsm dest) (argToAsm toAdd) 
    IMul dest toAdd -> printf " imul %s, %s" (argToAsm dest) (argToAsm toAdd) 
    IAnd dest mask -> printf " and %s, %s" (argToAsm dest) (argToAsm mask)
    IOr dest mask -> printf " or %s, %s" (argToAsm dest) (argToAsm mask)
    IXor dest mask -> printf " xor %s, %s" (argToAsm dest) (argToAsm mask)
    IShr dest toShift -> printf " shr %s, %s" (argToAsm dest) (argToAsm toShift)
    ISar dest toShift -> printf " sar %s, %s" (argToAsm dest) (argToAsm toShift)
    IShl dest toShift -> printf " shl %s, %s" (argToAsm dest) (argToAsm toShift)
    IRet -> "       ret\n"
    ILabel name -> name ++ ":"
    IPush arg -> printf " push %s" (argToAsm arg)
    IPop arg -> undefined
    ICall str -> printf " call %s" str
    ICmp left right -> printf " cmp %s, %s" (argToAsm left) (argToAsm right)
    IJne label -> printf " jne near %s" label 
    IJe label -> printf " je near %s" label 
    IJmp label -> printf " jmp near %s" label 
    IJno label -> printf " jno near %s" label 
    IJl label -> printf " jl near %s" label 
    IJg label -> printf " jg near %s" label
    IJo label -> printf " jo near %s" label


toAsm :: [Instruction] -> String
toAsm = foldl (\s i -> printf "%s\n%s" s (iToAsm i)) ""
