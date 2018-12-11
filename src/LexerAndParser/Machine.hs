module Machine where

import TAC(TAC)
import qualified TAC

import Data.Set(Set)
import qualified Data.Set as Set

import Data.Map(Map)
import qualified Data.Map as Map


import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

type MIPSInstruction = String
type MIPSCode = [MIPSInstruction]

type BlockId = Int

data Operand = Register Int | Immediate Int | Label String | Parent Operand deriving (Eq, Ord)
instance Show Operand where
  show (Register r) = show (regNumberToMips r)
  show (Immediate i) = show i
  show (Label l) = l
  show (Parent op) = "(" ++ show op ++ ")"

regNumberToMips i
  | 0 <= i && i < 10 = "$t" ++ show i
  | otherwise = "$s" ++ show (i-10)

type Variable = (String, Int)
data Location = Mem | Reg Int

data MachineMState = MachineMState { regHas :: Map Operand (Set Variable)
                                   , varIsIn :: Map Variable (Set Location)
                                   , newSPOffset :: Int
                                   , argStack :: [TAC.X]

                                   , varOffset :: Map Variable Int
                                   , aliveAtEndOfBlock :: Set Variable
                                   , nRegisters :: Int
                                   }
initMachineMState :: Int -> Map Variable Int -> Set Variable -> MachineMState
initMachineMState nR varOff aliveAtEnd = MachineMState { regHas = emptyRegHas nR
                                                       , varIsIn = Map.empty
                                                       , newSPOffset = 0
                                                       , argStack = []

                                                       , varOffset = varOff
                                                       , aliveAtEndOfBlock = aliveAtEnd
                                                       , nRegisters = nR
                                                       }
emptyRegHas :: Int -> Map Operand (Set Variable)
emptyRegHas n = foldl (\m i -> Map.insert (Register i) Set.empty m) Map.empty [0..(n-1)]

type MachineM a = WriterT MIPSCode (StateT MachineMState IO) a

runMachineM :: MachineM a -> Int -> Map Variable Int -> Set Variable
                          -> IO MIPSCode
runMachineM f nR varOff aliveAtEnd = evalStateT (execWriterT f) $ initMachineMState nR varOff aliveAtEnd

tac2mips :: TAC -> MachineM ()
tac2mips tac = mapM_ tacInstruction2mipsInstruction tac

-- tacInstruction2mipsInstruction  {{{1
tacInstruction2mipsInstruction :: TAC.Instruction -> MachineM ()
-- BinOpInstr {{{2
tacInstruction2mipsInstruction instr@(TAC.BinOpInstr x y op z) = do
    Registers3 rx ry rz <- getReg instr
    checkVarInReg x rx -- TODO Fix
    checkVarInReg y ry
    checkVarInReg z rz
    case op of
         TAC.Add -> case (ry, rz) of
                      (Register _, _) -> tell [ printInstr "add" [rx, ry, rz] ]
                      (Immediate _, Register _) -> tell [ printInstr "add" [rx, rz, ry] ]
                      (Immediate i1, Immediate i2) -> tell [ printInstr "li" [rx, Immediate (i1+i2)] ]
                      _ -> undefined
         TAC.Sub -> case (ry, rz) of
                      (Register _, _) -> tell [ printInstr "sub" [rx, ry, rz] ]
                      (Immediate _, Register _) -> tell [ printInstr "sub" [rx, rz, ry] , printInstr "neg" [rx, rx] ]
                      (Immediate i1, Immediate i2) -> tell [ printInstr "li" [rx, Immediate (i1-i2)] ]
                      _ -> undefined
         TAC.Mul -> case (ry, rz) of
                      (Register _, _) -> tell [ printInstr "mul" [rx, ry, rz] ]
                      (Immediate _, Register _) -> tell [ printInstr "mul" [rx, rz, ry] ]
                      (Immediate i1, Immediate i2) -> tell [ printInstr "li" [rx, Immediate (i1*i2)] ]
                      _ -> undefined
         TAC.Div -> case (ry, rz) of
                      (Register _, _) -> tell [ printInstr "div" [rx, ry, rz] ]
                      (Immediate _, Register _) -> undefined
                      (Immediate i1, Immediate i2) -> tell [ printInstr "li" [rx, Immediate (i1 `div` i2)] ]
                      _ -> undefined
         TAC.Mod -> case (ry, rz) of
                      (Register _, _) -> tell [ printInstr "rem" [rx, ry, rz] ]
                      (Immediate _, Register _) -> undefined
                      (Immediate i1, Immediate i2) -> tell [ printInstr "li" [rx, Immediate (i1 `mod` i2)] ]
                      _ -> undefined
    return ()

-- UnOpInstr {{{2
tacInstruction2mipsInstruction instr@(TAC.UnOpInstr x op y) = do
    Registers2 rx ry <- getReg instr
    checkVarInReg x rx -- TODO FIX
    checkVarInReg y ry
    case op of
         TAC.Minus -> case ry of
                      (Register _) -> tell [ printInstr "neg" [rx, ry] ]
                      (Immediate i) -> tell [ printInstr "li" [rx, Immediate (-i)] ]
                      _ -> undefined

-- CopyInstr {{{2
tacInstruction2mipsInstruction instr@(TAC.CopyInstr x y) = do
    Registers2 rx ry <- getReg instr
    checkVarInReg x rx -- TODO FIX
    checkVarInReg y ry
    case ry of
      (Register _) -> tell [ printInstr "la" [rx, Parent ry]]
      (Immediate _) -> tell [ printInstr "li" [rx, ry]]
      _ -> undefined

-- Goto {{{2
tacInstruction2mipsInstruction (TAC.Goto label) = do
    spillAtEnd
    tell [ printInstr "b" [Label label]]

-- IfGoto {{{2
tacInstruction2mipsInstruction instr@(TAC.IfGoto x label) = do
    Registers1 rx <- getReg instr
    checkVarInReg x rx
    spillAtEnd
    tell [ printInstr "bnez" [rx, Label label] ]

-- IfRelGoto {{{2
tacInstruction2mipsInstruction instr@(TAC.IfRelGoto x op y label) = do
    Registers2 rx ry <- getReg instr
    checkVarInReg x rx
    checkVarInReg y ry
    spillAtEnd
    case op of
      TAC.LTOET -> case (rx, ry) of
                     (Register _, _) -> tell [ printInstr "ble" [rx, ry, Label label] ]
                     (Immediate _, Register _) -> tell [printInstr "bgt" [ry, rx, Label label] ]
                     (Immediate i1, Immediate i2) -> tell
                                    [ printInstr "b" [ Label label ] | i1 <= i2 ]
                     _ -> undefined
      TAC.LT2 -> case (rx, ry) of
                     (Register _, _) -> tell [ printInstr "blt" [rx, ry, Label label] ]
                     (Immediate _, Register _) -> tell [printInstr "bge" [ry, rx, Label label] ]
                     (Immediate i1, Immediate i2) -> tell
                                    [ printInstr "b" [ Label label ] | i1 < i2 ]
                     _ -> undefined
      TAC.GTOET -> case (rx, ry) of
                     (Register _, _) -> tell [ printInstr "bge" [rx, ry, Label label] ]
                     (Immediate _, Register _) -> tell [printInstr "blt" [ry, rx, Label label] ]
                     (Immediate i1, Immediate i2) -> tell
                                    [ printInstr "b" [ Label label ] | i1 >= i2 ]
                     _ -> undefined
      TAC.GT2 -> case (rx, ry) of
                     (Register _, _) -> tell [ printInstr "bgt" [rx, ry, Label label] ]
                     (Immediate _, Register _) -> tell [printInstr "ble" [ry, rx, Label label] ]
                     (Immediate i1, Immediate i2) -> tell
                                    [ printInstr "b" [ Label label ] | i1 > i2 ]
                     _ -> undefined
      TAC.ET -> case (rx, ry) of
                     (Register _, _) -> tell [ printInstr "beq" [rx, ry, Label label] ]
                     (Immediate _, Register _) -> tell [printInstr "beq" [ry, rx, Label label] ]
                     (Immediate i1, Immediate i2) -> tell
                                    [ printInstr "b" [ Label label ] | i1 == i2 ]
                     _ -> undefined
      TAC.NET -> case (rx, ry) of
                     (Register _, _) -> tell [ printInstr "bne" [rx, ry, Label label] ]
                     (Immediate _, Register _) -> tell [printInstr "bne" [ry, rx, Label label] ]
                     (Immediate i1, Immediate i2) -> tell
                                    [ printInstr "b" [ Label label ] | i1 /= i2 ]
                     _ -> undefined

-- Param {{{2
-- Push operand to state
tacInstruction2mipsInstruction instr@(TAC.Param x) = do
  stk <- gets argStack
  modify (\s -> s{argStack = x : stk})

-- Call {{{2
-- get new fp = (minimum offset not used)
-- save current fp there
-- increase it by 8
-- pop arguments into stack, starting at the new fp
-- spill all the alive at end values
-- jal
tacInstruction2mipsInstruction instr@(TAC.Call label i) = do
  newfpoff <- gets newSPOffset
  tell [ "sw $fp "++show newfpoff++"($fp)" ]
  let newfpoff' = newfpoff+8
  storeArgsFrom newfpoff'
  spillAtEnd
  tell [ "jal " ++ label ]

-- CallAssign {{{2
-- count temporal to compute fp
-- get new fp = (minimum offset not used)
-- save current fp there
-- increase it by 8
-- pop arguments into stack, starting at the new fp
-- spill all the alive at end values
-- jal
tacInstruction2mipsInstruction instr@(TAC.CallAssign x label i) = do
  newfpoff <- (+ getWidth x)  <$> gets newSPOffset
  tell [ "sw $fp "++show newfpoff++"($fp)" ]
  let newfpoff' = newfpoff+8
  storeArgsFrom newfpoff'
  spillAtEnd
  tell [ "jal " ++ label ]

-- ReturnVoid {{{2
-- reload ra (fp-4)
-- reload fp (fp-8)
-- br ra
tacInstruction2mipsInstruction instr@(TAC.ReturnVoid) = do
  tell [ "lw $ra -4($fp)", "lw $fp -8($fp)", "jr $ra" ]


-- Return {{{2
-- copy return value at fp-8-width
-- reload ra (fp-4)
-- reload fp (fp-8)
-- br ra
tacInstruction2mipsInstruction instr@(TAC.Return x) = do
  Registers1 rx <- getReg instr
  checkVarInReg x rx
  tell [ "lw $ra -4($fp)", "lw $fp -8($fp)"]
  tell [ "sw "++show rx++" -12($fp)" ] -- TODO REAL COPY OF ARBITRARY WIDTH
  tell [ "jr $ra" ]

-- array {{{2
tacInstruction2mipsInstruction instr@(TAC.ArrayGetPos x y z) = undefined
tacInstruction2mipsInstruction instr@(TAC.ArraySetPos x y z) = undefined

-- print {{{2
tacInstruction2mipsInstruction instr@(TAC.Print x) = do
  Registers1 rx <- getReg instr
  checkVarInReg x rx
  tell [ "la $a0 ("++show rx++")" ]
  tell [ "li $v0 1" ]
  tell [ "syscall " ]

-- putLabel {{{2
tacInstruction2mipsInstruction instr@(TAC.PutLabel label) = tell [ label ++ ":" ]

-- saveRA {{{2
tacInstruction2mipsInstruction instr@(TAC.SaveRA) = tell [ "sw $ra -4($fp)" ]

-- Exit {{{2
tacInstruction2mipsInstruction instr@(TAC.Exit) =
    tell [ "li $v0 10" , "syscall" ]

-- printInstr {{{2
printInstr :: String -> [Operand] -> String
printInstr oper [] = oper
printInstr oper [o1] = oper ++ " " ++ show o1
printInstr oper [o1,o2] = oper ++ " " ++ show o1 ++ "," ++ show o2
printInstr oper [o1,o2,o3] = oper ++ " " ++ show o1 ++ "," ++ show o2 ++ "," ++ show o3
printInstr o os = error $ "alo? " ++ show o ++ " " ++ show os


-- checkVarInReg {{{1
checkVarInReg :: TAC.X -> Operand -> MachineM ()
checkVarInReg (TAC.IntCons _) _ = return ()
checkVarInReg (varOrTemp) (Register i) = return ()


varOrTempToMachineVar :: TAC.X -> Variable
varOrTempToMachineVar (TAC.Name s) = s
varOrTempToMachineVar (TAC.Temporal i _ s) = ("$t"++show i, s)
varOrTempToMachineVar _ = undefined

data RegAssign = Registers1 Operand
               | Registers2 Operand Operand
               | Registers3 Operand Operand Operand

getReg :: TAC.Instruction -> MachineM RegAssign
getReg (TAC.BinOpInstr x y _ z) = undefined
getReg (TAC.UnOpInstr x _ y) = undefined
getReg (TAC.CopyInstr x y ) = undefined
getReg (TAC.IfGoto x _) = undefined
getReg (TAC.IfRelGoto x _ y _) = undefined
getReg (TAC.Return x) = undefined
getReg (TAC.ArrayGetPos x y i) = undefined
getReg (TAC.ArraySetPos x i y) = undefined
getReg (TAC.Print x) = undefined
getReg _ = undefined

getReg2 :: TAC.X -> MachineM (Operand)
getReg2 = undefined

-- spill everything just before a jump
spillAtEnd :: MachineM ()
spillAtEnd = undefined


getWidth :: TAC.X -> Int
getWidth _ = 4



storeArgsFrom :: Int -> MachineM ()
storeArgsFrom off = do
  args <- reverse <$> gets argStack
  foldM_ f off args
  where f off x = do
          rx <- getReg2 x
          checkVarInReg x rx
          tell [ "sw "++show rx++" "++show off++"($fp)" ]
          return $ off+4
