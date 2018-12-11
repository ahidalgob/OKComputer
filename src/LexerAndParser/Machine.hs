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
  show (Register i)
      | 0 <= i && i < 10 = "$t" ++ show i
      | otherwise = "$s" ++ show (i-10)
  show (Immediate i) = show i
  show (Label l) = l
  show (Parent op) = "(" ++ show op ++ ")"

type Variable = (String, Int)
data Location = Mem | Reg Int

-- MachineM Monad{{{1
data MachineMState = MachineMState { regHas :: Map Operand (Set Variable) -- what variables does each register has
                                   , varIsIn :: Map Variable (Set Location) -- where is each variable
                                   , newSPOffset :: Int -- where to set new sp on calls
                                   , argStack :: [TAC.X] -- args to push

                                   , varOffset :: Map Variable Int -- offset of each variable
                                   , aliveAtEndOfBlock :: Set Variable -- set of variables at the end
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



-- tacInstruction2mipsInstruction  {{{1
tac2mips :: TAC -> MachineM ()
tac2mips tac = mapM_ tacInstruction2mipsInstruction tac

-- ######### getReg ############
-- gets a list of registers to use in the instruction
-- it should be safe to use the registers after the call to getReg, this is:
--  for operands: either the register already has the variable or
--                we can load it to the register and add it to the set, in this case
--                the set should be already empty
--  for result:
--
-- ###########################
-- For noncopy instructions, the result register has only the result, no other value

tacInstruction2mipsInstruction :: TAC.Instruction -> MachineM ()
-- BinOpInstr {{{2
tacInstruction2mipsInstruction instr@(TAC.BinOpInstr x y op z) = do

    [rx, ry, rz] <- getReg instr
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
    setVarInReg x rx

-- UnOpInstr {{{2
tacInstruction2mipsInstruction instr@(TAC.UnOpInstr x op y) = do
    [rx, ry] <- getReg instr
    checkVarInReg y ry
    case op of
         TAC.Minus -> case ry of
                      (Register _) -> tell [ printInstr "neg" [rx, ry] ]
                      (Immediate i) -> tell [ printInstr "li" [rx, Immediate (-i)] ]
                      _ -> undefined
    setVarInReg x rx

-- CopyInstr {{{2
tacInstruction2mipsInstruction instr@(TAC.CopyInstr x y) = do
    [rx, ry] <- getReg instr
    checkVarInReg y ry
    addVarInReg x ry
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
    [rx] <- getReg instr
    checkVarInReg x rx
    spillAtEnd
    tell [ printInstr "bnez" [rx, Label label] ]

-- IfRelGoto {{{2
tacInstruction2mipsInstruction instr@(TAC.IfRelGoto x op y label) = do
    [rx, ry] <- getReg instr
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
  tell [ "sw $fp -"++show newfpoff++"($fp)" ]
  let newfpoff' = newfpoff+8
  storeArgsStarting newfpoff'
  spillAtEnd
  tell [ "jal " ++ label ]

-- CallAssign {{{2
tacInstruction2mipsInstruction instr@(TAC.CallAssign x label i) = do
  newfpoff <- (+ getWidth x)  <$> gets newSPOffset
  modify (\s -> s{newSPOffset = newfpoff})
  tacInstruction2mipsInstruction (TAC.Call label i)

-- ReturnVoid {{{2
-- reload ra (fp-4)
-- reload fp (fp-8)
-- br ra
tacInstruction2mipsInstruction instr@(TAC.ReturnVoid) = do
  tell [ "lw $ra 4($fp)", "lw $fp 8($fp)", "jr $ra" ]


-- Return {{{2
-- copy return value at fp-8-width
-- reload ra (fp-4)
-- reload fp (fp-8)
-- br ra
tacInstruction2mipsInstruction instr@(TAC.Return x) = do
  [rx] <- getReg instr
  checkVarInReg x rx
  tell [ "lw $ra 4($fp)", "lw $fp 8($fp)"]
  tell [ "sw "++show rx++" 12($fp)" ] -- TODO REAL COPY OF ARBITRARY WIDTH
  tell [ "jr $ra" ]
  setVarInReg x rx

-- array {{{2
tacInstruction2mipsInstruction instr@(TAC.ArrayGetPos x y z) = undefined
tacInstruction2mipsInstruction instr@(TAC.ArraySetPos x y z) = undefined

-- print {{{2
tacInstruction2mipsInstruction instr@(TAC.Print x) = do
  [rx] <- getReg instr
  checkVarInReg x rx
  tell [ "la $a0 ("++show rx++")" ]
  tell [ "li $v0 1" ]
  tell [ "syscall " ]

-- putLabel {{{2
tacInstruction2mipsInstruction instr@(TAC.PutLabel label) = tell [ label ++ ":" ]

-- saveRA {{{2
tacInstruction2mipsInstruction instr@(TAC.SaveRA) = tell [ "sw $ra 4($fp)" ]

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


-- checkVarInReg, getReg... {{{1

-- getReg {{{2
getReg :: TAC.Instruction -> MachineM [Operand]
getReg (TAC.BinOpInstr x y _ z) = do
  ry <- findBestOperandReg y [z]
  rz <- findBestOperandReg z [y]
  rx <- findBestResultReg x
  return [rx, ry, rz]

getReg (TAC.UnOpInstr x _ y) = do
  ry <- findBestOperandReg y []
  rx <- findBestResultReg x
  return [rx, ry]

getReg (TAC.CopyInstr x y ) =
  case y of
    (TAC.IntCons i) -> do
      rx <- findBestResultReg x
      return [rx, Immediate i]
    _ -> do
      ry <- findBestOperandReg y []
      return [ry, ry]

getReg (TAC.IfGoto y _) = do
      ry <- findBestOperandReg y []
      return [ry]

getReg (TAC.IfRelGoto y _ z _) = do
      ry <- findBestOperandReg y [z]
      rz <- findBestOperandReg z [y]
      return [ry, rz]

getReg (TAC.Return y) = do
      ry <- findBestOperandReg y []
      return [ry]

getReg (TAC.Print y) = do
      ry <- findBestOperandReg y []
      return [ry]

getReg (TAC.ArrayGetPos x y i) = undefined
getReg (TAC.ArraySetPos x i y) = undefined
getReg _ = undefined

-- getReg helpers {{{2

findBestResultReg :: TAC.X -> MachineM Operand
findBestResultReg (TAC.IntCons i) = return $ Immediate i
findBestResultReg x' = do
  let x = varOrTempToMachineVar x'
  maybeR1 <- findRegisterWithOnly x
  case maybeR1 of
    Just r -> return r
    Nothing -> do
      maybeR2 <- findEmptyRegister
      case maybeR2 of
        Just r -> return r
        Nothing -> findRegisterWithout []


findBestOperandReg :: TAC.X -> [TAC.X] -> MachineM Operand
findBestOperandReg x' l' = do
  let x = varOrTempToMachineVar x'
      l = map varOrTempToMachineVar (filter tmpOrVar l')
  maybeR1 <- findRegisterWith x
  case maybeR1 of
    Just r -> return r
    Nothing -> do
      maybeR2 <- findEmptyRegister
      case maybeR2 of
        Just r -> return r
        Nothing -> findRegisterWithout l

findRegisterWithOnly :: Variable -> MachineM (Maybe Operand)
findRegisterWithOnly = undefined

findRegisterWith :: Variable -> MachineM (Maybe Operand)
findRegisterWith = undefined

findEmptyRegister :: MachineM (Maybe Operand)
findEmptyRegister = undefined

findRegisterWithout :: [Variable] -> MachineM Operand
findRegisterWithout = undefined

checkVarInReg :: TAC.X -> Operand -> MachineM ()
checkVarInReg (TAC.IntCons _) _ = return ()
checkVarInReg (varOrTemp) (Register i) = return ()

setVarInReg :: TAC.X -> Operand -> MachineM ()
setVarInReg = undefined

addVarInReg :: TAC.X -> Operand -> MachineM ()
addVarInReg = undefined


spillAtEnd :: MachineM ()
spillAtEnd = undefined

getWidth :: TAC.X -> Int
getWidth _ = 4

storeArgsStarting :: Int -> MachineM ()
storeArgsStarting off = do
  args <- reverse <$> gets argStack
  foldM_ f off args
  where f off x = do
          rx <- findBestOperandReg x []
          checkVarInReg x rx
          tell [ "sw "++show rx++" -"++show off++"($fp)" ] -- Don't work for constants
          return $ off+4

varOrTempToMachineVar :: TAC.X -> Variable
varOrTempToMachineVar (TAC.Name s) = s
varOrTempToMachineVar (TAC.Temporal i _ s) = ("$t"++show i, s)
varOrTempToMachineVar _ = undefined

tmpOrVar :: TAC.X -> Bool
tmpOrVar TAC.Name{} = True
tmpOrVar TAC.Temporal{} = True
tmpOrVar _ = undefined
