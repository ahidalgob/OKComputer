module Machine where

import TAC(TAC)
import qualified TAC

import Data.Set(Set)
import qualified Data.Set as Set

import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

type MIPSInstruction = String
type MIPSCode = [MIPSInstruction]

type BlockId = Int

data Register = Register Int | DummyRegister deriving (Eq, Ord)
type Variable = (String, Int)
data Location = Mem | Reg Register

data MachineMState = MachineMState { regHas :: Map Register (Set Variable)
                                   , varIsIn :: Map Variable (Set Location)
                                   , newSPOffset :: Int

                                   , varOffset :: Map Variable Int
                                   , aliveAtEndOfBlock :: Set Variable
                                   , nRegisters :: Int
                                   }
initMachineMState :: Int -> Map Variable Int -> Set Variable -> MachineMState
initMachineMState nR varOff aliveAtEnd = MachineMState { regHas = emptyRegHas nR
                                                       , varIsIn = Map.empty
                                                       , newSPOffset = 0

                                                       , varOffset = varOff
                                                       , aliveAtEndOfBlock = aliveAtEnd
                                                       , nRegisters = nR
                                                       }
emptyRegHas :: Int -> Map Register (Set Variable)
emptyRegHas n = foldl (\m i -> Map.insert (Register i) Set.empty m) Map.empty [0..(n-1)]

type MachineM a = WriterT MIPSCode (StateT MachineMState IO) a

runMachineM :: MachineM a -> Int -> Map Variable Int -> Set Variable
                          -> IO MIPSCode
runMachineM f nR varOff aliveAtEnd = evalStateT (execWriterT f) $ initMachineMState nR varOff aliveAtEnd

tac2mips :: TAC -> MachineM ()
tac2mips tac = mapM_ tacInstruction2mipsInstruction tac

tacInstruction2mipsInstruction :: TAC.Instruction -> MachineM ()
tacInstruction2mipsInstruction instr@(TAC.BinOpInstr x y op z) = do
    Registers3 r1 r2 r3 <- getReg instr
    checkVarInReg x r1
    checkVarInReg y r2
    checkVarInReg z r3
    case op of
         TAC.Add -> tell [""]
         TAC.Sub -> tell [""]
         TAC.Mul -> tell [""]
         TAC.Div -> tell [""]
         TAC.Mod -> tell [""]
    return ()

checkVarInReg :: TAC.X -> Register -> MachineM ()
checkVarInReg x r = undefined


data RegAssign = Registers1 Register
               | Registers2 Register Register
               | Registers3 Register Register Register

getReg :: TAC.Instruction -> MachineM RegAssign
getReg instr = undefined


