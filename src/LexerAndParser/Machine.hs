module Machine where

import TAC(TAC)
import qualified TAC

import Data.Set(Set)
import qualified Data.Set as Set

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

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
data Location = Mem | Reg Operand deriving (Eq, Ord)

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

fromJust' :: String -> Maybe a -> a
fromJust' _ (Just x) = x
fromJust' s Nothing = error s

updateNewSP :: TAC.X -> MachineM ()
updateNewSP TAC.IntCons{} = return ()
updateNewSP v =
  case varOrTempToMachineVar v of
    (_, 0) -> return ()
    var -> do
      old <- gets newSPOffset
      off <- (fromJust' "F1") . Map.lookup var <$> gets varOffset
      modify (\s -> s{newSPOffset = old `max` off+4})

-- tacInstruction2mipsInstruction  {{{1
tac2mips :: TAC -> MachineM ()
tac2mips tac = mapM_ tacInstruction2mipsInstruction (tac ++ [TAC.Spill])

tacInstruction2mipsInstruction :: TAC.Instruction -> MachineM ()
-- BinOpInstr {{{2
tacInstruction2mipsInstruction instr@(TAC.BinOpInstr x y op z) = do
    updateNewSP x
    updateNewSP y
    updateNewSP z
    [rx, ry, rz] <- getReg instr
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

-- UnOpInstr {{{2
tacInstruction2mipsInstruction instr@(TAC.UnOpInstr x op y) = do
    updateNewSP x
    updateNewSP y
    [rx, ry] <- getReg instr
    case op of
         TAC.Minus -> case ry of
                      (Register _) -> tell [ printInstr "neg" [rx, ry] ]
                      (Immediate i) -> tell [ printInstr "li" [rx, Immediate (-i)] ]
                      _ -> undefined

-- CopyInstr {{{2
tacInstruction2mipsInstruction instr@(TAC.CopyInstr x y) = do
    updateNewSP x
    updateNewSP y
    [rx, ry] <- getReg instr
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
    updateNewSP x
    [rx] <- getReg instr
    spillAtEnd
    tell [ printInstr "bnez" [rx, Label label] ]

-- IfRelGoto {{{2
tacInstruction2mipsInstruction instr@(TAC.IfRelGoto x op y label) = do
    updateNewSP x
    updateNewSP y
    [rx, ry] <- getReg instr
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
  updateNewSP x
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
  let newfpoff' = newfpoff+8
  storeArgsStarting newfpoff'
  spillAtEnd
  tell [ "sw $sp,-"++show newfpoff++"($sp)" ]
  tell [ "add $sp,$sp,-"++show newfpoff' ]
  tell [ "jal " ++ label ]

-- CallAssign {{{2
tacInstruction2mipsInstruction instr@(TAC.CallAssign x label i) = do
  let x' = varOrTempToMachineVar x
  off <- fromJust' "F420" . Map.lookup x' <$> gets varOffset
  modify (\s -> s{newSPOffset = off+12-8})
  tacInstruction2mipsInstruction (TAC.Call label i)

-- ReturnVoid {{{2
-- reload ra (fp+4)
-- reload fp (fp+8)
-- br ra
tacInstruction2mipsInstruction instr@(TAC.ReturnVoid) = do
  tell [ "lw $ra,4($sp)", "lw $sp,8($sp)", "jr $ra" ]


-- Return {{{2
-- copy return value at fp+8+width
-- reload ra (fp+4)
-- reload fp (fp+8)
-- br ra
tacInstruction2mipsInstruction instr@(TAC.Return x) = do
  updateNewSP x
  [rx] <- getReg instr
  case rx of
    Immediate _ -> tell [ "li $fp,"++show rx, "sw $fp,12($sp)" ]
    _ ->  tell [ "sw "++show rx++",12($sp)" ]
  tell [ "lw $ra,4($sp)", "lw $sp,8($sp)"]
  tell [ "jr $ra" ]

-- array {{{2
-- x = y[i]
tacInstruction2mipsInstruction instr@(TAC.ArrayGetPos x y i) = do
  updateNewSP x
  updateNewSP i
  let y' = varOrTempToMachineVar y
  [rx, ri] <- getReg instr
  off <- fromJust' "F69" . Map.lookup y' <$> gets varOffset
  tell [ "add $a3,$fp,"++show off ]
  tell [ "add $a3,$a3,"++show ri ]
  tell [ "lw "++show rx++",($a3)" ]
  return ()

-- x[i] = y
tacInstruction2mipsInstruction instr@(TAC.ArraySetPos x i y) = do
  updateNewSP i
  updateNewSP y
  let x' = varOrTempToMachineVar x
  [ri, ry] <- getReg instr
  off <- fromJust' "F69" . Map.lookup x' <$> gets varOffset
  tell [ "add $a3,$fp,"++show off ]
  tell [ "add $a3,$a3,"++show ri ]
  tell [ "sw "++show ry++",($a3)" ] -- maybe don't work with immediates
  return ()


-- print {{{2
tacInstruction2mipsInstruction instr@(TAC.Print x) = do
  updateNewSP x
  [rx] <- getReg instr
  tell [ "la $a0,("++show rx++")" ]
  tell [ "li $v0,1" ]
  tell [ "syscall" ]

-- putLabel {{{2
tacInstruction2mipsInstruction instr@(TAC.PutLabel label) = tell [ label ++ ":" ]

-- saveRA {{{2
tacInstruction2mipsInstruction instr@(TAC.SaveRA) = tell [ "sw $ra,4($sp)" ]

tacInstruction2mipsInstruction (TAC.Spill) = do
    spillAtEnd

-- Exit {{{2
tacInstruction2mipsInstruction instr@(TAC.Exit) =
    tell [ "li $v0,10" , "syscall" ]

-- printInstr {{{2
printInstr :: String -> [Operand] -> String
printInstr oper [] = oper
printInstr oper [o1] = oper ++ " " ++ show o1
printInstr oper [o1,o2] = oper ++ " " ++ show o1 ++ "," ++ show o2
printInstr oper [o1,o2,o3] = oper ++ " " ++ show o1 ++ "," ++ show o2 ++ "," ++ show o3
printInstr o os = error $ "alo? FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" ++ show o ++ " " ++ show os


-- getReg... {{{1

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
      let x' = varOrTempToMachineVar x
      removeVarEverywhere x'
      insertVarInRegister x' ry
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

-- x = y[i]
getReg (TAC.ArrayGetPos x y i) = do
      ri <- findBestOperandReg i []
      rx <- findBestResultReg x
      return [rx, ri]

-- x[i] = y
getReg (TAC.ArraySetPos x i y) = do
      ri <- findBestOperandReg i []
      ry <- findBestOperandReg y []
      return [ri, ry]

getReg _ = undefined

-- getReg helpers {{{2


findBestOperandReg :: TAC.X -> [TAC.X] -> MachineM Operand
findBestOperandReg (TAC.IntCons i) _ = return $ Immediate i
findBestOperandReg x' l' = do
  let x = varOrTempToMachineVar x'
      l = map varOrTempToMachineVar (filter tmpOrVar l')
  maybeR1 <- findRegisterWith x
  case maybeR1 of
    Just r -> return r
    Nothing -> do
      maybeR2 <- findEmptyRegister
      case maybeR2 of
        Just r -> do
          loadVarInReg x r
          return r
        Nothing -> do
          rx <- findRegisterWithout l
          cleanRegister rx
          loadVarInReg x rx
          return rx

findBestResultReg :: TAC.X -> MachineM Operand
findBestResultReg x' = do
  let x = varOrTempToMachineVar x'
  maybeR1 <- findRegisterWithOnly x
  case maybeR1 of
    Just r -> do
      removeVarEverywhere x
      insertVarInRegister x r
      return r
    Nothing -> do
      maybeR2 <- findEmptyRegister
      case maybeR2 of
        Just r -> do
          removeVarEverywhere x
          insertVarInRegister x r
          return r
        Nothing -> do
          r <- findRegisterWithout []
          removeVarEverywhere x
          cleanRegister r
          insertVarInRegister x r
          return r


-- x always end only at r after this
insertVarInRegister :: Variable -> Operand -> MachineM ()
insertVarInRegister x r = do
  regHas' <- Map.adjust (Set.insert x) r <$> gets regHas
  varIsIn' <- Map.insert x (Set.singleton (Reg r)) <$> gets varIsIn
  modify (\s -> s{regHas = regHas', varIsIn = varIsIn'})

removeVarEverywhere :: Variable -> MachineM ()
removeVarEverywhere x = do
  xIsIn <- Set.toAscList <$> Map.findWithDefault (Set.empty) x <$> gets varIsIn
  mapM_ (removeVarFrom x) xIsIn
  varIsIn' <- Map.insert x (Set.empty) <$> gets varIsIn
  modify (\s -> s{varIsIn = varIsIn'})

removeVarFrom :: Variable -> Location -> MachineM ()
removeVarFrom x (Mem) = return ()
removeVarFrom x (Reg r) = do
  regHas' <- Map.adjust (Set.delete x) r <$> gets regHas
  modify (\s -> s{regHas = regHas'})

removeRegFrom :: Location -> Variable -> MachineM ()
removeRegFrom loc x = do
  varIsIn' <- Map.adjust (Set.delete loc) x <$> gets varIsIn
  modify (\s -> s{varIsIn = varIsIn'})

initializeMapWith :: Ord k => k -> a -> Map k a -> Map k a
initializeMapWith k a s =
  case Map.lookup k s of
    Just _ -> s
    Nothing -> Map.insert k a s


-- just writes the code, adds r to x and x to r
loadVarInReg :: Variable -> Operand -> MachineM ()
loadVarInReg x r = do
  regHas' <- Map.insert r (Set.singleton x) <$> gets regHas
  varIsIn' <- Map.insert x (Set.fromList [Mem, Reg r]) <$> gets varIsIn
  modify (\s -> s{regHas = regHas', varIsIn = varIsIn'})
  case x of
    (_, 0) -> undefined
    _ -> do
      off <- (fromJust' "F2") . Map.lookup x <$> gets varOffset
      tell ["lw "++show r++",-"++show off++"($sp)"]

-- removes all variables for r
cleanRegister :: Operand -> MachineM ()
cleanRegister r = do
  rHas <- Set.toList . (fromJust' "F3") . Map.lookup r <$> gets regHas
  toSpill <- filterM (variableOnlyIn r) rHas
  mapM_ (spillVariableInRegister r) toSpill
  mapM_ (removeRegFrom $ Reg r) rHas
  regHas' <- Map.insert r Set.empty <$> gets regHas
  modify (\s -> s{regHas = regHas'})
  where
    variableOnlyIn :: Operand -> Variable -> MachineM Bool
    variableOnlyIn r x = (== Set.singleton (Reg r)) . (fromJust' "F4") . Map.lookup x <$> gets varIsIn


spillVariableInRegister :: Operand -> Variable -> MachineM ()
spillVariableInRegister r var@(id, 0) = do
  tell ["sw "++show r++","++id]
  varIsIn' <- Map.adjust (Set.insert Mem) var <$> gets varIsIn
  modify (\s -> s{varIsIn = varIsIn'})
spillVariableInRegister r var = do
  off <- (fromJust' "F5") . Map.lookup var <$> gets varOffset
  tell ["sw "++show r++",-"++show off++"($sp)"]
  varIsIn' <- Map.adjust (Set.insert Mem) var <$> gets varIsIn
  modify (\s -> s{varIsIn = varIsIn'})


findRegisterWithOnly :: Variable -> MachineM (Maybe Operand)
findRegisterWithOnly x = do
  regs <- Map.toList . Map.filter (== Set.singleton x) <$> gets regHas
  case regs of
    [] -> return Nothing
    r:_ -> return $ Just $ fst r

findRegisterWith :: Variable -> MachineM (Maybe Operand)
findRegisterWith x = do
  regs <- Map.toList . Map.filter (Set.member x) <$> gets regHas
  case regs of
    [] -> return Nothing
    r:_ -> return $ Just $ fst r

findEmptyRegister :: MachineM (Maybe Operand)
findEmptyRegister = do
  regs <- Map.toList . Map.filter (== Set.empty) <$> gets regHas
  case regs of
    [] -> return Nothing
    r:_ -> return $ Just $ fst r

findRegisterWithout :: [Variable] -> MachineM Operand
findRegisterWithout [] = return (Register 0)
findRegisterWithout [x] = do
  regs <- Map.toList . Map.filter (Set.notMember x) <$> gets regHas
  case regs of
    r:_ -> return $ fst r
    _ -> error "F"

spillAtEnd :: MachineM ()
spillAtEnd = do
  toSpill <- Set.toList <$> gets aliveAtEndOfBlock
  mapM_ spillIfNeed toSpill

spillIfNeed :: Variable -> MachineM ()
spillIfNeed x = do
  xIsIn <- (fromJust' "F6") . Map.lookup x . initializeMapWith x (Set.singleton Mem)  <$> gets varIsIn
  case Set.member Mem xIsIn of
    True -> return ()
    False -> let Reg r = Set.findMin xIsIn
              in spillVariableInRegister r x

getWidth :: TAC.X -> Int
getWidth _ = 4

storeArgsStarting :: Int -> MachineM ()
storeArgsStarting off = do
  args <- reverse <$> gets argStack
  foldM_ f off args
  where f off x = do
          rx <- findBestOperandReg x []
          case rx of
            Immediate _ -> tell [ "li $fp,"++show rx, "sw $fp,-"++show off++"($sp)" ]
            _ ->  tell [ "sw "++show rx++",-"++show off++"($sp)" ]
          return $ off+4

varOrTempToMachineVar :: TAC.X -> Variable
varOrTempToMachineVar (TAC.Name s) = s
varOrTempToMachineVar (TAC.Temporal i _ s) = ("$t"++show i, s)
varOrTempToMachineVar _ = undefined

tmpOrVar :: TAC.X -> Bool
tmpOrVar TAC.Name{} = True
tmpOrVar TAC.Temporal{} = True
tmpOrVar _ = undefined
