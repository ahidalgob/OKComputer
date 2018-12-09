import Lexer
import ParseMonad
import Parser
import AST
import TAC
import SymTable
import Machine
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import System.Environment
import Data.List

-- front {{{1
lexer code = do
  (alexed, state) <- runParseM alexMonadScan code
  case alexed of
      Left msg -> print msg
      Right tokens -> do
          print (alex_invalidC state)
          mapM_ print tokens

parser code = do
  (res,st) <- runParseM parse code
  case res of
    Right algo -> printSTARTN 0 algo
    Left algomas -> putStrLn $ show algomas

sym code = do
  (res,st) <- runParseM parse code
  case res of
    Right algo -> mapM_
        (\(id, lst) -> (putStrLn id >> (mapM_ print lst))) (HM.toList (state_SymTable st))
    Left algomas -> putStrLn $ show algomas

-- tac{{{1
tac code = do
  (Right ast, parseState) <- runParseM parse code
  let funcs' = filter isFuncSym $ concat.HM.elems $ state_SymTable parseState
  let funcs = map (\f -> (sym_label f, sym_AST f)) funcs'

  ((_, tac), tacState) <- runTACkerM (tacStart ast >> tacFuncs funcs) (ParseMonad.state_scwidth parseState) (ParseMonad.state_offset parseState)
  --((_,tac2), tacState) <- runTACkerMS (tacFuncs funcs) tacState'
  --let tac = tac1 ++ tac2
  let bp = backPatchMap tacState
  let bpmap = backPatcher bp tac
  mapM_ print bpmap

  putStrLn "==="
  let offsets = TAC.state_offset tacState
  mapM_ (print . sortOn snd) $ groupBy (\x y -> (snd.fst) x == (snd.fst) y) $ sortOn (snd.fst) $ Map.toList offsets

  let sc_widths = TAC.state_scwidth tacState
  let sc_parent = ParseMonad.state_scparent parseState

  putStrLn "=== widths"
  mapM_ print $ Map.toList sc_widths
  putStrLn "=== parents"
  mapM_ print $ Map.toList sc_parent

  let sc_off = computeScoff (Map.toAscList sc_parent) (Map.singleton 0 0) sc_widths
  putStrLn "=== sc offs"
  mapM_ print $ Map.toList sc_off

  putStrLn "=== final offset"
  let offsets' = recomputeOffset (Map.toList offsets) sc_off
  mapM_ (print . sortOn snd) $ groupBy (\x y -> (snd.fst) x == (snd.fst) y) $ sortOn (snd.fst) $ Map.toList offsets'

-- mips{{{1
mips code = do
  (Right ast, parseState) <- runParseM parse code
  let funcs' = filter isFuncSym $ concat.HM.elems $ state_SymTable parseState
  let funcs = map (\f -> (sym_label f, sym_AST f)) funcs'

  ((_, tac), tacState) <- runTACkerM (tacStart ast >> tacFuncs funcs) (ParseMonad.state_scwidth parseState) (ParseMonad.state_offset parseState)
  let bp = backPatchMap tacState
  let tac' = backPatcher bp tac
  putStrLn "TAC"
  mapM_ print tac'

  let offsets = TAC.state_offset tacState
  let sc_widths = TAC.state_scwidth tacState
  let sc_parent = ParseMonad.state_scparent parseState

  let sc_off = computeScoff (Map.toAscList sc_parent) (Map.singleton 0 0) sc_widths
  let offsets' = recomputeOffset (Map.toList offsets) sc_off

  putStrLn "\n\nBlocks"
  let (graph, mips_code, ins, outs) = Machine.mipsCode tac' offsets'
  print graph
  mapM_ (\(id, bl) -> putStrLn ("+++++++++++++"++show id) >> mapM_ print bl) mips_code


  mapM_ (\(id, set) -> putStrLn ("+++++++++++++"++show id) >> mapM_ print set) ins
  mapM_ (\(id, set) -> putStrLn ("+++++++++++++"++show id) >> mapM_ print set) outs

-- aux{{{1
computeScoff :: [(Scope, Scope)] -> Map.Map Scope Int -> Map.Map Scope Int -> Map.Map Scope Int
computeScoff [] m _ = m
computeScoff ((sc, par_sc):ls) scoff widths = computeScoff ls (Map.insert sc (par_scoff + par_width) scoff) widths
  where Just par_scoff = Map.lookup par_sc scoff
        Just par_width = Map.lookup par_sc widths

recomputeOffset :: [((String, Scope), Int)] -> Map.Map Scope Int -> Map.Map (String, Scope) Int
recomputeOffset [] _ = Map.empty
recomputeOffset (((id, sc), off1):ls) sc_off = Map.insert (id, sc) (off1 + scope_off) $ recomputeOffset ls sc_off
  where Just scope_off = Map.lookup sc sc_off

backPatcher :: Map.Map Label Label -> TAC -> TAC
backPatcher _ [] = []
backPatcher m (Goto lab : ls) = Goto (backPatchLabel m lab):
                                  backPatcher m ls
backPatcher m (IfGoto x lab : ls) = IfGoto x (backPatchLabel m lab):
                                  backPatcher m ls
--backPatcher m (IfFalseGoto x lab)
backPatcher m (IfRelGoto x op y lab : ls) = IfRelGoto x op y (backPatchLabel m lab):
                                  backPatcher m ls
backPatcher m (i:ls) = i : backPatcher m ls


backPatchLabel :: Map.Map Label Label -> Label -> Label
backPatchLabel m lab =
  case Map.lookup lab m of
    Nothing -> lab
    Just lab' -> lab'



-- main{{{1
main = do
  [option, file] <- getArgs
  case option of
       "-l" -> readFile file >>= lexer
       "-p" -> readFile file >>= parser
       "-s" -> readFile file >>= sym
       "-t" -> readFile file >>= tac
       "-m" -> readFile file >>= mips
