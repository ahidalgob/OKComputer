import Lexer
import ParseMonad
import Parser
import AST
import TAC
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import System.Environment

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

tac code = do
  (Right ast, parseState) <- runParseM parse code
  ((_, tac), tacState) <- runTACkerM (tacStart ast)
  let bp = backPatchMap tacState
  let bpmap = backPatcher bp tac
  mapM_ print bpmap


backPatcher :: M.Map Label Label -> TAC -> TAC
backPatcher _ [] = []
backPatcher m (Goto lab : ls) = Goto (backPatchLabel m lab):
                                  backPatcher m ls
backPatcher m (IfGoto x lab : ls) = IfGoto x (backPatchLabel m lab):
                                  backPatcher m ls
--backPatcher m (IfFalseGoto x lab)
backPatcher m (IfRelGoto x op y lab : ls) = IfRelGoto x op y (backPatchLabel m lab):
                                  backPatcher m ls
backPatcher m (i:ls) = i : backPatcher m ls


backPatchLabel :: M.Map Label Label -> Label -> Label
backPatchLabel m lab =
  case M.lookup lab m of
    Nothing -> lab
    Just lab' -> lab'



main = do
  [option, file] <- getArgs
  case option of
       "-l" -> readFile file >>= lexer
       "-p" -> readFile file >>= parser
       "-s" -> readFile file >>= sym
       "-t" -> readFile file >>= tac
