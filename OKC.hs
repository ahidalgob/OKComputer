import Lexer
import ParseMonad
import Parser
import AST
import TAC
import Data.HashMap.Strict
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
        (\(id, lst) -> (putStrLn id >> (mapM_ print lst))) (toList (state_SymTable st))
    Left algomas -> putStrLn $ show algomas

tac code = do
  (Right ast, parseState) <- runParseM parse code
  ((_, tac), tacState) <- runTACkerM (tacStart ast)
  mapM_ print tac






main = do
  [option, file] <- getArgs
  case option of
       "-l" -> readFile file >>= lexer
       "-p" -> readFile file >>= parser
       "-s" -> readFile file >>= sym
       "-t" -> readFile file >>= tac
