import Lexer
import ParseMonad
import Parser
import AST
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
  print (res,st)
  case res of
--    Right algo -> putStrLn $ show algo
    Right algo -> printSTARTN 0 algo 
    Left algomas -> putStrLn $ show algomas
-- printSTARTN 0 (Right res)
--  Right res -> print res
--      printSTARTN res  

main = do
  [option, file] <- getArgs
  case option of
       "-l" -> readFile file >>= lexer
       "-p" -> readFile file >>= parser
