import Lexer
import ParseMonad
import Parser
import System.Environment

lexer code = do
  (alexed, state) <- runParseM alexMonadScan code
  case alexed of
      Left msg -> print msg
      Right tokens -> do
          print (alex_invalidC state)
          mapM_ print tokens

parser code = do
  res <- runParseM parse code
  print res

main = do
  [option, file] <- getArgs
  case option of
       "-l" -> readFile file >>= lexer
       "-p" -> readFile file >>= parser
