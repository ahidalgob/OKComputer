import Lexer
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
  (alexed, state) <- runParseM parse code
  return ()

main = do
  code <- getArgs >>= readFile.head
  parser code
