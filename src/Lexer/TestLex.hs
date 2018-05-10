import Lexer
import System.Environment

main = do
  code <- getArgs >>= readFile.head
  alexed <- runParseM code alexMonadScan
  case alexed of
      Left msg -> print msg
      Right (tokens, state) -> do
          print (alex_invalidC state)
          mapM_ print tokens
