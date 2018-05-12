import Lexer
import System.Environment

main = do
  code <- getArgs >>= readFile.head
  (alexed, state) <- runParseM alexMonadScan code
  case alexed of
      Left msg -> print msg
      Right tokens -> do
          print (alex_invalidC state)
          mapM_ print tokens
