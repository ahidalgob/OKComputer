import Lexer
import System.Environment

main = do
  code <- getArgs >>= readFile.head
  let alexed = runAlex code (alexMonadScan >>=
              (\lt -> Alex (\s -> Right (s, (s,lt)))))
  case alexed of
      Left msg -> print msg
      Right (state, tokens) -> do
          print (alex_ust state)
          mapM_ print tokens
