{
module Lexer(Token(..)) where
}

%wrapper "monadUserState"


$digit = 0-9			-- digits
$Alpha = [a-zA-Z]		-- alphabetic characters
-- ...

tokens :-
  $white+				                    ; -- skip white spaces

  cantstop                                  {newToken CantStopTkn}


{

type Pos = (Int, Int)

data Token =
  CantStopTkn { tknPos :: (Int, Int) }

  deriving Show



alexEOF :: Alex [Token]
alexEOF = return []

--------- User State
data AlexUserState = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = undefined



getPos :: AlexInput -> Pos
getPos ((AlexPn _ line col), _, _, _) = (line, col)


newToken :: (Pos -> Token) -> AlexAction [Token]
newToken tknConstr = \alexIn _ -> do
    let pos = getPos alexIn
    tokens <- alexMonadScan
    return $ (tknConstr pos):tokens


getInput :: IO String
getInput = undefined

main = do
    code <- getInput
    let alexed = runAlex code (alexMonadScan >>=
                (\lt -> Alex (\s -> Right (s, (s,lt)))))
    case alexed of
        Left msg -> undefined
        Right (state, tokens) -> undefined

}
