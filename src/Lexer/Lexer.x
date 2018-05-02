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
  CantStopTkn { TknPos :: (Int, Int) }

  deriving Show


data AlexUserState = undefined

alexInitUserState :: AlexUserState
alexInitUserState = undefined


newToken :: (Pos -> Token) -> AlexAction [Token]
newToken tknConstr = \alexIn _ -> do
    let pos = getPosition alexIn
    tokens <- alexMonadScan
    return $ (tknConstr pos):tokens



main = do
    code <- undefined
    alexed <- runAlex code (alexMonadScan >>=
                (lt -> Alex (\s -> Right (s, (s,lt)))))
    case alexed of
        Left msg -> undefined
        Right (state, tokens) -> undefined

}
