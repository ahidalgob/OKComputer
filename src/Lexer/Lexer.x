{
module Lexer(Token(..)) where
}

%wrapper "monadUserState"


$digit = 0-9			-- digits
$Alpha = [a-zA-Z]		-- alphabetic characters
-- ...

tokens :-
  $white+				                    ; -- skip white spaces

  cantstop                                {newToken CantStopTkn}

  -- Type Tokens
  int                                     {newToken IntTkn}
  float                                   {newToken FloatTkn}



  -- Operations Tokens
  true                                    {newToken TrueTkn}
  false                                   {newToken FalseTkn}
  mod                                     {newToken ModTkn}
  div                                     {newToken DivTkn}
  not                                     {newToken NotTkn}
  and                                     {newToken AndTkn}
  or                                      {newToken OrTkn}
  \,                                      {newToken CommaTkn}
  \(                                      {newToken ParenOpenTkn}
  \)                                      {newToken ParenCloseTkn}    
  \;                                      {newToken SemicolonTkn}
  \+                                      {newToken PlusTkn}
  \=\=                                    {newToken EqualTkn}
  \*                                      {newToken ProductTkn}    
  \-                                      {newToken MinusTkn}
  \%                                      {newToken RestTkn}
  \/                                      {newToken DivExacTkn}
  \/\=                                    {newToken DifTkn}
  \>\=                                    {newToken GreaterEqualTkn}
  \<\=                                    {newToken LessEqualTkn}
  \>                                      {newToken GreaterTkn}
  \<                                      {newToken LessTkn}
  \-\>                                    {newToken TypeTkn}
  \=                                      {newToken AssignTkn}


{

type Pos = (Int, Int)

data Token =
  CantStopTknn { TknnPos :: (Int, Int) }

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
newToken TknnConstr = \alexIn _ -> do
    let pos = getPos alexIn
    tokens <- alexMonadScan
    return $ (TknnConstr pos):tokens


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
