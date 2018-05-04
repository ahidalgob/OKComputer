{
module Lexer(Token(..)) where
}

%wrapper "monadUserState"


$digit = 0-9			-- digits
$Alpha = [a-zA-Z]		-- alphabetic characters
-- ...

tokens :-
  $white+				                    ; -- skip white spaces
  "#".*                             ; -- skip comments
  
  -- Instructions
  youbegin                                {newToken YouBeginTkn}        -- Block Start
  whereiend                               {newToken WhereIEndTkn}       -- Block End
  if                                      {newToken IfTkn}              -- Selection
  ifyouhavetoask                          {newToken IfYouHaveToAskTkn}  -- Selection
  otherside                               {newToken OthersideTkn}       -- Selection
  cantstop                                {newToken CantStopTkn}        -- While Iteration
  breakthru                               {newToken BreakthruTkn}       -- Break
  onemoretime                             {newToken OneMoreTimeTkn}     -- For Iteration
  to                                      {newToken ToTkn}              -- For Iteration
  readmymind                              {newToken ReadMyMindTkn}      -- Data entry/read
  go                                      {newToken GoTkn}              -- Data exit/write
  goslowly                                {newToken GoSlowlyTkn}        -- Data exit/writeln
  neworder                                {newToken NewOrderTkn}        -- Method/Proc
  dafunk                                  {newToken DaFunkTkn}          -- Method with return/Function
  getback                                 {newToken GetBackTkn}         -- Return
  intothevoid                             {newToken IntoTheVoidTkn}     -- Void
  newlife                                 {newToken NewLifeTkn}         -- Calloc
  saveme                                  {newToken SaveMeTkn}          -- Malloc
  keepyourselfalive                       {newToken KeepAliveTkn}       -- Realloc
  amnesiac                                {newToken AmnesiacTkn}        -- Free
  exitmusic                               {newToken ExitMusicTkn}       -- Exit
  aroundtheworld                          {newToken AroundTheWorldTkn}  -- Import
  holeinmysoul                            {newToken HoleInMySoulTkn}    -- Templates

  -- Type Tokens
  int                                     {newToken IntTkn}
  float                                   {newToken FloatTkn}
  char                                    {newToken CharTkn}
  boolean                                 {newToken BooleanTkn}
  true                                    {newToken TrueTkn}
  false                                   {newToken FalseTkn}
  \[                                      {newToken ArrayStartTkn}
  \]                                      {newToken ArrayEndTkn}
  band                                    {newToken BandTkn}            -- Registers/structs
  union                                   {newToken UnionTkn}
  \&                                      {newToken PointerTkn}         -- Pointers
  duets                                   {newToken DuetsTkn}           -- Tuple
  left                                    {newToken LeftTkn}
  right                                   {newToken RightTkn}

  -- Operations Tokens
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
  \!\=                                    {newToken DifTkn}
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
