{
module Lexer(Token(..), Alex(Alex), alexMonadScan, AlexState(..), AlexUserState, runAlex) where
}

%wrapper "monadUserState"


$digit = 0-9			-- digits
$Alpha = [a-zA-Z]		-- alphabetic characters
-- ...

tokens :-
  $white+				                    ; -- skip white spaces
  "#".*                                     ; -- skip comments

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

  -- Strings
  $digit+(\.[$digit]+)?                   {newStringToken NumLiteralTkn}  -- Numbers
  $Alpha[a-zA-Z\_0-9]*                          {newStringToken IdTkn}          -- Id
  -- \"([^\\\"\n]|\\\\|\\\"|\\n)*\"       {newStringToken StringTkn}      -- Strings Correctos

  .                                       {invalidCharacter}

{

type Pos = (Int, Int)

data Token =

  YouBeginTkn         { tknPos :: (Int, Int) }  |
  WhereIEndTkn        { tknPos :: (Int, Int) }  |
  IfTkn               { tknPos :: (Int, Int) }  |
  IfYouHaveToAskTkn   { tknPos :: (Int, Int) }  |
  OthersideTkn        { tknPos :: (Int, Int) }  |
  CantStopTkn         { tknPos :: (Int, Int) }  |
  BreakthruTkn        { tknPos :: (Int, Int) }  |
  OneMoreTimeTkn      { tknPos :: (Int, Int) }  |
  ToTkn               { tknPos :: (Int, Int) }  |
  ReadMyMindTkn       { tknPos :: (Int, Int) }  |
  GoTkn               { tknPos :: (Int, Int) }  |
  GoSlowlyTkn         { tknPos :: (Int, Int) }  |
  NewOrderTkn         { tknPos :: (Int, Int) }  |
  DaFunkTkn           { tknPos :: (Int, Int) }  |
  GetBackTkn          { tknPos :: (Int, Int) }  |
  IntoTheVoidTkn      { tknPos :: (Int, Int) }  |
  NewLifeTkn          { tknPos :: (Int, Int) }  |
  SaveMeTkn           { tknPos :: (Int, Int) }  |
  KeepAliveTkn        { tknPos :: (Int, Int) }  |
  AmnesiacTkn         { tknPos :: (Int, Int) }  |
  ExitMusicTkn        { tknPos :: (Int, Int) }  |
  AroundTheWorldTkn   { tknPos :: (Int, Int) }  |
  HoleInMySoulTkn     { tknPos :: (Int, Int) }  |
  IntTkn              { tknPos :: (Int, Int) }  |
  FloatTkn            { tknPos :: (Int, Int) }  |
  CharTkn             { tknPos :: (Int, Int) }  |
  BooleanTkn          { tknPos :: (Int, Int) }  |
  TrueTkn             { tknPos :: (Int, Int) }  |
  FalseTkn            { tknPos :: (Int, Int) }  |
  ArrayStartTkn       { tknPos :: (Int, Int) }  |
  ArrayEndTkn         { tknPos :: (Int, Int) }  |
  BandTkn             { tknPos :: (Int, Int) }  |
  UnionTkn            { tknPos :: (Int, Int) }  |
  PointerTkn          { tknPos :: (Int, Int) }  |
  DuetsTkn            { tknPos :: (Int, Int) }  |
  LeftTkn             { tknPos :: (Int, Int) }  |
  RightTkn            { tknPos :: (Int, Int) }  |
  ModTkn              { tknPos :: (Int, Int) }  |
  DivTkn              { tknPos :: (Int, Int) }  |
  NotTkn              { tknPos :: (Int, Int) }  |
  AndTkn              { tknPos :: (Int, Int) }  |
  OrTkn               { tknPos :: (Int, Int) }  |
  CommaTkn            { tknPos :: (Int, Int) }  |
  ParenOpenTkn        { tknPos :: (Int, Int) }  |
  ParenCloseTkn       { tknPos :: (Int, Int) }  |
  SemicolonTkn        { tknPos :: (Int, Int) }  |
  PlusTkn             { tknPos :: (Int, Int) }  |
  EqualTkn            { tknPos :: (Int, Int) }  |
  ProductTkn          { tknPos :: (Int, Int) }  |
  MinusTkn            { tknPos :: (Int, Int) }  |
  RestTkn             { tknPos :: (Int, Int) }  |
  DivExacTkn          { tknPos :: (Int, Int) }  |
  DifTkn              { tknPos :: (Int, Int) }  |
  GreaterEqualTkn     { tknPos :: (Int, Int) }  |
  LessEqualTkn        { tknPos :: (Int, Int) }  |
  GreaterTkn          { tknPos :: (Int, Int) }  |
  LessTkn             { tknPos :: (Int, Int) }  |
  TypeTkn             { tknPos :: (Int, Int) }  |
  AssignTkn           { tknPos :: (Int, Int) }  |
  NumLiteralTkn       { tknPos :: (Int, Int), tknString :: String } |
  IdTkn               { tknPos :: (Int, Int), tknString :: String }
  -- StringTkn        { tknPos :: (Int, Int), tknString :: String }

  deriving Show



alexEOF :: Alex [Token]
alexEOF = return []

--------- User State
data AlexUserState = AlexUserState {invalidC :: [(Char, Pos)]} deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState []

pushInvalidC :: Char -> Pos -> Alex ()
pushInvalidC c pos = do
  ust <- alexGetUserState
  alexSetUserState ust{invalidC = (c,pos):invalidC ust}



getPos :: AlexInput -> Pos
getPos ((AlexPn _ line col), _, _, _) = (line, col)

getCurrentInput :: AlexInput -> String
getCurrentInput (_, _, _, s) = s




newToken :: (Pos -> Token) -> AlexAction [Token]
newToken tknConstr = \alexIn _ -> do
  let pos = getPos alexIn
  tokens <- alexMonadScan
  return $ (tknConstr pos):tokens


newStringToken :: (Pos -> String -> Token) -> AlexAction [Token]
newStringToken tknConstr = \alexIn len -> do
  let pos = getPos alexIn
      s = take len $ getCurrentInput alexIn
  tokens <- alexMonadScan
  return $ (tknConstr pos s):tokens


invalidCharacter :: AlexAction [Token]
invalidCharacter = \alexIn _ -> do
  let pos = getPos alexIn
      c = head $ getCurrentInput alexIn
  pushInvalidC c pos
  alexMonadScan


}
