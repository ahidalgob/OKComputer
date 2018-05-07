{
module Lexer(Token(..), Alex(Alex), alexMonadScan, AlexState(..), AlexUserState, runAlex) where
}

%wrapper "monadUserState"


$digit = 0-9            -- digits
$Alpha = [a-zA-Z]       -- alphabetic characters
-- ...

tokens :-
<0>  $white+                                    ; -- skip white spaces
<0>  "#".*                                      ; -- skip comments

  -- Instructions
<0>  maintheme                               {newToken MainThemeTkn}       -- File Start
<0>  youbegin                                {newToken YouBeginTkn}        -- Block Start
<0>  whereiend                               {newToken WhereIEndTkn}       -- Block End
<0>  if                                      {newToken IfTkn}              -- Selection
<0>  ifyouhavetoask                          {newToken IfYouHaveToAskTkn}  -- Selection
<0>  otherside                               {newToken OthersideTkn}       -- Selection
<0>  cantstop                                {newToken CantStopTkn}        -- While Iteration
<0>  breakthru                               {newToken BreakthruTkn}       -- Break
<0>  onemoretime                             {newToken OneMoreTimeTkn}     -- For Iteration
<0>  to                                      {newToken ToTkn}              -- For Iteration
<0>  readmymind                              {newToken ReadMyMindTkn}      -- Data entry/read
<0>  go                                      {newToken GoTkn}              -- Data exit/write
<0>  goslowly                                {newToken GoSlowlyTkn}        -- Data exit/writeln
<0>  neworder                                {newToken NewOrderTkn}        -- Method/Proc
<0>  dafunk                                  {newToken DaFunkTkn}          -- Method with return/Function
<0>  getback                                 {newToken GetBackTkn}         -- Return
<0>  intothevoid                             {newToken IntoTheVoidTkn}     -- Void
<0>  newlife                                 {newToken NewLifeTkn}         -- Calloc
<0>  saveme                                  {newToken SaveMeTkn}          -- Malloc
<0>  keepyourselfalive                       {newToken KeepAliveTkn}       -- Realloc
<0>  amnesiac                                {newToken AmnesiacTkn}        -- Free
<0>  exitmusic                               {newToken ExitMusicTkn}       -- Exit
<0>  aroundtheworld                          {newToken AroundTheWorldTkn}  -- Import
<0>  holeinmysoul                            {newToken HoleInMySoulTkn}    -- Templates

  -- Type Tokens
<0>  int                                     {newToken IntTkn}
<0>  float                                   {newToken FloatTkn}
<0>  char                                    {newToken CharTkn}
<0>  boolean                                 {newToken BooleanTkn}
<0>  ok                                      {newToken OkTkn}              -- True
<0>  notok                                   {newToken NotOkTkn}           -- False
<0>  \[                                      {newToken ArrayStartTkn}
<0>  \]                                      {newToken ArrayEndTkn}
<0>  band                                    {newToken BandTkn}            -- Registers/structs
<0>  union                                   {newToken UnionTkn}
<0>  \&                                      {newToken PointerTkn}         -- Pointers
<0>  duets                                   {newToken DuetsTkn}           -- Tuple
<0>  left                                    {newToken LeftTkn}
<0>  right                                   {newToken RightTkn}

  -- Operations Tokens
<0>  mod                                     {newToken ModTkn}
<0>  div                                     {newToken DivTkn}
<0>  not                                     {newToken NotTkn}
<0>  and                                     {newToken AndTkn}
<0>  or                                      {newToken OrTkn}
<0>  \,                                      {newToken CommaTkn}
<0>  \(                                      {newToken ParenOpenTkn}
<0>  \)                                      {newToken ParenCloseTkn}
<0>  \;                                      {newToken SemicolonTkn}
<0>  \+                                      {newToken PlusTkn}
<0>  \=\=                                    {newToken EqualTkn}
<0>  \*                                      {newToken ProductTkn}
<0>  \-                                      {newToken MinusTkn}
<0>  \%                                      {newToken RestTkn}
<0>  \/                                      {newToken DivExacTkn}
<0>  \!\=                                    {newToken DifTkn}
<0>  \>\=                                    {newToken GreaterEqualTkn}
<0>  \<\=                                    {newToken LessEqualTkn}
<0>  \>                                      {newToken GreaterTkn}
<0>  \<                                      {newToken LessTkn}
<0>  \-\>                                    {newToken TypeTkn}
<0>  \=                                      {newToken AssignTkn}

  -- String Tokens
<0>         $digit+(\.[$digit]+)?                       {newStringToken NumLiteralTkn}  -- Numbers
<0>         $Alpha[a-zA-Z\_0-9]*                        {newStringToken IdTkn}          -- Id
<0>         \"                                          {beginString}
<string>    ([^\"] | \n)                                {addCharToString}
<string>    \"                                          {endString}

  -- \"([^\\\"\n]|\\\\|\\\"|\\n)*\"             {newStringToken StringTkn}      -- Strings Correctos

  .                                             {invalidCharacter}

{

type Pos = (Int, Int)

data Token =

  MainThemeTkn        { tknPos :: (Int, Int) }  |
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
  OkTkn               { tknPos :: (Int, Int) }  |
  NotOkTkn            { tknPos :: (Int, Int) }  |
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
  IdTkn               { tknPos :: (Int, Int), tknString :: String } |
  StringTkn        { tknPos :: (Int, Int), tknString :: String }

  deriving Show



alexEOF :: Alex [Token]
alexEOF = return []

--------- User State
data AlexUserState = AlexUserState {
                        invalidC :: [(Char, Pos)],
                        strPos :: Pos,
                        str :: String
                        } deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState{invalidC=[], strPos=(0,0), str=""}

pushStrC :: Char -> Alex ()
pushStrC c = do
  ust <- alexGetUserState
  alexSetUserState ust{str = c:str ust}

getAndClearStr :: Alex String
getAndClearStr = do
  ust <- alexGetUserState
  let s = str ust
  alexSetUserState ust{str=""}
  return $ reverse s

setStrPos :: Pos -> Alex ()
setStrPos pos = do
  ust <- alexGetUserState
  alexSetUserState ust{strPos = pos}

getStrPos :: Alex Pos
getStrPos = do
  ust <- alexGetUserState
  return $ strPos ust



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
  (tknConstr pos :) <$> alexMonadScan


newStringToken :: (Pos -> String -> Token) -> AlexAction [Token]
newStringToken tknConstr = \alexIn len -> do
  let pos = getPos alexIn
      s = take len $ getCurrentInput alexIn
  (tknConstr pos s :) <$> alexMonadScan

beginString :: AlexAction [Token]
beginString = \alexIn _ -> do
  alexSetStartCode 1 -- ???
  setStrPos $ getPos alexIn
  alexMonadScan

addCharToString = \alexIn _ -> do
  pushStrC (head $ getCurrentInput alexIn)
  alexMonadScan

endString = \alexIn _ -> do
  s <- getAndClearStr
  pos <- getStrPos
  alexSetStartCode 0
  (StringTkn pos s :) <$> alexMonadScan

invalidCharacter :: AlexAction [Token]
invalidCharacter = \alexIn _ -> do
  let pos = getPos alexIn
      c = head $ getCurrentInput alexIn
  pushInvalidC c pos
  alexMonadScan


}
