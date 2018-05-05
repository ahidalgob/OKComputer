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

  -- Strings
  -- $digit+(\.[$digit]+)?                   {newTokenWithString NumLiteralTkn} -- Cadenas de Digitos  
  -- [a-z][a-zA-Z\_0-9]*                    {newTokenWithString IdTkn} -- Identificadores
  -- [a-z][a-zA-Z\_0-9]*\(                  {newFuncIdTkn} -- Identificadores de funciones
  -- \"([^\\\"\n]|\\\\|\\\"|\\n)*\"              {newTokenWithString StringTkn} -- Strings Correctos

{

type Pos = (Int, Int)

data Token =

  YouBeginTkn         { TknPos :: (Int, Int) }  |
  WhereIEndTkn        { TknPos :: (Int, Int) }  |
  IfTkn               { TknPos :: (Int, Int) }  |
  IfYouHaveToAskTkn   { TknPos :: (Int, Int) }  |
  OthersideTkn        { TknPos :: (Int, Int) }  |
  CantStopTkn         { TknPos :: (Int, Int) }  |
  BreakthruTkn        { TknPos :: (Int, Int) }  |
  OneMoreTimeTkn      { TknPos :: (Int, Int) }  |
  ToTkn               { TknPos :: (Int, Int) }  |
  ReadMyMindTkn       { TknPos :: (Int, Int) }  |
  GoTkn               { TknPos :: (Int, Int) }  |
  GoSlowlyTkn         { TknPos :: (Int, Int) }  |
  NewOrderTkn         { TknPos :: (Int, Int) }  |
  DaFunkTkn           { TknPos :: (Int, Int) }  |
  GetBackTkn          { TknPos :: (Int, Int) }  |
  IntoTheVoidTkn      { TknPos :: (Int, Int) }  |
  NewLifeTkn          { TknPos :: (Int, Int) }  |
  SaveMeTkn           { TknPos :: (Int, Int) }  |
  KeepAliveTkn        { TknPos :: (Int, Int) }  |
  AmnesiacTkn         { TknPos :: (Int, Int) }  |
  ExitMusicTkn        { TknPos :: (Int, Int) }  |
  AroundTheWorldTkn   { TknPos :: (Int, Int) }  |
  HoleInMySoulTkn     { TknPos :: (Int, Int) }  |
  IntTkn              { TknPos :: (Int, Int) }  |
  FloatTkn            { TknPos :: (Int, Int) }  |
  CharTkn             { TknPos :: (Int, Int) }  |
  BooleanTkn          { TknPos :: (Int, Int) }  |
  TrueTkn             { TknPos :: (Int, Int) }  |
  FalseTkn            { TknPos :: (Int, Int) }  |
  ArrayStartTkn       { TknPos :: (Int, Int) }  |
  ArrayEndTkn         { TknPos :: (Int, Int) }  |
  BandTkn             { TknPos :: (Int, Int) }  |
  UnionTkn            { TknPos :: (Int, Int) }  |
  PointerTkn          { TknPos :: (Int, Int) }  |
  DuetsTkn            { TknPos :: (Int, Int) }  |
  LeftTkn             { TknPos :: (Int, Int) }  |
  RightTkn            { TknPos :: (Int, Int) }  |
  ModTkn              { TknPos :: (Int, Int) }  |
  DivTkn              { TknPos :: (Int, Int) }  |
  NotTkn              { TknPos :: (Int, Int) }  |
  AndTkn              { TknPos :: (Int, Int) }  |
  OrTkn               { TknPos :: (Int, Int) }  |
  CommaTkn            { TknPos :: (Int, Int) }  |
  ParenOpenTkn        { TknPos :: (Int, Int) }  |
  ParenCloseTkn       { TknPos :: (Int, Int) }  |
  SemicolonTkn        { TknPos :: (Int, Int) }  |
  PlusTkn             { TknPos :: (Int, Int) }  |
  EqualTkn            { TknPos :: (Int, Int) }  |
  ProductTkn          { TknPos :: (Int, Int) }  |
  MinusTkn            { TknPos :: (Int, Int) }  |
  RestTkn             { TknPos :: (Int, Int) }  |
  DivExacTkn          { TknPos :: (Int, Int) }  |
  DifTkn              { TknPos :: (Int, Int) }  |
  GreaterEqualTkn     { TknPos :: (Int, Int) }  |
  LessEqualTkn        { TknPos :: (Int, Int) }  |
  GreaterTkn          { TknPos :: (Int, Int) }  |
  LessTkn             { TknPos :: (Int, Int) }  |
  TypeTkn             { TknPos :: (Int, Int) }  |
  AssignTkn           { TknPos :: (Int, Int) }
  -- NumLiteralTkn    { TknPos :: (Int, Int), TknString :: String }
  -- IdTkn            { TknPos :: (Int, Int), TknString :: String }
  -- FuncIdTkn        { TknPos :: (Int, Int), TknString :: String }
  -- StringTkn        { TknPos :: (Int, Int), TknString :: String }

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
