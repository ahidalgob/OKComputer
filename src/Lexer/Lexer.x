{
module Lexer(alexMonadScan, alexGetToken) where
import Tokens
import ParseMonad
import LowLevelAlex
import Control.Monad.Except
}


$digit = 0-9            -- digits
$Alpha = [a-zA-Z]       -- alphabetic characters
-- ...

tokens :-
<0> \n                                       {newToken NewLineTkn}
<0>  $white+                                    ; -- skip white spaces
<0>  "#".*                                      ; -- skip comments

  -- Instructions
<0>  youbegin                                {newToken YouBeginTkn}        -- Block Start
<0>  whereiend                               {newToken WhereIEndTkn}       -- Block End
<0>  if                                      {newToken IfTkn}              -- Selection
<0>  ifyouhavetoask                          {newToken IfYouHaveToAskTkn}  -- Selection
<0>  otherside                               {newToken OthersideTkn}       -- Selection
<0>  cantstop                                {newToken CantStopTkn}        -- While Iteration
<0>  breakthru                               {newToken BreakthruTkn}       -- Break
<0>  onemoretime                             {newToken OneMoreTimeTkn}     -- For Iteration
<0>  \;                                      {newToken SemiColonTkn}       -- For Iteration
<0>  readmymind                              {newToken ReadMyMindTkn}      -- Data entry/read
<0>  go                                      {newToken GoTkn}              -- Data exit/write
<0>  gomental                                {newToken GoMentalTkn}        -- Data exit/write without blanks
<0>  goslowly                                {newToken GoSlowlyTkn}        -- Data exit/writeln
<0>  dafunk                                  {newToken DaFunkTkn}          -- Method with return/Function
<0>  \:                                      {newToken ColonTkn}           -- Method with return/Function
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
<0>         \"                                          {beginString}         --"
<string>    ([^\"] | \n)                                {addCharToString}     --"
<string>    \"                                          {endString}           --"


  .                                           {invalidCharacter}

{







alexEOF :: ParseM Token
alexEOF = return EOFTkn


alexGetToken :: ParseM Token
alexGetToken = do
  inp__ <- getAlexInput
  sc <- getAlexStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> throwError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp__' _len -> do
        setAlexInput inp__'
        alexGetToken
    AlexToken inp__' len action -> do
        setAlexInput inp__'
        action (ignorePendingBytes inp__) len

alexMonadScan :: ParseM [Token]
alexMonadScan = do
  tkn <- alexGetToken
  case tkn of
       EOFTkn -> return []
       _ -> (tkn:) <$> alexMonadScan



-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Int -> ParseM result

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip _input _len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code _input _len = do setAlexStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input__ len = do
  setAlexStartCode code
  action input__ len

token :: (AlexInput -> Int -> token) -> AlexAction token
token t input__ len = return (t input__ len)




------------------------------------------------------------
------------------------- End of Alex Required functions









newToken :: (Pos -> Token) -> AlexAction Token
newToken tknConstr = \alexIn _ -> return $ tknConstr $ getPos alexIn

newStringToken :: (Pos -> String -> Token) -> AlexAction Token
newStringToken tknConstr = \alexIn len -> do
  let pos = getPos alexIn
      s = take len $ getCurrentInput alexIn
  return $ tknConstr pos s

beginString :: AlexAction Token
beginString = \alexIn _ -> do
  setAlexStartCode 1 -- ???
  setStrPos $ getPos alexIn
  alexGetToken

addCharToString :: AlexAction Token
addCharToString = \alexIn _ -> do
  pushStrC (head $ getCurrentInput alexIn)
  alexGetToken

endString :: AlexAction Token
endString = \_ _ -> do
  s <- getAndClearStr
  pos <- getStrPos
  setAlexStartCode 0
  return $ StringTkn pos s

invalidCharacter :: AlexAction Token
invalidCharacter = \alexIn _ -> do
  let pos = getPos alexIn
      c = head $ getCurrentInput alexIn
  pushInvalidC c pos
  alexGetToken



}
