{
module Lexer(alexMonadScan, alexGetToken) where
import Tokens
import ParseMonad
import LowLevelAlex
import Control.Monad.Except
}

$ignore = [\ \t] -- check against [\ \t\n\f\v\r].
$digit = 0-9            -- digits
$Alpha = [a-zA-Z]       -- alphabetic characters
$alpha = [a-z]       -- alphabetic characters
$ALPHA = [A-Z]       -- alphabetic characters
-- ...

tokens :-
<0> \n                                       {newLine}
<0>  $ignore+                                   ; -- skip white spaces
<0>  "#".*                                      ; -- skip comments

  -- Instructions
<0>  typedef                                 {newToken TypedefTkn}
<0>  youbegin                                {newToken YouBeginTkn}        -- Block Start
<0>  whereiend                               {newToken WhereIEndTkn}       -- Block End
<0>  if                                      {newToken IfTkn}              -- Selection
<0>  ifyouhavetoask                          {newToken IfYouHaveToAskTkn}  -- Selection
<0>  otherside                               {newToken OthersideTkn}       -- Selection
<0>  cantstop                                {newToken CantStopTkn}        -- While Iteration
<0>  breakthru                               {newToken BreakthruTkn}       -- Break
<0>  onemoretime                             {newToken OneMoreTimeTkn}     -- For Iteration
<0>  \.                                      {newToken DotTkn}
<0>  \.\_                                    {newToken TupleAccessTkn}
<0>  \{                                      {newToken OpenBraceTkn}
<0>  \}                                      {newToken CloseBraceTkn}
<0>  List                                    {newToken ListTypeTkn}
<0>  \+\+                                    {newToken ConcatTkn}
<0>  Tuple                                   {newToken TupleTypeTkn}
<0>  \<\<                                    {newToken OpenTupleTkn}
<0>  \>\>                                    {newToken CloseTupleTkn}
<0>  \;                                      {newToken SemiColonTkn}       -- For Iteration
<0>  readmymind                              {newToken ReadMyMindTkn}      -- Data entry/read
<0>  go                                      {newToken GoTkn}              -- Data exit/write
<0>  gomental                                {newToken GoMentalTkn}        -- Data exit/write without blanks
<0>  goslowly                                {newToken GoSlowlyTkn}        -- Data exit/writeln
<0>  dafunk                                  {newToken DaFunkTkn}          -- Method with return/Function
<0>  \:                                      {newToken ColonTkn}           -- Method with return/Function
<0>  getback                                 {newToken GetBackTkn}         -- Return
<0>  Intothevoid                             {newToken IntoTheVoidTkn}     -- Void
<0>  newlife                                 {newToken NewLifeTkn}         -- Calloc
<0>  saveme                                  {newToken SaveMeTkn}          -- Malloc
<0>  keepyourselfalive                       {newToken KeepAliveTkn}       -- Realloc
<0>  amnesiac                                {newToken AmnesiacTkn}        -- Free
<0>  exitmusic                               {newToken ExitMusicTkn}       -- Exit
<0>  aroundtheworld                          {newToken AroundTheWorldTkn}  -- Import
<0>  holeinmysoul                            {newToken HoleInMySoulTkn}    -- Templates

  -- Type Tokens
<0>  Int                                     {newToken IntTkn}
<0>  Float                                   {newToken FloatTkn}
<0>  Char                                    {newToken CharTkn}
<0>  Boolean                                 {newToken BooleanTkn}
<0>  String                                  {newToken StringTkn}
<0>  ok                                      {newToken OkTkn}              -- True
<0>  notok                                   {newToken NotOkTkn}           -- False
<0>  \[                                      {newToken ArrayStartTkn}
<0>  \]                                      {newToken ArrayEndTkn}
<0>  Record                                  {newToken RecordTkn}
<0>  Union                                   {newToken UnionTkn}
<0>  \^                                      {newToken PointerTkn}         -- Pointers

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
<0>  \'.\'                                   {newCharTkn}

  -- String Tokens
<0>         $digit+                                     {newStringToken IntLiteralTkn}  -- Numbers
<0>         $digit+(\.[$digit]+)?                       {newStringToken FloatLiteralTkn}  -- Numbers
<0>         $alpha[a-zA-Z\_0-9]*                        {newStringToken VarIdTkn}          -- Id
<0>         $ALPHA[a-zA-Z\_0-9]*                        {newStringToken TypeIdTkn}          -- Id
<0>         \"                                          {beginString}         --"
<string>    ([^\"] | \n)                                {addCharToString}     --"
<string>    \"                                          {endString}           --"


  .                                          {invalidCharacter}

{







alexEOF :: ParseM Token
alexEOF = return EOFTkn


alexGetToken :: ParseM Token
alexGetToken = do
  inp__ <- getAlexInput
  sc <- getAlexStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                                                    -- this should never ever happen
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



type AlexAction result = AlexInput -> Int -> ParseM result


------------------------------------------------------------
------------------------- End of Alex Required functions

newToken :: (Pos -> Token) -> AlexAction Token
newToken tknConstr = \alexIn _ -> do
  let pos = getPos alexIn
  --liftIO $ print $ tknConstr pos
  setLastNewLine False
  return $ tknConstr pos

newCharTkn :: AlexAction Token
newCharTkn = \alexIn _ -> do
  let pos = getPos alexIn
      c = (getCurrentInput alexIn)!!1
  --liftIO $ print $ tknConstr pos
  setLastNewLine False
  return $ LiteralCharTkn pos c

newStringToken :: (Pos -> String -> Token) -> AlexAction Token
newStringToken tknConstr = \alexIn len -> do
  let pos = getPos alexIn
      s = take len $ getCurrentInput alexIn
  setLastNewLine False
  return $ tknConstr pos s

newLine :: AlexAction Token
newLine = \alexIn _ -> do
  lastnl <- getLastNewLine
  case lastnl of
    False -> do let pos = getPos alexIn
                --liftIO $ print (NewLineTkn pos)
                setLastNewLine True
                return $ NewLineTkn pos
    True -> alexGetToken

-- No idea what this was
--newEndTkn :: AlexAction Token
--newEndTkn = \alexIn _ -> do
  --let pos = getPos alexIn
  ----liftIO $ print $ tknConstr pos
  --setLastNewLine True
  --return $ WhereIEndTkn pos

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
  setLastNewLine False
  return $ LiteralStringTkn pos s

invalidCharacter :: AlexAction Token
invalidCharacter = \alexIn _ -> do
  let pos = getPos alexIn
      c = head $ getCurrentInput alexIn
  pushInvalidC c pos
  alexGetToken



}
