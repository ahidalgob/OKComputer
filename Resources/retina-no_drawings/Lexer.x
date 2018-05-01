-- Retina - Proyecto de Traductores
-- Lexer
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708



{
module Lexer(Token(..), printToken, runAlexScan, AlexUserState(..)) where
}

%wrapper "monadUserState"

$digit = 0-9			-- digits
$Alpha = [a-zA-Z]		-- alphabetic characters
$alpha = [a-z]		    -- alphabetic characters
$ALPHA = [A-Z]		    -- alphabetic characters
$sim = [\=\+\-\*\/\/\=\%\=\=\<\>\>\=\<\=]

tokens :-
    $white+				                    ; --Desechar los espacios en blanco
    "#".*				                    ; --Desechar los comentarios



    --Tokens de palabras reservadas:
    while                                   {pushToken WhileTK}
    while\(                                 {pushTokenWithOpenPar WhileTK}
    for                                     {pushToken ForTK}
    from                                    {pushToken FromTK}
    to                                      {pushToken ToTK}
    by                                      {pushToken ByTK}
    repeat                                  {pushToken RepeatTK}
    func                                    {pushToken FuncTK}
    return                                  {pushToken ReturnTK}
    return\(                                {pushTokenWithOpenPar ReturnTK}
    begin                                   {pushToken BeginTK}
    program                                 {pushToken ProgramTK}
    with                                    {pushToken WithTK}
    do                                      {pushToken DoTK}
    end                                     {pushToken EndTK}
    times                                   {pushToken TimesTK}
    not                                     {pushToken NotTK}
    not\(                                   {pushTokenWithOpenPar NotTK}
    and                                     {pushToken AndTK}
    and\(                                   {pushTokenWithOpenPar AndTK}
    or                                      {pushToken OrTK}
    or\(                                    {pushTokenWithOpenPar OrTK}
    read                                    {pushToken ReadTK}
    read\(                                  {pushTokenWithOpenPar ReadTK}
    write                                   {pushToken WriteTK}
    write\(                                 {pushTokenWithOpenPar WriteTK}
    writeln                                 {pushToken WritelnTK}
    writeln\(                               {pushTokenWithOpenPar WritelnTK}
    if                                      {pushToken IfTK}
    if\(                                    {pushTokenWithOpenPar IfTK}
    then                                    {pushToken ThenTK}
    else                                    {pushToken ElseTK}
    number                                  {pushToken NumberTK}
    boolean                                 {pushToken BooleanTK}
    true                                    {pushToken TrueTK}
    false                                   {pushToken FalseTK}
    mod                                     {pushToken ModTK}
    mod\(                                   {pushTokenWithOpenPar ModTK}
    div                                     {pushToken DivTK}
    div\(                                   {pushTokenWithOpenPar DivTK}
    \,                                      {pushToken CommaTK}
    \(                                      {pushToken ParenOpenTK}
    \)                                      {pushToken ParenCloseTK}
    \;                                      {pushToken SemicolonTK}
    \+                                      {pushToken PlusTK}
    \=\=                                    {pushToken EqualTK}
    \*                                      {pushToken ProductTK}
    \-                                      {pushToken MinusTK}
    \%                                      {pushToken RestTK}
    \/                                      {pushToken DivExacTK}
    \/\=                                    {pushToken DifTK}
    \>\=                                    {pushToken GreaterEqualTK}
    \<\=                                    {pushToken LessEqualTK}
    \>                                      {pushToken GreaterTK}
    \<                                      {pushToken LessTK}
    \-\>                                    {pushToken TypeTK}
    \=                                      {pushToken AssignTK}
    $digit+(\.[$digit]+)?	                {pushTokenWithString NumLiteralTK} -- Cadenas de Digitos
    [$digit \.]+                            {pushInvalid InvalidTK} -- Invalid Numbers
    [a-z][a-zA-Z\_0-9]*	            	    {pushTokenWithString IdTK} -- Identificadores
    [a-z][a-zA-Z\_0-9]*\(	            	{pushFuncIdTK} -- Identificadores de funciones
    [$Alpha $digit \_]+                     {pushInvalid InvalidTK} -- Invalid Id
    \"([^\\\"\n]|\\\\|\\\"|\\n)*\"              {pushTokenWithString StringTK} -- Strings Correctos
    \"([^\"\n]|\\\"|\\n)*\"                     {pushInvalid InvalidTK} -- Invalid Strings
    \"([^\"\n]|\\\"|\\n)*                       {pushInvalid InvalidTK} -- Invalid Strings
    [a-zA-Z\_0-9]*\"([^\\\"\n]|\\\\|\\\"|\\n)*\"[a-zA-Z\_0-9]*  {pushInvalid InvalidTK} -- Invalid Strings
    .                                       {pushInvalid InvalidTK} -- npi
{


-- The token type:
data Token =
    WhileTK { tokenPos :: (Int, Int) }                               |
    ForTK { tokenPos :: (Int, Int) }                                 |
    FromTK { tokenPos :: (Int, Int) }                                |
    ToTK { tokenPos :: (Int, Int) }                                  |
    ByTK { tokenPos :: (Int, Int) }                                  |
    BeginTK { tokenPos :: (Int, Int) }                               |
    FuncTK { tokenPos :: (Int, Int) }                                |
    ReturnTK {tokenPos :: (Int, Int)}                                |
    RepeatTK { tokenPos :: (Int, Int) }                              |
    ProgramTK { tokenPos :: (Int, Int) }                             |
    WithTK { tokenPos :: (Int, Int) }                                |
    DoTK { tokenPos :: (Int, Int) }                                  |
    EndTK { tokenPos :: (Int, Int) }                                 |
    TimesTK { tokenPos :: (Int, Int) }                               |
    NotTK { tokenPos :: (Int, Int) }                                 |
    AndTK { tokenPos :: (Int, Int) }                                 |
    OrTK { tokenPos :: (Int, Int) }                                  |
    ReadTK { tokenPos :: (Int, Int) }                                |
    WriteTK { tokenPos :: (Int, Int) }                               |
    WritelnTK { tokenPos :: (Int, Int) }                             |
    IfTK { tokenPos :: (Int, Int) }                                  |
    ThenTK { tokenPos :: (Int, Int) }                                |
    ElseTK { tokenPos :: (Int, Int) }                                |
    NumberTK { tokenPos :: (Int, Int) }                              |
    BooleanTK { tokenPos :: (Int, Int) }                             |
    TrueTK { tokenPos :: (Int, Int) }                                |
    FalseTK { tokenPos :: (Int, Int) }                               |
    DivTK { tokenPos :: (Int, Int) }                                 |
    ModTK { tokenPos :: (Int, Int) }                                 |
    CommaTK { tokenPos :: (Int, Int) }                               |
    PlusTK { tokenPos :: (Int, Int) }                                |
    EqualTK { tokenPos :: (Int, Int) }                               |
    ProductTK { tokenPos :: (Int, Int) }                             |
    MinusTK { tokenPos :: (Int, Int) }                               |
    RestTK { tokenPos :: (Int, Int) }                                |
    DivExacTK { tokenPos :: (Int, Int) }                             |
    DifTK  { tokenPos :: (Int, Int) }                                |
    GreaterEqualTK { tokenPos :: (Int, Int) }                        |
    LessEqualTK { tokenPos :: (Int, Int) }                           |
    GreaterTK { tokenPos :: (Int, Int) }                             |
    LessTK { tokenPos :: (Int, Int) }                                |
    AssignTK { tokenPos :: (Int, Int) }                              |
    ParenOpenTK { tokenPos :: (Int, Int) }                           |
    ParenCloseTK { tokenPos :: (Int, Int) }                          |
    SemicolonTK { tokenPos :: (Int, Int) }                           |
    TypeTK { tokenPos :: (Int, Int) }                                |
    NumLiteralTK { tokenPos :: (Int, Int), tokenString :: String}    |
    StringTK { tokenPos :: (Int, Int), tokenString :: String}        |
    IdTK { tokenPos :: (Int, Int), tokenString :: String}            |
    FuncIdTK { tokenPos :: (Int, Int), tokenString :: String}        |
    InvalidTK { tokenPos :: (Int, Int), tokenString :: String}
    deriving (Eq)

instance Show Token where
    show (WhileTK _)        = "palabra reservada 'while'"
    show (ForTK _)          = "palabra reservada 'for'"
    show (FromTK _)         = "palabra reservada 'from'"
    show (ToTK _)           = "palabra reservada 'to'"
    show (ByTK _)           = "palabra reservada 'by'"
    show (BeginTK _)        = "palabra reservada 'begin'"
    show (FuncTK _)         = "palabra reservada 'func'"
    show (ReturnTK _)       = "palabra reservada 'return'"
    show (RepeatTK _)       = "palabra reservada 'repeat'"
    show (ProgramTK _)      = "palabra reservada 'program'"
    show (WithTK _)         = "palabra reservada 'with'"
    show (DoTK _)           = "palabra reservada 'do'"
    show (EndTK _)          = "palabra reservada 'end'"
    show (TimesTK _)        = "palabra reservada 'times'"
    show (NotTK _)          = "palabra reservada 'not'"
    show (AndTK _)          = "palabra reservada 'and'"
    show (OrTK _)           = "palabra reservada 'or'"
    show (ReadTK _)         = "palabra reservada 'read'"
    show (WriteTK _)        = "palabra reservada 'write'"
    show (WritelnTK _)      = "palabra reservada 'writeln'"
    show (IfTK _)           = "palabra reservada 'if'"
    show (ThenTK _)         = "palabra reservada 'then'"
    show (ElseTK _)         = "palabra reservada 'else'"
    show (NumberTK _)       = "tipo de dato 'number'"
    show (BooleanTK _)      = "tipo de dato 'boolean'"
    show (TrueTK _)         = "literal booleano 'true'"
    show (FalseTK _)        = "literal booleano 'false'"
    show (DivTK _)          = "palabra reservada 'div'"
    show (ModTK _)          = "palabra reservada 'mod'"
    show (CommaTK _)        = "signo ','"
    show (PlusTK _)         = "signo '+'"
    show (EqualTK _)        = "signo '=='"
    show (ProductTK _)      = "signo '*'"
    show (MinusTK _)        = "signo '-'"
    show (RestTK _)         = "signo '%'"
    show (DivExacTK _)      = "signo '/'"
    show (DifTK _)          = "signo '/='"
    show (GreaterEqualTK _) = "signo '>='"
    show (LessEqualTK _)    = "signo '<='"
    show (GreaterTK _)      = "signo '>'"
    show (LessTK _)         = "signo '<'"
    show (AssignTK _)       = "signo '='"
    show (ParenOpenTK _)    = "signo '('"
    show (ParenCloseTK _)   = "signo ')'"
    show (SemicolonTK _)    = "signo ';'"
    show (TypeTK _)         = "signo '->'"
    show (NumLiteralTK _ s) = "literal numerico '"++ s ++ "'"
    show (StringTK _ s)     = "literal de string "++ s
    show (IdTK _ s)         = "identificador '"++ s ++ "'"
    show (FuncIdTK _ s)     = "identificador de funcion '"++ s ++ "'"
    show (InvalidTK _ s)    = "input no reconocido '"++ s ++ "'"


printToken tk = putStrLn $ "linea " ++ (show ln) ++", columna " ++ (show cn) ++ ": " ++ show tk
    where
        (ln, cn) = tokenPos tk

alexEOF :: Alex ()
alexEOF = return ()

data AlexUserState = AlexUserState { tokenList :: [Token], invalidTokens :: Bool}
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState [] False

modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \s ->    let current = alex_ust s
                                        new     = f current
                                    in
                                        Right (s { alex_ust = new },())

getUserState ::  Alex AlexUserState
getUserState = Alex $ \s -> Right (s,alex_ust s)

pushValid :: Token -> AlexUserState -> AlexUserState
pushValid tkn ts =  if invalidTokens ts
                        then ts
                        else ts{tokenList = tkn : (tokenList ts)}

alexPosn2Pair (AlexPn _ c l) = (c, l)

pushToken :: ((Int, Int) -> Token) -> AlexAction ()
pushToken tokenizer =
    \(posn,prevChar,pending,s) len -> modifyUserState (pushValid (tokenizer $ alexPosn2Pair posn)) >> alexMonadScan

pushTokenWithString :: ((Int, Int) -> String -> Token) -> AlexAction ()
pushTokenWithString tokenizer =
    \(posn,prevChar,pending,s) len -> modifyUserState (pushValid $ tokenizer (alexPosn2Pair posn) $ take len s) >> alexMonadScan

pushFuncIdTK :: AlexAction ()
pushFuncIdTK =
     \(posn@(AlexPn x ln cn),prevChar,pending,s) len -> do  modifyUserState (pushValid $ FuncIdTK (alexPosn2Pair posn) $ take (len-1) s)
                                                            modifyUserState (pushValid $ ParenOpenTK $ alexPosn2Pair (AlexPn x ln (cn+len-1)))
                                                            alexMonadScan

pushTokenWithOpenPar :: ((Int, Int) -> Token) -> AlexAction ()
pushTokenWithOpenPar tokenizer =
    \(posn@(AlexPn x ln cn),prevChar,pending,s) len -> do   modifyUserState (pushValid $ tokenizer $ alexPosn2Pair posn)
                                                            modifyUserState (pushValid $ ParenOpenTK $ alexPosn2Pair (AlexPn x ln (cn+len-1)))
                                                            alexMonadScan

pushInvalid :: ((Int, Int) -> String -> Token) -> AlexAction ()
pushInvalid tokenizer =
    \(posn,prevChar,pending,s) len -> modifyUserState checkInvalid >> modifyUserState (push (alexPosn2Pair posn) $ take len s) >> alexMonadScan
        where
            push :: (Int, Int) -> String -> AlexUserState -> AlexUserState
            push posn ss ts = ts{tokenList = (tokenizer posn ss):(tokenList ts)}
            checkInvalid :: AlexUserState -> AlexUserState
            checkInvalid ts =   if not $ invalidTokens ts
                                    then AlexUserState [] True
                                    else ts

runAlexScan :: String -> Either String AlexUserState
runAlexScan s = runAlex s $ alexMonadScan >> getUserState

}
