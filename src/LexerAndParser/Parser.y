-- OKComputer
-- Parser

{
module Parser where
import Lexer
import ParseMonad
import Tokens
import Control.Monad.Except
import AST
}

%name parse
%tokentype { Token }
%monad { ParseM }
%error { (throwError . show) }
%lexer { lexwrap }{ EOFTkn }

%token
  youbegin                                { YouBeginTkn $$ }        -- Block Start
  whereiend                               { WhereIEndTkn $$ }       -- Block End
  if                                      { IfTkn $$ }              -- Selection
  ifyouhavetoask                          { IfYouHaveToAskTkn $$ }  -- Selection
  otherside                               { OthersideTkn $$ }       -- Selection
  cantstop                                { CantStopTkn $$ }        -- While Iteration
  breakthru                               { BreakthruTkn $$ }       -- Break
  onemoretime                             { OneMoreTimeTkn $$ }     -- For Iteration
  ';'                                     { SemiColonTkn $$ }       -- For Iteration
  readmymind                              { ReadMyMindTkn $$ }      -- Data entry/read
  go                                      { GoTkn $$ }              -- Data exit/write
  gomental                                { GoMentalTkn $$ }
  goslowly                                { GoSlowlyTkn $$ }        -- Data exit/writeln
  dafunk                                  { DaFunkTkn $$}          -- Method with return/Function
  ':'                                     { ColonTkn $$ }           -- Method with return/Function
  getback                                 { GetBackTkn $$ }         -- Return
  intothevoid                             { IntoTheVoidTkn $$ }     -- Void
  newlife                                 { NewLifeTkn $$ }         -- Calloc
--  saveme                                  { SaveMeTkn }          -- Malloc
--  keepyourselfalive                       { KeepAliveTkn }       -- Realloc
  amnesiac                                { AmnesiacTkn $$ }        -- Free
  exitmusic                               { ExitMusicTkn $$ }       -- Exit
  aroundtheworld                          { AroundTheWorldTkn $$ }  -- Import
--  holeinmysoul                            { HoleInMySoulTkn }    -- Templates

  -- Type Tokens
  int                                     { IntTkn $$ }
  float                                   { FloatTkn $$ }
  char                                    { CharTkn $$ }
  boolean                                 { BooleanTkn $$ }
  ok                                      { OkTkn $$ }              -- True
  notok                                   { NotOkTkn $$ }           -- False
  '{'                                     { OpenBraceTkn $$ }
  '}'                                     { CloseBraceTkn $$ }
  '['                                     { ArrayStartTkn $$ }
  ']'                                     { ArrayEndTkn $$ }
  band                                    { BandTkn $$ }            -- Registers/structs
  union                                   { UnionTkn $$ }
  '.'										{ DotTkn $$ }
  '^'									{ PointerTkn $$ }
  duets                                   { DuetsTkn $$ }           -- Tuple
  left                                    { LeftTkn $$ }
  right                                   { RightTkn $$ }

  -- Operations Tokens
  mod                                     { ModTkn $$ }
  div                                     { DivTkn $$ }
  not                                     { NotTkn $$ }
  and                                     { AndTkn $$ }
  or                                      { OrTkn $$ }
  ','                                     { CommaTkn $$ }
  '('                                     { ParenOpenTkn $$ }
  ')'                                     { ParenCloseTkn $$ }
  '+'                                     { PlusTkn $$ }
  '=='                                    { EqualTkn $$ }
  '*'                                     { ProductTkn $$ }
  '-'                                     { MinusTkn $$ }
  '%'                                     { RestTkn $$ }
  '/'                                     { DivExacTkn $$ }
  '!='                                    { DifTkn $$ }
  '>='                                    { GreaterEqualTkn $$ }
  '<='                                    { LessEqualTkn $$ }
  '>'                                     { GreaterTkn $$ }
  '<'                                     { LessTkn $$ }
 -- '->'                                    { TypeTkn }
  '='                                     { AssignTkn $$ }

  -- Otros
  id                                      { IdTkn _ _ }
  n                                       { NumLiteralTkn _ _ }
  newline                                 { NewLineTkn $$ }
  c                                       { LiteralCharTkn _ _ } -- char
  string                                  { StringTkn _ _ }

-- TODO:
-- duets or tuples in general.
-- Array expression  {1,2,3}
-- char
-- braces
-- string operations
--

%left or
%left and
%nonassoc '>' '<' '==' '!=' '>=' '<='
%left '+' '-'
%left '*' '/' '%' mod div
%nonassoc not
%right '='

-- TODO
-- unary minus sign
-- unary ^
%%
-- Start
START : IMPORTS OUTSIDEFUNCTION         { STARTN $1 $2 }


IMPORTS : IMPORT newline IMPORTS        { (IMPORTN $1):$3  } --TODO Left Recursion
        | {- empty -}                   { [] }

IMPORT : aroundtheworld IDS             { reverse $2 }

IDS : IDS ',' id                             { (IDN $ tknString $3):$1 }
  | id                                       { [IDN $ tknString $1] }


OUTSIDEFUNCTION : FUNCTIONINIC OUTSIDEFUNCTION      { (OUTFUNCTIONINIC $1):$2 }
           | DECLARATION newline OUTSIDEFUNCTION          { (OUTDECLARATION $1):$3 }
           | DEFINESTRUCT newline OUTSIDEFUNCTION         { (OUTDEFINE $1):$3 }
           | {- empty -}                            { [] }


FUNCTIONINIC : dafunk id '(' LPARAMETERSFUNC ')' ':' RETURNTYPE BLOCK    { FUNCTIONINICN (IDN $ tknString $2) $4 $7 $8 }


RETURNTYPE: intothevoid                                                 { INTOTHEVOIDN }
          | TYPE                                                        { RETURNSOMN $1 } --TODO

LPARAMETERSFUNC : {- empty -}                                           { [] }
                | NONEMPTYLPARAMETERSFUNC                               { $1 }

NONEMPTYLPARAMETERSFUNC : FUNCTIONPARAMETER ',' NONEMPTYLPARAMETERSFUNC { $1:($3) } -- TODO
                        | FUNCTIONPARAMETER                             { [$1] }

FUNCTIONPARAMETER : TYPE id                                             { PARAMETERN $1 (tknString $2)}
           -- | TYPE id '[' ']'                                            {% liftIO $ putStrLn "FUNCTIONPARAMETER  -> TYPE id '[' ']'" }


BLOCK : MAYBELINE youbegin MAYBELINE INSIDEFUNCTION whereiend           { BLOCKN $4 }
      --| MAYBELINE INSTRUCTION                                           { BLOCKN }

INSIDEFUNCTION : INSIDEFUNCTION INSTRUCTION                     { INSIDEN }
         | {- empty -}                                                  { INSIDEN }


DECLARATION : TYPE DECLARATIONTYPE { DECLARATIONN $1 $2 } -- TODO

TYPE : TYPE2              { TYPENOPOINTERN $1 }
    |  TYPE2 '^'          { TYPEPOINTERN $1 }

TYPE2 : int                                    { INTN }
   | float                                    { FLOATN }
   | boolean                                  { BOOLEANN }
   | char                                     { CHARN }
   | string                                   { STRINGN }
   | id                                       { IDSTRUCTN $ tknString $1 }

DECLARATIONTYPE : ID '=' EXPRESSION                 { [DECTYPEN1 $1 $3] }
            | ID '=' EXPRESSION ',' DECLARATIONTYPE { (DECTYPEN1 $1 $3):($5) }
            | ID                                    { [DECTYPEN2 $1] }
            | ID ',' DECLARATIONTYPE                { (DECTYPEN2 $1):($3) }

ID : id                                                    { ID2N }
   | id '[' EXPRESSION ']'                                 { ID2N }

-- Probablemente vaya newline antes del youbegin y whereiend PUESTOS
INSTRUCTION : go '(' PRINT ')' newline                                                                                          {% liftIO $ putStrLn "INSTRUCTION -> go '(' PRINT ')' " }
            | goslowly '(' PRINT ')' newline                                                                                    {% liftIO $ putStrLn "INSTRUCTION -> goslowly '(' PRINT ')' " }
            | gomental '(' PRINT ')' newline                                                                                    {% liftIO $ putStrLn "INSTRUCTION -> gomental '(' PRINT ')' " }
            | readmymind '(' IDS ')' newline                                                                                     {% liftIO $ putStrLn "INSTRUCTION -> readmymind '(' id ')' " }
            | amnesiac '(' id ')' newline 																						{ }

            | if EXPRESSION BLOCK IFELSE                                                                                {% liftIO $ putStrLn "INSTRUCTION -> if EXPRESSION BLOCK IFELSE " }
            | cantstop EXPRESSION BLOCK                                                                                 {% liftIO $ putStrLn "INSTRUCTION -> cantstop EXPRESSION BLOCK  " }
            | onemoretime TYPE id '=' EXPRESSION ';' EXPRESSION ';' EXPRESSION BLOCK                                    {% liftIO $ putStrLn "INSTRUCTION -> onemoretime TYPE id '=' EXPRESSION ';' EXPRESSION ';'EXPRESSION BLOCK " }
            | getback EXPRESSION newline                                                                                        {% liftIO $ putStrLn "INSTRUCTION -> getback EXPRESSION " }
            | breakthru newline                                                                                                 {% liftIO $ putStrLn "INSTRUCTION -> breakthru " }
            | exitmusic newline                                                                                                 {% liftIO $ putStrLn "INSTRUCTION -> exitmusic " }
            | DECLARATION newline  {}
            | EXPRESSION newline   {}

IFELSE : ifyouhavetoask EXPRESSION BLOCK IFELSE                                     { % liftIO $ putStrLn "IFELSE -> ifyouhavetoask EXPRESSION BLOCK newline IFELSE " }
       | otherside BLOCK                                                            { % liftIO $ putStrLn "IFELSE -> otherside BLOCK newline " }
       | {- empty -}                                                                        { }

PRINT : string ',' PRINT                     { % liftIO $ putStrLn "PRINT -> string ',' PRINT " }
     | id ','     PRINT                      { % liftIO $ putStrLn "PRINT -> id ','     PRINT " }
     | string                                { % liftIO $ putStrLn "PRINT -> string " }
     | id                                    { % liftIO $ putStrLn "PRINT -> id " }

DEFINESTRUCT : band id '{' newline LDECLARATIONS newline'}'    {DEFINESTRUCTN}
             | union id '{' newline LDECLARATIONS newline '}'   {DEFINESTRUCTN}

LDECLARATIONS : LDECLARATIONS newline DECLARATION  {% liftIO $ putStrLn "LDECLARATIONS -> DECLARATION LDECLARATIONS" }
              | DECLARATION                        {% liftIO $ putStrLn "LDECLARATIONS -> DECLARATION" }


EXPRESSION : id                         { EXPRESSIONN }
           | n                          { EXPRESSIONN }
           | string                     { EXPRESSIONN }
      --   | c                          { % liftIO $ putStrLn "EXPRESSION -> c " }
           | ok                         { EXPRESSIONN }
           | notok                      { EXPRESSIONN }
           | '(' EXPRESSION ')'         { EXPRESSIONN }
           | EXPRESSION '<' EXPRESSION  { EXPRESSIONN }
           | EXPRESSION '>' EXPRESSION  { EXPRESSIONN }
           | EXPRESSION '<=' EXPRESSION { EXPRESSIONN }
           | EXPRESSION '>=' EXPRESSION { EXPRESSIONN }
           | EXPRESSION '==' EXPRESSION { EXPRESSIONN }
           | EXPRESSION '!=' EXPRESSION { EXPRESSIONN }
           | not EXPRESSION             { EXPRESSIONN }
           | EXPRESSION and EXPRESSION  { EXPRESSIONN }
           | EXPRESSION or EXPRESSION   { EXPRESSIONN }
           | '-' EXPRESSION             { EXPRESSIONN }
           | EXPRESSION '+' EXPRESSION  { EXPRESSIONN }
           | EXPRESSION '-' EXPRESSION  { EXPRESSIONN }
           | EXPRESSION '*' EXPRESSION  { EXPRESSIONN }
           | EXPRESSION '/' EXPRESSION  { EXPRESSIONN }
           | EXPRESSION '%' EXPRESSION  { EXPRESSIONN }
           | EXPRESSION mod EXPRESSION  { EXPRESSIONN }
           | EXPRESSION div EXPRESSION  { EXPRESSIONN }
           -- | EXPRESSIONTUPLE            { % liftIO $ putStrLn "EXPRESSION -> EXPRESSIONTUPLE " }
           | ARRAYPOSITION              { EXPRESSIONN }
           | EXPRESSIONSTRUCT           { EXPRESSIONN }
           | FUNCTIONCALL               { EXPRESSIONN }
           | newlife '(' EXPRESSION ')' { EXPRESSIONN }
           | '^' id                     { EXPRESSIONN }
           | id '=' EXPRESSION          { EXPRESSIONN }

           {-
EXPRESSIONTUPLE : left id '(' n ')'                         { % liftIO $ putStrLn "EXPRESSIONTUPLE -> left id '(' n ')' " } -- x = left tupla1(2)
         | right id '(' n ')'                               { % liftIO $ putStrLn "EXPRESSIONTUPLE -> right id '(' n ')' " } -- x = right tupla1(1)
         | left id '(' id ')'                               { % liftIO $ putStrLn "EXPRESSIONTUPLE -> left id '(' id ')' " }
         | right id '(' id ')'                              { % liftIO $ putStrLn "EXPRESSIONTUPLE -> right id '(' id ')' " }
-}

ARRAYPOSITION : id '[' n ']'                                { % liftIO $ putStrLn "ARRAYPOSITION -> id '[' n ']' " }
         | id '[' id ']'                                    { % liftIO $ putStrLn "ARRAYPOSITION -> id '[' id ']' " }

EXPRESSIONSTRUCT : id '.' id                                { % liftIO $ putStrLn "EXPRESSIONSTRUCT -> id . id " }

FUNCTIONCALL : id '(' IDS ')'                                { % liftIO $ putStrLn "EXPRESSIONSTRUCT -> id '(' id ')' " }

--LPARAMETERSSTRUCT : id '=' EXPRESSION ',' LPARAMETERSSTRUCT { % liftIO $ putStrLn "LPARAMETERSSTRUCT -> id '=' EXPRESSION ',' --LPARAMETERSSTRUCT " }
        --  | id '=' EXPRESSION                               { % liftIO $ putStrLn "LPARAMETERSSTRUCT -> id '=' EXPRESSION " }


MAYBELINE : {- empty -}                   {% liftIO $ putStrLn "MAYBELINE -> \\ " }
          | newline                       {% liftIO $ putStrLn "MAYBELINE -> newline " }

{


lexwrap :: (Token -> ParseM a) -> ParseM a
lexwrap cont = do
  tkn <- alexGetToken
  liftIO $ putStrLn $ "    " ++ show tkn
  cont tkn
}