-- OKComputer
-- Parser

{
module Parser where
import Lexer
import ParseMonad
import Tokens
import Control.Monad.Except
}

%name parse
%tokentype { Token }
%monad { ParseM }
%error { (throwError . show) }
%lexer { lexwrap }{ EOFTkn }

%token
  youbegin                                { YouBeginTkn _ }        -- Block Start
  whereiend                               { WhereIEndTkn _ }       -- Block End
  if                                      { IfTkn $$ }              -- Selection
  ifyouhavetoask                          { IfYouHaveToAskTkn $$ }  -- Selection
  otherside                               { OthersideTkn _ }       -- Selection
  cantstop                                { CantStopTkn $$ }        -- While Iteration
  breakthru                               { BreakthruTkn _ }       -- Break
  onemoretime                             { OneMoreTimeTkn $$ }     -- For Iteration
  ';'                                     { SemiColonTkn _ }       -- For Iteration
  readmymind                              { ReadMyMindTkn $$ }      -- Data entry/read
  go                                      { GoTkn $$ }              -- Data exit/write
  gomental                                { GoMentalTkn $$ }
  goslowly                                { GoSlowlyTkn $$ }        -- Data exit/writeln
  dafunk                                  { DaFunkTkn $$}          -- Method with return/Function
  ':'                                     { ColonTkn _ }           -- Method with return/Function
  getback                                 { GetBackTkn _ }         -- Return
  intothevoid                             { IntoTheVoidTkn _ }     -- Void
  newlife                                 { NewLifeTkn _ }         -- Calloc
--  saveme                                  { SaveMeTkn }          -- Malloc
--  keepyourselfalive                       { KeepAliveTkn }       -- Realloc
  amnesiac                                { AmnesiacTkn _ }        -- Free
  exitmusic                               { ExitMusicTkn _ }       -- Exit
  aroundtheworld                          { AroundTheWorldTkn _ }  -- Import
--  holeinmysoul                            { HoleInMySoulTkn }    -- Templates

  -- Type Tokens
  int                                     { IntTkn _ }
  pointer                                 { PointerTkn _ }  -- Apuntador, agregar a LEXER
  float                                   { FloatTkn _ }
  char                                    { CharTkn _ }
  boolean                                 { BooleanTkn _ }
  ok                                      { OkTkn _ }              -- True
  notok                                   { NotOkTkn _ }           -- False
  '['                                     { ArrayStartTkn _ }
  ']'                                     { ArrayEndTkn _ }
  band                                    { BandTkn _ }            -- Registers/structs
  union                                   { UnionTkn _ }
--  '&'                                      { PointerTkn }         -- Pointers
  duets                                   { DuetsTkn _ }           -- Tuple
  left                                    { LeftTkn _ }
  right                                   { RightTkn _ }

  -- Operations Tokens
  mod                                     { ModTkn _ }
  div                                     { DivTkn _ }
  not                                     { NotTkn _ }
  and                                     { AndTkn _ }
  or                                      { OrTkn _ }
  ','                                     { CommaTkn _ }
  '('                                     { ParenOpenTkn _ }
  ')'                                     { ParenCloseTkn _ }
  '+'                                     { PlusTkn _ }
  '=='                                    { EqualTkn _ }
  '*'                                     { ProductTkn _ }
  '-'                                     { MinusTkn _ }
  '%'                                     { RestTkn _ }
  '/'                                     { DivExacTkn _ }
  '!='                                    { DifTkn _ }
  '>='                                    { GreaterEqualTkn _ }
  '<='                                    { LessEqualTkn _ }
  '>'                                     { GreaterTkn _ }
  '<'                                     { LessTkn _ }
 -- '->'                                    { TypeTkn }
  '='                                     { AssignTkn _ }

  -- Otros
  id                                      { IdTkn _ _ }
  n                                       { NumLiteralTkn _ _ }
  newline                                 { NewLineTkn _ }
  -- c                                       { } -- char
  string                                  { StringTkn _ _ }


%left or
%left and
%nonassoc '>' '<' '==' '!=' '>=' '<='
%left '+' '-'
%left '*' '/' '%' mod div
%nonassoc not
-- TODO unary minus sign
%%
-- Start
START : IMPORTS LFUNCTIONS { } -- Global variables?

IMPORTS : IMPORT IMPORTS        { }
        | {- empty -}           { }

IMPORT : aroundtheworld  IDS  LINE1  { }

LDECLARATIONS : DECLARATION LDECLARATIONS   { }
              | DECLARATION                 { }

DECLARATION : TYPE DECLARATIONTYPE newline { }
        | TUPLE { }
        | ARRAY { }
        | STRUCT { }
        | newlife id { }      -- No estoy claro todavia como haremos esto

-- DECLARATION : IDS   { }
--      | IDS '=' EXPRESSION   { }
--      | IDS '=' EXPRESSION ',' DECLARATION   { }

DECLARATIONTYPE : id '=' EXPRESSION { }
            | id '=' EXPRESSION ',' DECLARATIONTYPE { }
            | id                 { }
            | id ',' DECLARATIONTYPE { }

LFUNCTIONS : FUNCIONINIC LFUNCTIONS { }
          | FUNCIONINIC { }

-- Probablemente vaya newline antes del youbegin y whereiend

FUNCIONINIC : dafunk id '(' LPARAMETERSFUNC ')' ':' RETURNTYPE LINE0 youbegin INSIDEFUNCTION whereiend LINE1 { }

RETURNTYPE: intothevoid                                            {}
          | int                                                    {}
          | char                                                   {}
          | boolean                                                {}

FUNCTIONPARAMETER : TYPE id { }
           | TYPE id '[' ']'  { }
           | duets id '(' ')'    { }
           | id id     { }       -- Structs

INSIDEFUNCTION : INSIDEFUNCTION newline DECLARATION { }
         | INSIDEFUNCTION newline INSTRUCTION       { }
         | INSIDEFUNCTION newline                   { }
         | DECLARATION                       { }
         | INSTRUCTION                      { }
         | {- empty -}                      { }

LPARAMETERSFUNC : {- empty -}                 { }
                | NONEMPTYLPARAMETERSFUNC     { }

NONEMPTYLPARAMETERSFUNC : FUNCTIONPARAMETER ',' NONEMPTYLPARAMETERSFUNC { }
                        | FUNCTIONPARAMETER                             { }

-- Probablemente vaya newline antes del youbegin y whereiend PUESTOS
INSTRUCTION : go '(' PRINT ')' newline  { }
            | goslowly '(' PRINT ')' newline  { }
            | gomental '(' PRINT ')'  newline  { }
            | amnesiac '(' id ')' newline { }
            | readmymind '(' id ')' newline { }
            | if EXPRESSION newline youbegin INSIDEFUNCTION whereiend newline { }
            | if EXPRESSION newline youbegin INSIDEFUNCTION whereiend newline IFELSE { }      -- No se si necesitaria newline
            | cantstop EXPRESSION newline youbegin INSIDEFUNCTION newline whereiend newline { }
            | onemoretime TYPE id '=' EXPRESSION ';' EXPRESSION ';'EXPRESSION newline youbegin INSIDEFUNCTION whereiend newline { }
            | id '=' EXPRESSION newline { }
            | getback EXPRESSION newline { }
            | breakthru newline  { }
            | exitmusic newline  { }



IFELSE : ifyouhavetoask EXPRESSION newline youbegin INSIDEFUNCTION whereiend newline IFELSE { }
       | otherside newline youbegin INSIDEFUNCTION newline whereiend newline{ }

-- ONEMORETIMEDEC : TYPE id '=' E

PRINT : string ',' PRINT   { }
     | id ','     PRINT  { }
     | string         { }
     | id           { }

IDS : id ',' IDS { }
  | id   { }

TYPE : int { }
   | pointer   { }
   | float { }
   | boolean { }
   | char { }
   | string { }

ARRAY : TYPE id '[' n ']'    { }
    | TYPE id '[' id ']'  { }

STRUCT : band id id '(' LDECLARATIONS ')'    { }
       | union id id '(' LDECLARATIONS ')'  { }
       | id id '(' LPARAMETERSSTRUCT ')'  { }

TUPLE : duets id '(' TYPE ',' TYPE ',' n ')'    { }
    | duets id '(' TYPE ',' TYPE ',' id ')'   { }

EXPRESSION : id   { }
      | n   { }
      | string  { }
      -- | c   { }
      | ok   { }
      | notok   { }
      | '(' EXPRESSION ')' { }
      | EXPRESSION '<' EXPRESSION { }
      | EXPRESSION '>' EXPRESSION { }
      | EXPRESSION '<=' EXPRESSION { }
      | EXPRESSION '>=' EXPRESSION { }
      | EXPRESSION '==' EXPRESSION { }
      | EXPRESSION '!=' EXPRESSION { }
      | not EXPRESSION { }
      | EXPRESSION and EXPRESSION { }
      | EXPRESSION or EXPRESSION { }
      | '-' EXPRESSION  { }
      | EXPRESSION '+' EXPRESSION { }
      | EXPRESSION '-' EXPRESSION { }
      | EXPRESSION '*' EXPRESSION { }
      | EXPRESSION '/' EXPRESSION { }
      | EXPRESSION '%' EXPRESSION { }
      | EXPRESSION mod EXPRESSION { }
      | EXPRESSION div EXPRESSION { }
      | EXPRESSIONTUPLE       { }
      | EXPRESSIONARRAY     { }
      | EXPRESSIONSTRUCT   { }

EXPRESSIONTUPLE : left id '(' n ')'  { } -- x = left tupla1(2)
         | right id '(' n ')'  { } -- x = right tupla1(1)
         | left id '(' id ')'  { }
         | right id '(' id ')'  { }

EXPRESSIONARRAY : id '[' n ']'    { }
         | id '[' id ']'  { }

EXPRESSIONSTRUCT : id '(' id ')'   { }

LPARAMETERSSTRUCT : id '=' EXPRESSION ',' LPARAMETERSSTRUCT   { }
          | id '=' EXPRESSION     { }


LINE0 : {- empty -}              { }
       | LINE1                   { }

LINE1 : newline                  { }
       | LINE1 newline           { }

{


lexwrap :: (Token -> ParseM a) -> ParseM a
lexwrap cont = alexGetToken >>= cont
}
