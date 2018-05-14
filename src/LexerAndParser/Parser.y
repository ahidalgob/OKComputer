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
  -- c                                       {% liftIO $ print " -> " } -- char
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
START : IMPORTS LFUNCTIONS {% liftIO $ putStrLn "START -> IMPORTS LFUNCTIONS" } -- TODO:  Global variables?


--newlines are correctly handled before each import.
IMPORTS : IMPORT newline IMPORTS        {% liftIO $ putStrLn "IMPORTS -> IMPORT newline IMPORTS" }
        | {- empty -}                   {% liftIO $ putStrLn "IMPORTS -> \\" }

IMPORT : aroundtheworld IDS             {% liftIO $ putStrLn "IMPORT -> aroundtheworld IDS " }


LFUNCTIONS : FUNCTIONINIC newline LFUNCTIONS    {% liftIO $ putStrLn "LFUNCTION -> FUNCTIONINIC newline LFUNCTIONS " }
           | {- empty -}                        {% liftIO $ putStrLn "LFUNCTION -> FUNCTIONINIC newline LFUNCTIONS " }


FUNCTIONINIC : dafunk id '(' LPARAMETERSFUNC ')' ':' RETURNTYPE BLOCK    {% liftIO $ putStrLn "FUNCTIONINIC  -> dafunk id '(' LPARAMETERSFUNC ')' ':' RETURNTYPE BLOCK" }


RETURNTYPE: intothevoid                                                 {% liftIO $ putStrLn "RETURNTYPE -> intothevoid" }
          | int                                                         {% liftIO $ putStrLn "RETURNTYPE -> int" }
          | char                                                        {% liftIO $ putStrLn "RETURNTYPE -> char" }
          | boolean                                                     {% liftIO $ putStrLn "RETURNTYPE -> boolean" }

LPARAMETERSFUNC : {- empty -}                                           {% liftIO $ putStrLn "LPARAMETERSFUNC -> " }
                | NONEMPTYLPARAMETERSFUNC                               {% liftIO $ putStrLn "LPARAMETERSFUNC -> " }

NONEMPTYLPARAMETERSFUNC : FUNCTIONPARAMETER ',' NONEMPTYLPARAMETERSFUNC {% liftIO $ putStrLn "NONEMPTYLPARAMETERSFUNC -> " }
                        | FUNCTIONPARAMETER                             {% liftIO $ putStrLn "NONEMPTYLPARAMETERSFUNC -> " }

FUNCTIONPARAMETER : TYPE id                                             {% liftIO $ putStrLn "FUNCTIONPARAMETER  -> " }
           | TYPE id '[' ']'                                            {% liftIO $ putStrLn "FUNCTIONPARAMETER  -> " }
           | duets id '(' ')'                                           {% liftIO $ putStrLn "FUNCTIONPARAMETER  -> " }
           | id id                                                      {% liftIO $ putStrLn "FUNCTIONPARAMETER  -> " }       -- Structs


BLOCK : MAYBELINE youbegin INSIDEFUNCTION MAYBELINE whereiend           {% liftIO $ putStrLn "BLOCK -> " }

INSIDEFUNCTION : INSIDEFUNCTION newline DECLARATION                     {% liftIO $ putStrLn "INSIDEFUNCTION -> " }
         | INSIDEFUNCTION newline INSTRUCTION                           {% liftIO $ putStrLn "INSIDEFUNCTION -> " }
         | DECLARATION                                                  {% liftIO $ putStrLn "INSIDEFUNCTION -> " }
         | INSTRUCTION                                                  {% liftIO $ putStrLn "INSIDEFUNCTION -> " }
         | {- empty -}                                                  {% liftIO $ putStrLn "INSIDEFUNCTION -> " }




LDECLARATIONS : DECLARATION LDECLARATIONS   {% liftIO $ putStrLn "LDECLARATIONS -> " }
              | DECLARATION                 {% liftIO $ putStrLn "LDECLARATIONS -> " }

DECLARATION : TYPE DECLARATIONTYPE { % liftIO $ putStrLn "DECLARATION -> " }
        | TUPLE                    { % liftIO $ putStrLn "DECLARATION -> " }
        | ARRAY                    { % liftIO $ putStrLn "DECLARATION -> " }
        | STRUCT                   { % liftIO $ putStrLn "DECLARATION -> " }
        | newlife id               { % liftIO $ putStrLn "DECLARATION -> " }      -- No estoy claro todavia como haremos esto


DECLARATIONTYPE : id '=' EXPRESSION                 {% liftIO $ putStrLn "DECLARATIONTYPE -> " }
            | id '=' EXPRESSION ',' DECLARATIONTYPE {% liftIO $ putStrLn "DECLARATIONTYPE -> " }
            | id                                    {% liftIO $ putStrLn "DECLARATIONTYPE -> " }
            | id ',' DECLARATIONTYPE                {% liftIO $ putStrLn "DECLARATIONTYPE -> " }



-- Probablemente vaya newline antes del youbegin y whereiend PUESTOS
INSTRUCTION : go '(' PRINT ')'                                                                                          {% liftIO $ putStrLn "INSTRUCTION -> " }
            | goslowly '(' PRINT ')'                                                                                    {% liftIO $ putStrLn "INSTRUCTION -> " }
            | gomental '(' PRINT ')'                                                                                    {% liftIO $ putStrLn "INSTRUCTION -> " }
            | amnesiac '(' id ')'                                                                                       {% liftIO $ putStrLn "INSTRUCTION -> " }
            | readmymind '(' id ')'                                                                                     {% liftIO $ putStrLn "INSTRUCTION -> " }
            | if EXPRESSION newline youbegin INSIDEFUNCTION whereiend                                                   {% liftIO $ putStrLn "INSTRUCTION -> " }
            | if EXPRESSION newline youbegin INSIDEFUNCTION whereiend IFELSE                                            {% liftIO $ putStrLn "INSTRUCTION -> " }
            | cantstop EXPRESSION newline youbegin INSIDEFUNCTION newline whereiend                                     {% liftIO $ putStrLn "INSTRUCTION -> " }
            | onemoretime TYPE id '=' EXPRESSION ';' EXPRESSION ';'EXPRESSION newline youbegin INSIDEFUNCTION whereiend {% liftIO $ putStrLn "INSTRUCTION -> " }
            | id '=' EXPRESSION                                                                                         {% liftIO $ putStrLn "INSTRUCTION -> " }
            | getback EXPRESSION                                                                                        {% liftIO $ putStrLn "INSTRUCTION -> " }
            | breakthru                                                                                                 {% liftIO $ putStrLn "INSTRUCTION -> " }
            | exitmusic                                                                                                 {% liftIO $ putStrLn "INSTRUCTION -> " }



IFELSE : ifyouhavetoask EXPRESSION newline youbegin INSIDEFUNCTION whereiend newline IFELSE { % liftIO $ putStrLn "IFELSE -> " }
       | otherside newline youbegin INSIDEFUNCTION newline whereiend newline                { % liftIO $ putStrLn "IFELSE -> " }

-- ONEMORETIMEDEC : TYPE id '=' E

PRINT : string ',' PRINT                     { % liftIO $ putStrLn "PRINT -> " }
     | id ','     PRINT                      { % liftIO $ putStrLn "PRINT -> " }
     | string                                { % liftIO $ putStrLn "PRINT -> " }
     | id                                    { % liftIO $ putStrLn "PRINT -> " }

IDS : id ',' IDS                             { % liftIO $ putStrLn "IDS -> " }
  | id                                       { % liftIO $ putStrLn "IDS -> " }

TYPE : int                                   { % liftIO $ putStrLn "TYPE -> " }
   | pointer                                 { % liftIO $ putStrLn "TYPE -> " }
   | float                                   { % liftIO $ putStrLn "TYPE -> " }
   | boolean                                 { % liftIO $ putStrLn "TYPE -> " }
   | char                                    { % liftIO $ putStrLn "TYPE -> " }
   | string                                  { % liftIO $ putStrLn "TYPE -> " }

ARRAY : TYPE id '[' n ']'                    { % liftIO $ putStrLn "ARRAY -> " }
    | TYPE id '[' id ']'                     { % liftIO $ putStrLn "ARRAY -> " }

STRUCT : band id id '(' LDECLARATIONS ')'    { % liftIO $ putStrLn "STRUCT -> " }
       | union id id '(' LDECLARATIONS ')'   { % liftIO $ putStrLn "STRUCT -> " }
       | id id '(' LPARAMETERSSTRUCT ')'     { % liftIO $ putStrLn "STRUCT -> " }

TUPLE : duets id '(' TYPE ',' TYPE ',' n ')' { % liftIO $ putStrLn "TUPLE -> " }
    | duets id '(' TYPE ',' TYPE ',' id ')'  { % liftIO $ putStrLn "TUPLE -> " }

EXPRESSION : id                         { % liftIO $ putStrLn "EXPRESSION -> " }
           | n                          { % liftIO $ putStrLn "EXPRESSION -> " }
           | string                     { % liftIO $ putStrLn "EXPRESSION -> " }
      --   | c                          { % liftIO $ putStrLn "EXPRESSION -> " }
           | ok                         { % liftIO $ putStrLn "EXPRESSION -> " }
           | notok                      { % liftIO $ putStrLn "EXPRESSION -> " }
           | '(' EXPRESSION ')'         { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION '<' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION '>' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION '<=' EXPRESSION { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION '>=' EXPRESSION { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION '==' EXPRESSION { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION '!=' EXPRESSION { % liftIO $ putStrLn "EXPRESSION -> " }
           | not EXPRESSION             { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION and EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION or EXPRESSION   { % liftIO $ putStrLn "EXPRESSION -> " }
           | '-' EXPRESSION             { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION '+' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION '-' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION '*' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION '/' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION '%' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION mod EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSION div EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSIONTUPLE            { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSIONARRAY            { % liftIO $ putStrLn "EXPRESSION -> " }
           | EXPRESSIONSTRUCT           { % liftIO $ putStrLn "EXPRESSION -> " }

EXPRESSIONTUPLE : left id '(' n ')'                         { % liftIO $ putStrLn "EXPRESSIONTUPLE -> " } -- x = left tupla1(2)
         | right id '(' n ')'                               { % liftIO $ putStrLn "EXPRESSIONTUPLE -> " } -- x = right tupla1(1)
         | left id '(' id ')'                               { % liftIO $ putStrLn "EXPRESSIONTUPLE -> " }
         | right id '(' id ')'                              { % liftIO $ putStrLn "EXPRESSIONTUPLE -> " }

EXPRESSIONARRAY : id '[' n ']'                              { % liftIO $ putStrLn "EXPRESSIONARRAY -> " }
         | id '[' id ']'                                    { % liftIO $ putStrLn "EXPRESSIONARRAY -> " }

EXPRESSIONSTRUCT : id '(' id ')'                            { % liftIO $ putStrLn "EXPRESSIONSTRUCT -> " }

LPARAMETERSSTRUCT : id '=' EXPRESSION ',' LPARAMETERSSTRUCT { % liftIO $ putStrLn "LPARAMETERSSTRUCT -> " }
          | id '=' EXPRESSION                               { % liftIO $ putStrLn "LPARAMETERSSTRUCT -> " }


MAYBELINE : {- empty -}                   {% liftIO $ putStrLn "MAYBELINE -> " }
          | newline                       {% liftIO $ putStrLn "MAYBELINE -> " }

{


lexwrap :: (Token -> ParseM a) -> ParseM a
lexwrap cont = do
  tkn <- alexGetToken
  liftIO $ putStrLn $ "    " ++ show tkn
  cont tkn
}
