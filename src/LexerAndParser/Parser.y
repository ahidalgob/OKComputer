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
           | {- empty -}                        {% liftIO $ putStrLn "LFUNCTION -> \\ " }


FUNCTIONINIC : dafunk id '(' LPARAMETERSFUNC ')' ':' RETURNTYPE BLOCK    {% liftIO $ putStrLn "FUNCTIONINIC  -> dafunk id '(' LPARAMETERSFUNC ')' ':' RETURNTYPE BLOCK" }


RETURNTYPE: intothevoid                                                 {% liftIO $ putStrLn "RETURNTYPE -> intothevoid" }
          | int                                                         {% liftIO $ putStrLn "RETURNTYPE -> int" }
          | char                                                        {% liftIO $ putStrLn "RETURNTYPE -> char" }
          | boolean                                                     {% liftIO $ putStrLn "RETURNTYPE -> boolean" }

LPARAMETERSFUNC : {- empty -}                                           {% liftIO $ putStrLn "LPARAMETERSFUNC -> \\ " }
                | NONEMPTYLPARAMETERSFUNC                               {% liftIO $ putStrLn "LPARAMETERSFUNC -> NONEMPTYLPARAMETERSFUNC " }

NONEMPTYLPARAMETERSFUNC : FUNCTIONPARAMETER ',' NONEMPTYLPARAMETERSFUNC {% liftIO $ putStrLn "NONEMPTYLPARAMETERSFUNC -> FUNCTIONPARAMETER ',' NONEMPTYLPARAMETERSFUNC" }
                        | FUNCTIONPARAMETER                             {% liftIO $ putStrLn "NONEMPTYLPARAMETERSFUNC -> FUNCTIONPARAMETER" }

FUNCTIONPARAMETER : TYPE id                                             {% liftIO $ putStrLn "FUNCTIONPARAMETER  -> TYPE id" }
           | TYPE id '[' ']'                                            {% liftIO $ putStrLn "FUNCTIONPARAMETER  -> TYPE id '[' ']'" }
           | duets id '(' ')'                                           {% liftIO $ putStrLn "FUNCTIONPARAMETER  -> duets id '(' ')'" }
           | id id                                                      {% liftIO $ putStrLn "FUNCTIONPARAMETER  -> id id" }       -- Structs


BLOCK : MAYBELINE youbegin INSIDEFUNCTION MAYBELINE whereiend           {% liftIO $ putStrLn "BLOCK -> MAYBELINE youbegin INSIDEFUNCTION MAYBELINE whereiend" }

INSIDEFUNCTION : INSIDEFUNCTION newline DECLARATION                     {% liftIO $ putStrLn "INSIDEFUNCTION -> INSIDEFUNCTION newline DECLARATION" }
         | INSIDEFUNCTION newline INSTRUCTION                           {% liftIO $ putStrLn "INSIDEFUNCTION -> INSIDEFUNCTION newline INSTRUCTION" }
         | DECLARATION                                                  {% liftIO $ putStrLn "INSIDEFUNCTION -> DECLARATION" }
         | INSTRUCTION                                                  {% liftIO $ putStrLn "INSIDEFUNCTION -> INSTRUCTION" }
         | {- empty -}                                                  {% liftIO $ putStrLn "INSIDEFUNCTION -> \\ " }




LDECLARATIONS : DECLARATION LDECLARATIONS   {% liftIO $ putStrLn "LDECLARATIONS -> DECLARATION LDECLARATIONS" }
              | DECLARATION                 {% liftIO $ putStrLn "LDECLARATIONS -> DECLARATION" }

DECLARATION : TYPE DECLARATIONTYPE { % liftIO $ putStrLn "DECLARATION -> TYPE DECLARATIONTYPE" }
        | TUPLE                    { % liftIO $ putStrLn "DECLARATION -> TUPLE" }
        | ARRAY                    { % liftIO $ putStrLn "DECLARATION -> ARRAY" }
        | STRUCT                   { % liftIO $ putStrLn "DECLARATION -> STRUCT" }
        | newlife id               { % liftIO $ putStrLn "DECLARATION -> newlife id" }      -- No estoy claro todavia como haremos esto


DECLARATIONTYPE : id '=' EXPRESSION                 {% liftIO $ putStrLn "DECLARATIONTYPE -> id '=' EXPRESSION" }
            | id '=' EXPRESSION ',' DECLARATIONTYPE {% liftIO $ putStrLn "DECLARATIONTYPE -> id '=' EXPRESSION ',' DECLARATIONTYPE" }
            | id                                    {% liftIO $ putStrLn "DECLARATIONTYPE -> id" }
            | id ',' DECLARATIONTYPE                {% liftIO $ putStrLn "DECLARATIONTYPE -> id ',' DECLARATIONTYPE" }



-- Probablemente vaya newline antes del youbegin y whereiend PUESTOS
INSTRUCTION : go '(' PRINT ')'                                                                                          {% liftIO $ putStrLn "INSTRUCTION -> go '(' PRINT ')' " }
            | goslowly '(' PRINT ')'                                                                                    {% liftIO $ putStrLn "INSTRUCTION -> goslowly '(' PRINT ')' " }
            | gomental '(' PRINT ')'                                                                                    {% liftIO $ putStrLn "INSTRUCTION -> gomental '(' PRINT ')' " }
            | amnesiac '(' id ')'                                                                                       {% liftIO $ putStrLn "INSTRUCTION -> amnesiac '(' id ')' " }
            | readmymind '(' id ')'                                                                                     {% liftIO $ putStrLn "INSTRUCTION -> readmymind '(' id ')' " }
            | if EXPRESSION newline youbegin INSIDEFUNCTION whereiend                                                   {% liftIO $ putStrLn "INSTRUCTION -> if EXPRESSION newline youbegin INSIDEFUNCTION whereiend " }
            | if EXPRESSION newline youbegin INSIDEFUNCTION whereiend IFELSE                                            {% liftIO $ putStrLn "INSTRUCTION -> if EXPRESSION newline youbegin INSIDEFUNCTION whereiend IFELSE " }
            | cantstop EXPRESSION newline youbegin INSIDEFUNCTION newline whereiend                                     {% liftIO $ putStrLn "INSTRUCTION -> cantstop EXPRESSION newline youbegin INSIDEFUNCTION newline whereiend  " }
            | onemoretime TYPE id '=' EXPRESSION ';' EXPRESSION ';'EXPRESSION newline youbegin INSIDEFUNCTION whereiend {% liftIO $ putStrLn "INSTRUCTION -> onemoretime TYPE id '=' EXPRESSION ';' EXPRESSION ';'EXPRESSION newline youbegin INSIDEFUNCTION whereiend " }
            | id '=' EXPRESSION                                                                                         {% liftIO $ putStrLn "INSTRUCTION -> id '=' EXPRESSION  " }
            | getback EXPRESSION                                                                                        {% liftIO $ putStrLn "INSTRUCTION -> getback EXPRESSION " }
            | breakthru                                                                                                 {% liftIO $ putStrLn "INSTRUCTION -> breakthru " }
            | exitmusic                                                                                                 {% liftIO $ putStrLn "INSTRUCTION -> exitmusic " }



IFELSE : ifyouhavetoask EXPRESSION newline youbegin INSIDEFUNCTION whereiend newline IFELSE { % liftIO $ putStrLn "IFELSE -> ifyouhavetoask EXPRESSION newline youbegin INSIDEFUNCTION whereiend newline IFELSE " }
       | otherside newline youbegin INSIDEFUNCTION newline whereiend newline                { % liftIO $ putStrLn "IFELSE -> otherside newline youbegin INSIDEFUNCTION newline whereiend newline " }

-- ONEMORETIMEDEC : TYPE id '=' E

PRINT : string ',' PRINT                     { % liftIO $ putStrLn "PRINT -> string ',' PRINT " }
     | id ','     PRINT                      { % liftIO $ putStrLn "PRINT -> id ','     PRINT " }
     | string                                { % liftIO $ putStrLn "PRINT -> string " }
     | id                                    { % liftIO $ putStrLn "PRINT -> id " }

IDS : id ',' IDS                             { % liftIO $ putStrLn "IDS -> id ',' IDS " }
  | id                                       { % liftIO $ putStrLn "IDS -> id " }

TYPE : int                                   { % liftIO $ putStrLn "TYPE -> int " }
   | pointer                                 { % liftIO $ putStrLn "TYPE -> pointer " }
   | float                                   { % liftIO $ putStrLn "TYPE -> float " }
   | boolean                                 { % liftIO $ putStrLn "TYPE -> boolean " }
   | char                                    { % liftIO $ putStrLn "TYPE -> char " }
   | string                                  { % liftIO $ putStrLn "TYPE -> string " }

ARRAY : TYPE id '[' n ']'                    { % liftIO $ putStrLn "ARRAY -> TYPE id '[' n ']' " }
    | TYPE id '[' id ']'                     { % liftIO $ putStrLn "ARRAY -> TYPE id '[' id ']' " }

STRUCT : band id id '(' LDECLARATIONS ')'    { % liftIO $ putStrLn "STRUCT -> band id id '(' LDECLARATIONS ')' " }
       | union id id '(' LDECLARATIONS ')'   { % liftIO $ putStrLn "STRUCT -> union id id '(' LDECLARATIONS ')' " }
       | id id '(' LPARAMETERSSTRUCT ')'     { % liftIO $ putStrLn "STRUCT -> id id '(' LPARAMETERSSTRUCT ')' " }

TUPLE : duets id '(' TYPE ',' TYPE ',' n ')' { % liftIO $ putStrLn "TUPLE -> duets id '(' TYPE ',' TYPE ',' n ')' " }
    | duets id '(' TYPE ',' TYPE ',' id ')'  { % liftIO $ putStrLn "TUPLE -> duets id '(' TYPE ',' TYPE ',' id ')' " }

EXPRESSION : id                         { % liftIO $ putStrLn "EXPRESSION -> id " }
           | n                          { % liftIO $ putStrLn "EXPRESSION -> n " }
           | string                     { % liftIO $ putStrLn "EXPRESSION -> string " }
      --   | c                          { % liftIO $ putStrLn "EXPRESSION -> c " }
           | ok                         { % liftIO $ putStrLn "EXPRESSION -> ok " }
           | notok                      { % liftIO $ putStrLn "EXPRESSION -> notok " }
           | '(' EXPRESSION ')'         { % liftIO $ putStrLn "EXPRESSION -> '(' EXPRESSION ')' " }
           | EXPRESSION '<' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION '<' EXPRESSION " }
           | EXPRESSION '>' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION '<' EXPRESSION " }
           | EXPRESSION '<=' EXPRESSION { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION '<=' EXPRESSION " }
           | EXPRESSION '>=' EXPRESSION { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION '>=' EXPRESSION " }
           | EXPRESSION '==' EXPRESSION { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION '==' EXPRESSION " }
           | EXPRESSION '!=' EXPRESSION { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION '!=' EXPRESSION " }
           | not EXPRESSION             { % liftIO $ putStrLn "EXPRESSION -> not EXPRESSION " }
           | EXPRESSION and EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION and EXPRESSION " }
           | EXPRESSION or EXPRESSION   { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION or EXPRESSION " }
           | '-' EXPRESSION             { % liftIO $ putStrLn "EXPRESSION -> '-' EXPRESSION " }
           | EXPRESSION '+' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION '+' EXPRESSION " }
           | EXPRESSION '-' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION '-' EXPRESSION " }
           | EXPRESSION '*' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION '*' EXPRESSION " }
           | EXPRESSION '/' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION '/' EXPRESSION " }
           | EXPRESSION '%' EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION '%' EXPRESSION " }
           | EXPRESSION mod EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION mod EXPRESSION " }
           | EXPRESSION div EXPRESSION  { % liftIO $ putStrLn "EXPRESSION -> EXPRESSION div EXPRESSION " }
           | EXPRESSIONTUPLE            { % liftIO $ putStrLn "EXPRESSION -> EXPRESSIONTUPLE " }
           | EXPRESSIONARRAY            { % liftIO $ putStrLn "EXPRESSION -> EXPRESSIONARRAY " }
           | EXPRESSIONSTRUCT           { % liftIO $ putStrLn "EXPRESSION -> EXPRESSIONSTRUCT " }

EXPRESSIONTUPLE : left id '(' n ')'                         { % liftIO $ putStrLn "EXPRESSIONTUPLE -> left id '(' n ')' " } -- x = left tupla1(2)
         | right id '(' n ')'                               { % liftIO $ putStrLn "EXPRESSIONTUPLE -> right id '(' n ')' " } -- x = right tupla1(1)
         | left id '(' id ')'                               { % liftIO $ putStrLn "EXPRESSIONTUPLE -> left id '(' id ')' " }
         | right id '(' id ')'                              { % liftIO $ putStrLn "EXPRESSIONTUPLE -> right id '(' id ')' " }

EXPRESSIONARRAY : id '[' n ']'                              { % liftIO $ putStrLn "EXPRESSIONARRAY -> id '[' n ']' " }
         | id '[' id ']'                                    { % liftIO $ putStrLn "EXPRESSIONARRAY -> id '[' id ']' " }

EXPRESSIONSTRUCT : id '(' id ')'                            { % liftIO $ putStrLn "EXPRESSIONSTRUCT -> id '(' id ')' " }

LPARAMETERSSTRUCT : id '=' EXPRESSION ',' LPARAMETERSSTRUCT { % liftIO $ putStrLn "LPARAMETERSSTRUCT -> id '=' EXPRESSION ',' LPARAMETERSSTRUCT " }
          | id '=' EXPRESSION                               { % liftIO $ putStrLn "LPARAMETERSSTRUCT -> id '=' EXPRESSION " }


MAYBELINE : {- empty -}                   {% liftIO $ putStrLn "MAYBELINE -> \\ " }
          | newline                       {% liftIO $ putStrLn "MAYBELINE -> newline " }

{


lexwrap :: (Token -> ParseM a) -> ParseM a
lexwrap cont = do
  tkn <- alexGetToken
  liftIO $ putStrLn $ "    " ++ show tkn
  cont tkn
}
