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
  pointer                                 { PointerTkn $$ }  -- Apuntador, agregar a LEXER
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
-- unary &
%%
-- Start
START : IMPORTS OUTSIDEFUNCTION {% liftIO $ putStrLn "START -> IMPORTS OUTSIDEFUNCTION" } -- TODO:  Global variables?


IMPORTS : IMPORT newline IMPORTS        {% liftIO $ putStrLn "IMPORTS -> IMPORT newline IMPORTS" }
        | {- empty -}                   {% liftIO $ putStrLn "IMPORTS -> \\" }

IMPORT : aroundtheworld IDS             {% liftIO $ putStrLn "IMPORT -> aroundtheworld IDS " }

IDS : id ',' IDS                             { % liftIO $ putStrLn "IDS -> id ',' IDS " }
  | id                                       { % liftIO $ putStrLn "IDS -> id " }


OUTSIDEFUNCTION : FUNCTIONINIC newline OUTSIDEFUNCTION    {% liftIO $ putStrLn "LFUNCTION -> FUNCTIONINIC newline OUTSIDEFUNCTION " }
           | DECLARATION newline OUTSIDEFUNCTION          {% liftIO $ putStrLn "LFUNCTION -> DECLARATION newline OUTSIDEFUNCTION " }
           | DEFINESTRUCT newline OUTSIDEFUNCTION         {% liftIO $ putStrLn "LFUNCTION -> DEFINESTRUCT newline OUTSIDEFUNCTION " }
           | {- empty -}                        {% liftIO $ putStrLn "LFUNCTION -> \\ " }


FUNCTIONINIC : dafunk id '(' LPARAMETERSFUNC ')' ':' RETURNTYPE BLOCK    {% liftIO $ putStrLn "FUNCTIONINIC  -> dafunk id '(' LPARAMETERSFUNC ')' ':' RETURNTYPE BLOCK" }


RETURNTYPE: intothevoid                                                 {% liftIO $ putStrLn "RETURNTYPE -> intothevoid" }
          | TYPE                                                        {% liftIO $ putStrLn "RETURNTYPE -> TYPE" }

LPARAMETERSFUNC : {- empty -}                                           {% liftIO $ putStrLn "LPARAMETERSFUNC -> \\ " }
                | NONEMPTYLPARAMETERSFUNC                               {% liftIO $ putStrLn "LPARAMETERSFUNC -> NONEMPTYLPARAMETERSFUNC " }

NONEMPTYLPARAMETERSFUNC : FUNCTIONPARAMETER ',' NONEMPTYLPARAMETERSFUNC {% liftIO $ putStrLn "NONEMPTYLPARAMETERSFUNC -> FUNCTIONPARAMETER ',' NONEMPTYLPARAMETERSFUNC" }
                        | FUNCTIONPARAMETER                             {% liftIO $ putStrLn "NONEMPTYLPARAMETERSFUNC -> FUNCTIONPARAMETER" }

FUNCTIONPARAMETER : TYPE id                                             {% liftIO $ putStrLn "FUNCTIONPARAMETER  -> TYPE id" }
           -- | TYPE id '[' ']'                                            {% liftIO $ putStrLn "FUNCTIONPARAMETER  -> TYPE id '[' ']'" }


BLOCK : MAYBELINE youbegin INSIDEFUNCTION MAYBELINE whereiend           {% liftIO $ putStrLn "BLOCK -> MAYBELINE youbegin INSIDEFUNCTION MAYBELINE whereiend" }
      --| MAYBELINE INSTRUCTION                                           { }

INSIDEFUNCTION : INSIDEFUNCTION newline INSTRUCTION                     {% liftIO $ putStrLn "INSIDEFUNCTION -> INSIDEFUNCTION newline DECLARATION" }
         | INSTRUCTION                                                  {% liftIO $ putStrLn "INSIDEFUNCTION -> INSTRUCTION" }
         | {- empty -}                                                  {% liftIO $ putStrLn "INSIDEFUNCTION -> \\ " }


DECLARATION : TYPE DECLARATIONTYPE { % liftIO $ putStrLn "DECLARATION -> TYPE DECLARATIONTYPE" }

TYPE : TYPE2              {}
    |  TYPE2 '^'          {}

TYPE2 : int                                    { % liftIO $ putStrLn "TYPE -> int " }
   | float                                    { % liftIO $ putStrLn "TYPE -> float " }
   | boolean                                  { % liftIO $ putStrLn "TYPE -> boolean " }
   | char                                     { % liftIO $ putStrLn "TYPE -> char " }
   | string                                   { % liftIO $ putStrLn "TYPE -> string " }
   | id                                       { % liftIO $ putStrLn "TYPE -> id " }

DECLARATIONTYPE : ID '=' EXPRESSION                 {% liftIO $ putStrLn "DECLARATIONTYPE -> ID '=' EXPRESSION" }
            | ID '=' EXPRESSION ',' DECLARATIONTYPE {% liftIO $ putStrLn "DECLARATIONTYPE -> ID '=' EXPRESSION ',' DECLARATIONTYPE" }
            | ID                                    {% liftIO $ putStrLn "DECLARATIONTYPE -> ID" }
            | ID ',' DECLARATIONTYPE                {% liftIO $ putStrLn "DECLARATIONTYPE -> ID ',' DECLARATIONTYPE" }

ID : id                                                    { }
   | id '[' EXPRESSION ']'                                 { }

-- Probablemente vaya newline antes del youbegin y whereiend PUESTOS
INSTRUCTION : go '(' PRINT ')'                                                                                          {% liftIO $ putStrLn "INSTRUCTION -> go '(' PRINT ')' " }
            | goslowly '(' PRINT ')'                                                                                    {% liftIO $ putStrLn "INSTRUCTION -> goslowly '(' PRINT ')' " }
            | gomental '(' PRINT ')'                                                                                    {% liftIO $ putStrLn "INSTRUCTION -> gomental '(' PRINT ')' " }
            | readmymind '(' IDS ')'                                                                                     {% liftIO $ putStrLn "INSTRUCTION -> readmymind '(' id ')' " }
            | amnesiac '(' id ')' 																						{ }
            
            | if EXPRESSION BLOCK IFELSE                                                                                {% liftIO $ putStrLn "INSTRUCTION -> if EXPRESSION BLOCK IFELSE " }
            | cantstop EXPRESSION BLOCK                                                                                 {% liftIO $ putStrLn "INSTRUCTION -> cantstop EXPRESSION BLOCK  " }
            | onemoretime TYPE id '=' EXPRESSION ';' EXPRESSION ';' EXPRESSION BLOCK                                    {% liftIO $ putStrLn "INSTRUCTION -> onemoretime TYPE id '=' EXPRESSION ';' EXPRESSION ';'EXPRESSION BLOCK " }
            | getback EXPRESSION                                                                                        {% liftIO $ putStrLn "INSTRUCTION -> getback EXPRESSION " }
            | breakthru                                                                                                 {% liftIO $ putStrLn "INSTRUCTION -> breakthru " }
            | exitmusic                                                                                                 {% liftIO $ putStrLn "INSTRUCTION -> exitmusic " }
            | DECLARATION  {}
            | EXPRESSION   {}

IFELSE : ifyouhavetoask EXPRESSION BLOCK IFELSE                                     { % liftIO $ putStrLn "IFELSE -> ifyouhavetoask EXPRESSION BLOCK newline IFELSE " }
       | otherside BLOCK                                                            { % liftIO $ putStrLn "IFELSE -> otherside BLOCK newline " }
       | {- empty -}                                                                        { }

PRINT : string ',' PRINT                     { % liftIO $ putStrLn "PRINT -> string ',' PRINT " }
     | id ','     PRINT                      { % liftIO $ putStrLn "PRINT -> id ','     PRINT " }
     | string                                { % liftIO $ putStrLn "PRINT -> string " }
     | id                                    { % liftIO $ putStrLn "PRINT -> id " }

DEFINESTRUCT : band id '{' newline LDECLARATIONS newline'}'    { % liftIO $ putStrLn "STRUCT -> band id id '(' LDECLARATIONS ')' " }
             | union id '{' newline LDECLARATIONS newline '}'   { % liftIO $ putStrLn "STRUCT -> union id id '(' LDECLARATIONS ')' " }

LDECLARATIONS : LDECLARATIONS newline DECLARATION  {% liftIO $ putStrLn "LDECLARATIONS -> DECLARATION LDECLARATIONS" }
              | DECLARATION                        {% liftIO $ putStrLn "LDECLARATIONS -> DECLARATION" }


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
           -- | EXPRESSIONTUPLE            { % liftIO $ putStrLn "EXPRESSION -> EXPRESSIONTUPLE " }
           | ARRAYPOSITION            { % liftIO $ putStrLn "EXPRESSION -> ARRAYPOSITION " }
           | EXPRESSIONSTRUCT           { % liftIO $ putStrLn "EXPRESSION -> EXPRESSIONSTRUCT " }
           | FUNCTIONCALL               { }
           | newlife '(' EXPRESSION ')' { }
           | '^' id                     { }
           | id '=' EXPRESSION          { % liftIO $ putStrLn "EXPRESSION -> id '=' EXPRESSION  " }

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
