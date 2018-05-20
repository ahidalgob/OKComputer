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

INSIDEFUNCTION : INSIDEFUNCTION INSTRUCTION                     { INSIDEN $2 }
         | {- empty -}                                                  { INSIDEVOID }


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

ID : id                                                    { IDNORMALN $ tknString $1 }
   | id '[' EXPRESSION ']'                                 { IDARRAYN (tknString $1) $3 }

-- Probablemente vaya newline antes del youbegin y whereiend PUESTOS
INSTRUCTION : go '(' PRINT ')' newline                                          { GOINGN $3 }
            | goslowly '(' PRINT ')' newline                                    { GOINGSLOWLYN $3 }
            | gomental '(' PRINT ')' newline                                    { GOINGMENTALN $3 }
            | readmymind '(' IDS ')' newline                                    { REDMYMINDN $3 }
            | amnesiac '(' id ')' newline 										{ AMNESIACN $ tknString $3 }
            | if EXPRESSION BLOCK IFELSE                                        { IFN $2 $3 $4 }
            | cantstop EXPRESSION BLOCK                                         { CANTSTOPN $2 $3 }
            | onemoretime TYPE id '=' EXPRESSION ';' EXPRESSION ';' EXPRESSION BLOCK                       { ONEMORETIMEN $2 (tknString $3) $5 $7 $9 $10 }
            | getback EXPRESSION newline                                        { GETBACKN $2 }
            | breakthru newline                                                 { BREAKTHRUN }
            | exitmusic newline                                                 { EXITMUSICN }
            | DECLARATION newline 												{ DECLARATIONNINST $1 }
            | EXPRESSION newline   												{ EXPRESSIONNINST $1 }

IFELSE : ifyouhavetoask EXPRESSION BLOCK IFELSE                                     { IFASKN $2 $3 $4 }
       | otherside BLOCK                                                            { OTHERSIDEN $2 }
       | {- empty -}                                                                { IFELSEVOID }

-- Probablemente tenga un detallito aca
PRINT : string ',' PRINT                     { (PRINTSTRING (tknString $1)):($3)  }
     | id ','     PRINT                      { (PRINTSTRING (tknString $1)):($3) }
     | string                                { [PRINTSTRING $ tknString $1] }
     | id                                    { [PRINTSTRING $ tknString $1] }

DEFINESTRUCT : band id '{' newline LDECLARATIONS newline'}'    {BANDN  (tknString $2) $5}
             | union id '{' newline LDECLARATIONS newline '}'   {UNIONN (tknString $2) $5}

LDECLARATIONS : LDECLARATIONS newline DECLARATION  { REC1 $1 $3 }
              | DECLARATION                        { REC2 $1 }


EXPRESSION : id                         { IDEXPRESSION $ tknString $1 }
           | n                          { NUMBEREXPN $ tknString $1 }
           | string                     { STRINGEXPN $ tknString $1 }
      --   | c                          { % liftIO $ putStrLn "EXPRESSION -> c " }
           | ok                         { OKN }
           | notok                      { NOTOKN }
           | '(' EXPRESSION ')'         { PARENTESISN $2 }
           | EXPRESSION '<' EXPRESSION  { COMPARN $1 "<" $3 }
           | EXPRESSION '>' EXPRESSION  { COMPARN $1 ">" $3 }
           | EXPRESSION '<=' EXPRESSION { COMPARN $1 "<=" $3 }
           | EXPRESSION '>=' EXPRESSION { COMPARN $1 ">=" $3 }
           | EXPRESSION '==' EXPRESSION { COMPARN $1 "==" $3 }
           | EXPRESSION '!=' EXPRESSION { COMPARN $1 "!=" $3 }
           | not EXPRESSION             { NOTN "not" $2  }
           | EXPRESSION and EXPRESSION  { LOGICN $1 "and" $3 }
           | EXPRESSION or EXPRESSION   { LOGICN $1 "or" $3 }
           | '-' EXPRESSION             { MINUSN "-" $2 }
           | EXPRESSION '+' EXPRESSION  { ARITN $1 "+" $3 }
           | EXPRESSION '-' EXPRESSION  { ARITN $1 "-" $3 }
           | EXPRESSION '*' EXPRESSION  { ARITN $1 "*" $3 }
           | EXPRESSION '/' EXPRESSION  { ARITN $1 "/" $3 }
           | EXPRESSION '%' EXPRESSION  { ARITN $1 "%" $3 }
           | EXPRESSION mod EXPRESSION  { ARITN $1 "mod" $3 }
           | EXPRESSION div EXPRESSION  { ARITN $1 "div" $3 }
           -- | EXPRESSIONTUPLE            { % liftIO $ putStrLn "EXPRESSION -> EXPRESSIONTUPLE " }
           | ARRAYPOSITION              { ARRAYINSTN $1 }
           | EXPRESSIONSTRUCT           { EXPSTRUCTN $1 }
           | FUNCTIONCALL               { FUNCCALLN $1 }
           | newlife '(' EXPRESSION ')' { NEWLIFEN $3 }
           | '^' id                     { POINTERN $ tknString $2 }
           | id '=' EXPRESSION          { ASSIGNN (tknString $1) $3 }

           {-
EXPRESSIONTUPLE : left id '(' n ')'                         { % liftIO $ putStrLn "EXPRESSIONTUPLE -> left id '(' n ')' " } -- x = left tupla1(2)
         | right id '(' n ')'                               { % liftIO $ putStrLn "EXPRESSIONTUPLE -> right id '(' n ')' " } -- x = right tupla1(1)
         | left id '(' id ')'                               { % liftIO $ putStrLn "EXPRESSIONTUPLE -> left id '(' id ')' " }
         | right id '(' id ')'                              { % liftIO $ putStrLn "EXPRESSIONTUPLE -> right id '(' id ')' " }
-}

ARRAYPOSITION : id '[' n ']'                                { ARRAYPOSN (tknString $1) (tknString $3) }
         | id '[' id ']'                                    { ARRAYPOSN (tknString $1) (tknString $3) }

EXPRESSIONSTRUCT : id '.' id                                { EXPRESSIONSTRUCTN (tknString $1) (tknString $3) }

FUNCTIONCALL : id '(' IDS ')'                                { FUNCTIONCALLN (tknString $1) $3}

--LPARAMETERSSTRUCT : id '=' EXPRESSION ',' LPARAMETERSSTRUCT { % liftIO $ putStrLn "LPARAMETERSSTRUCT -> id '=' EXPRESSION ',' --LPARAMETERSSTRUCT " }
        --  | id '=' EXPRESSION                               { % liftIO $ putStrLn "LPARAMETERSSTRUCT -> id '=' EXPRESSION " }


MAYBELINE : {- empty -}                   { }
          | newline                       { }

{


lexwrap :: (Token -> ParseM a) -> ParseM a
lexwrap cont = do
  tkn <- alexGetToken
  liftIO $ putStrLn $ "    " ++ show tkn
  cont tkn
}
