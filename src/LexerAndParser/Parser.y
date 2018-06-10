-- OKComputer
-- Parser

{
module Parser where
import Lexer
import LowLevelAlex
import ParseMonad
import Tokens
import Control.Monad.Except
import AST
import SymTable

import Data.Maybe
}

%name parse
%tokentype { Token }
%monad { ParseM }
%error { (throwError . ParseError . show) }
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
  --'{'                                     { OpenBraceTkn $$ }
  --'}'                                     { CloseBraceTkn $$ }
  '['                                     { ArrayStartTkn $$ }
  ']'                                     { ArrayEndTkn $$ }
  --band                                    { BandTkn $$ }            -- Registers/structs
  --union                                   { UnionTkn $$ }
  '.'										                  { DotTkn $$ }
  '^'									                    { PointerTkn $$ }

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
-- Array type
-- char
-- braces
-- string operations
--

%right '='
%left or
%left and
%nonassoc '>' '<' '==' '!=' '>=' '<='
%left '+' '-'
%left '*' '/' '%' mod div
%nonassoc not
%nonassoc '^' --TODO check
%nonassoc '['
%nonassoc '.'

-- TODO
-- unary minus sign
%%
START :: { STARTN }
START : IMPORTS OUTSIDEFUNCTION         { STARTN (reverse $1) $2 }


IMPORTS :: { [IMPORTN] }
IMPORTS : IMPORTS newline IMPORT        { (IMPORTN $3):$1 }
        | {- empty -}                   { [] }

IMPORT :: { [Id] }
IMPORT : aroundtheworld IDS             { reverse $2 }

IDS :: { [Id] }
IDS : IDS ',' id                             { (tknString $3):$1 }
  | id                                       { [tknString $1] }


OUTSIDEFUNCTION :: { [OUTSIDEN] }
OUTSIDEFUNCTION : FUNCTIONINIC OUTSIDEFUNCTION            { (OUTFUNCTIONINIC $1):$2 }
                | DECLARATION newline OUTSIDEFUNCTION     { (OUTASSIGN $1):$3 }
                | {- empty -}                             { [] }


FUNCTIONINIC :: { FUNCTIONINICN }
FUNCTIONINIC : FUNCTIONSIGN BLOCK    { let (tkn, prms, ret) = $1 in FUNCTIONINICN (tknString tkn) prms ret $2 } --TODO modify Function symbol

FUNCTIONSIGN :: { (Token, [Parameter], OKType) }
FUNCTIONSIGN : dafunk BEGIN id '(' LPARAMETERSFUNC ')' ':' RETURNTYPE {%
        do
            addFuncToSymTable (OKFunc (map param_type $5) $8) (tknString $3) (tknPos $3) --TODO we need to save the list of the param names
            return ($3, $5, $8)}

RETURNTYPE :: { OKType }
RETURNTYPE: intothevoid                                                 { OKVoid }
          | TYPE                                                        { $1 }

LPARAMETERSFUNC :: { [Parameter] }
LPARAMETERSFUNC : {- empty -}                                           { [] }
                | NONEMPTYLPARAMETERSFUNC                               { reverse $1 }

NONEMPTYLPARAMETERSFUNC :: { [Parameter] }
NONEMPTYLPARAMETERSFUNC : NONEMPTYLPARAMETERSFUNC ',' FUNCTIONPARAMETER { $3:($1) }
                        | FUNCTIONPARAMETER                             { [$1] }

FUNCTIONPARAMETER :: { Parameter }
FUNCTIONPARAMETER : TYPE id
                          {% do
                             scope <- stateScopeStackTop
                             addToSymTable $1 (tknString $2) (tknPos $2)
                             return $ Parameter $1 (tknString $2, scope) }

BLOCK :: { [INSTRUCTIONN] }
BLOCK : MAYBELINE youbegin MAYBELINE INSIDEFUNCTION END                    { reverse $4 }
      -- | MAYBELINE INSTRUCTION                                           { [] } -- TODO

BEGIN : {- empty -}                                                       {% stateBeginScope }

END : whereiend                                                           {% stateEndScope }

INSIDEFUNCTION :: { [INSTRUCTIONN] }
INSIDEFUNCTION : INSIDEFUNCTION INSTRUCTION                             { $2:$1 }
              | INSIDEFUNCTION DECLARATION newline                      { (map EXPRESSIONNINST $ reverse $2)++$1 }
              | {- empty -}                                             { [] }


-- Get all the assignments and then do the declarations
DECLARATION :: { [EXPRESSIONN] } -- All Expressions are assignments
DECLARATION : TYPE DECLARATIONVARS {%
      do  let decls = reverse $2
              assigns = filter (isJust.snd) decls
              vars = map (\x -> (tknString.fst $ x, tknPos.fst $ x)) decls
          mapM_ (uncurry (addToSymTable $1)) vars
          mapM createAssign (map (\(x,y) -> (x,fromJust y)) assigns)
}


-- TODO Arrays and other things
-- Symbols are added in parent rule
DECLARATIONVARS :: { [(Token, Maybe EXPRESSIONN)] }
DECLARATIONVARS : id '=' EXPRESSION                 { [($1, Just $3)] }
            | DECLARATIONVARS ',' id '=' EXPRESSION { ($3, Just $5):($1) }
            | id                                    { [($1, Nothing)] }
            | DECLARATIONVARS ',' id                { ($3, Nothing):($1) }


TYPE :: { OKType }
TYPE : TYPE2              { $1 }
    |  TYPE '^'          { OKPointer $1 }

TYPE2 :: { OKType }
TYPE2 : int                                    { OKint }
   | float                                    { OKfloat }
   | boolean                                  { OKboolean }
   | char                                     { OKchar }
   | string                                   { OKstring }
   | id                                       { OKNameType $ tknString $1 }


INSTRUCTION :: { INSTRUCTIONN }
INSTRUCTION : go '(' PRINT ')' newline                                          { GOINGN $ reverse $3 }
            | goslowly '(' PRINT ')' newline                                    { GOINGSLOWLYN $ reverse $3 }
            | gomental '(' PRINT ')' newline                                    { GOINGMENTALN $ reverse $3 }
            | readmymind '(' LVALS ')' newline                                  { READMYMINDN $3 }
            | amnesiac '(' id ')' newline                                       { AMNESIACN $ tknString $3 }
            | if EXPRESSION BEGIN BLOCK IFELSE                                  { IFN $2 (reverse $4) $5 }
            | cantstop EXPRESSION BEGIN BLOCK                                   { CANTSTOPN $2 (reverse $4) }
            | onemoretime BEGIN DECLARATION ';' EXPRESSION ';' EXPRESSION BLOCK { ONEMORETIMEN $3 $5 $7 (reverse $8) }
            | getback EXPRESSION newline                                        { GETBACKN $2 }
            | breakthru newline                                                 { BREAKTHRUN }
            | exitmusic newline                                                 { EXITMUSICN }
            | EXPRESSION newline                                                { EXPRESSIONNINST $1 }

IFELSE : ifyouhavetoask EXPRESSION BEGIN BLOCK IFELSE                           { IFASKN $2 $4 $5 }
       | otherside BEGIN BLOCK                                                  { OTHERSIDEN $3 }
       | {- empty -}                                                            { IFELSEVOID }

PRINT : PRINT ',' EXPRESSION                     { (PRINTSTRING $3):($1)  }
      | EXPRESSION                               { [PRINTSTRING $ $1] }

EXPRESSION :: { EXPRESSIONN }
EXPRESSION : id
                {% do
                    scope <- stateFindSymScope (tknString $1) (tknPos $1)
                    return $ IDEXPRESSION $ (tknString $1, scope) }
           | n                          { NUMBEREXPN $ tknString $1 }
           | string                     { STRINGEXPN $ tknString $1 }
           | c                          { CHAREXP $ tknChar $1 }
           | ok                         { OKN }
           | notok                      { NOTOKN }
           | '(' EXPRESSION ')'         { PARENTESISN $2 }
           | EXPRESSION '<' EXPRESSION  { COMPARN $1 "<" $3 }
           | EXPRESSION '>' EXPRESSION  { COMPARN $1 ">" $3 }
           | EXPRESSION '<=' EXPRESSION { COMPARN $1 "<=" $3 }
           | EXPRESSION '>=' EXPRESSION { COMPARN $1 ">=" $3 }
           | EXPRESSION '==' EXPRESSION { COMPARN $1 "==" $3 }
           | EXPRESSION '!=' EXPRESSION { COMPARN $1 "!=" $3 }
           | not EXPRESSION             { NOTN $2  }
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
           | ARRAYPOSITION              { ARRAYINSTN $1 } -- TODO check sym
           | EXPRESSIONSTRUCT           { EXPSTRUCTN $1 } -- TODO check sym
           | FUNCTIONCALL               { FUNCCALLN $1 }
           | newlife '(' EXPRESSION ')' { NEWLIFEN $3 }
           | '^' EXPRESSION             { POINTERN $ $2 }
           | LVAL '=' EXPRESSION        { ASSIGNN $1 $3 }

EXPRESSIONS :: { [EXPRESSIONN] }
EXPRESSIONS :                       { [] }
            | NONEMPTYEXPRESSIONS   { reverse $ $1 }

NONEMPTYEXPRESSIONS :: { [EXPRESSIONN] }
NONEMPTYEXPRESSIONS : NONEMPTYEXPRESSIONS ',' EXPRESSION        { $3 : $1 }
                    | EXPRESSION                                { [$1] }

-- Anything with L-Value: variables, record.member, array[position]...
LVAL :: { SymId }
LVAL : id                               {% do
                                          scope <- stateFindSymScope (tknString $1) (tknPos $1)
                                          return (tknString $1, scope)}

LVALS :: { [SymId] }
LVALS :                                 { [] }
      | NONEMPTYLVALS                   { reverse ($1) }

NONEMPTYLVALS :: { [SymId] }
NONEMPTYLVALS : NONEMPTYLVALS ',' LVAL    { $3 : $1 }
              | LVAL                      { [$1] }

-- Things that can evaluate to array:
-- id, struct.member
ARRAYPOSITION : EXPRESSION '[' EXPRESSION ']'                   { ARRAYPOSN $1 $3 }

EXPRESSIONSTRUCT : EXPRESSION '.' id                            { EXPRESSIONSTRUCTN $1 (tknString $3) }

FUNCTIONCALL : id '(' EXPRESSIONS ')'                                {%
                                                    do  scope <- stateFindSymScope (tknString $1) (tknPos $1)
                                                        return $ FUNCTIONCALLN (tknString $1) $3} -- TODO Flat

MAYBELINE : {- empty -}                   { }
          | newline                       { }

{

addToSymTable :: OKType -> Id -> Pos -> ParseM ()
addToSymTable t id pos = do
        scope <- stateScopeStackTop
        stateInsertSym $ Sym scope id pos t [] -- TODO real symbol (type...)

addFuncToSymTable :: OKType -> Id -> Pos -> ParseM ()
addFuncToSymTable t id pos = do
        stateInsertSym $ Sym 1 id pos t [] -- TODO real symbol (type...)

createAssign :: (Token, EXPRESSIONN) -> ParseM (EXPRESSIONN)
createAssign (tkn, exp) = do
        scope <- stateFindSymScope (tknString tkn) (tknPos tkn)
        return $ ASSIGNN (tknString tkn, scope) exp

lexwrap :: (Token -> ParseM a) -> ParseM a
lexwrap cont = do
  tkn <- alexGetToken
  --liftIO $ putStrLn $ "    " ++ show tkn
  cont tkn
}
