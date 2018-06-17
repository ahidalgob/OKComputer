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
  youbegin                                { YouBeginTkn _ }        -- Block Start
  whereiend                               { WhereIEndTkn _ }       -- Block End
  if                                      { IfTkn _ }              -- Selection
  ifyouhavetoask                          { IfYouHaveToAskTkn _ }  -- Selection
  otherside                               { OthersideTkn _ }       -- Selection
  cantstop                                { CantStopTkn _ }        -- While Iteration
  breakthru                               { BreakthruTkn _ }       -- Break
  onemoretime                             { OneMoreTimeTkn _ }     -- For Iteration
  ';'                                     { SemiColonTkn _ }       -- For Iteration
  readmymind                              { ReadMyMindTkn _ }      -- Data entry/read
  go                                      { GoTkn _ }              -- Data exit/write
  gomental                                { GoMentalTkn _ }
  goslowly                                { GoSlowlyTkn _ }        -- Data exit/writeln
  dafunk                                  { DaFunkTkn _}          -- Method with return/Function
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
  float                                   { FloatTkn _ }
  char                                    { CharTkn _ }
  boolean                                 { BooleanTkn _ }
  ok                                      { OkTkn _ }              -- True
  notok                                   { NotOkTkn _ }           -- False
  --'{'                                     { OpenBraceTkn _ }
  --'}'                                     { CloseBraceTkn _ }
  '['                                     { ArrayStartTkn _ }
  ']'                                     { ArrayEndTkn _ }
  --band                                    { BandTkn _ }            -- Registers/structs
  --union                                   { UnionTkn _ }
  '.'										                  { DotTkn _ }
  '^'									                    { PointerTkn _ }

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
DECLARATION : TYPE DECLARATIONVARS {% declarationAction $1 $2 }


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
TYPE2 : int                                    { OKInt }
   | float                                    { OKFloat }
   | boolean                                  { OKBoolean }
   | char                                     { OKChar }
   | string                                   { OKString }
   | id                                       { OKNameType $ tknString $1 }


INSTRUCTION :: { INSTRUCTIONN }
INSTRUCTION : go '(' PRINT ')' newline                                          { GOINGN $ reverse $3 }
            | goslowly '(' PRINT ')' newline                                    { GOINGSLOWLYN $ reverse $3 }
            | gomental '(' PRINT ')' newline                                    { GOINGMENTALN $ reverse $3 }
            | readmymind '(' LVALS ')' newline                                  { READMYMINDN $3 }
            | amnesiac '(' id ')' newline                                       { AMNESIACN $ tknString $3 }
            | if EXPRESSION BEGIN BLOCK IFELSE                                  {%
                                                                              checkSameType (tknPos $1) (expType $2) OKBoolean >> (return $ IFN $2 (reverse $4) $5) }
            | cantstop EXPRESSION BEGIN BLOCK                                   {%
                                                                              checkSameType (tknPos $1) (expType $2) OKBoolean >> (return $ CANTSTOPN $2 (reverse $4)) }
            | onemoretime BEGIN DECLARATION ';' EXPRESSION ';' EXPRESSION BLOCK {%
                                                                              checkSameType (tknPos $1) (expType $5) OKBoolean >> (return $ ONEMORETIMEN $3 $5 $7 (reverse $8)) }
            | getback EXPRESSION newline                                        { GETBACKN $2 } --TODO should we check we're inside a function with that type?
            | breakthru newline                                                 { BREAKTHRUN }
            | exitmusic newline                                                 { EXITMUSICN }
            | EXPRESSION newline                                                { EXPRESSIONNINST $1 }

IFELSE : ifyouhavetoask EXPRESSION BEGIN BLOCK IFELSE                           {%
                                                                              checkSameType (tknPos $1) (expType $2) OKBoolean >> (return $ IFASKN $2 $4 $5) }
       | otherside BEGIN BLOCK                                                  { OTHERSIDEN $3 }
       | {- empty -}                                                            { IFELSEVOID }

PRINT : PRINT ',' EXPRESSION                     { (PRINTSTRING $3):($1)  }
      | EXPRESSION                               { [PRINTSTRING $ $1] }

EXPRESSION :: { EXPRESSIONN }
EXPRESSION : id {% do
                    sym <- stateFindSym (tknString $1) (tknPos $1)
                    return $ IDEXPRESSION (tknString $1, sym_scope sym) (sym_type sym)}
           | n                          { NUMBEREXPN (tknString $1) OKFloat}
           | string                     { STRINGEXPN (tknString $1) OKString}
           | c                          { CHAREXPN (tknChar $1) OKChar}
           | ok                         { BOOLEANEXPN True OKBoolean}
           | notok                      { BOOLEANEXPN False OKBoolean}
           | '(' EXPRESSION ')'         { PARENTESISN $2 (expType $2)}
           | EXPRESSION '<' EXPRESSION  {% (checkOrdCompType (tknPos $2) (expType $1) (expType $3)) >>= return . COMPARN $1 "<" $3 }
           | EXPRESSION '>' EXPRESSION  {% (checkOrdCompType (tknPos $2) (expType $1) (expType $3)) >>= return . COMPARN $1 ">" $3 }
           | EXPRESSION '<=' EXPRESSION {% (checkOrdCompType (tknPos $2) (expType $1) (expType $3)) >>= return . COMPARN $1 "<=" $3 }
           | EXPRESSION '>=' EXPRESSION {% (checkOrdCompType (tknPos $2) (expType $1) (expType $3)) >>= return . COMPARN $1 ">=" $3 }
           | EXPRESSION '==' EXPRESSION {% (checkCompType (tknPos $2) (expType $1) (expType $3)) >>= return . COMPARN $1 "==" $3 }
           | EXPRESSION '!=' EXPRESSION {% (checkCompType (tknPos $2) (expType $1) (expType $3)) >>= return . COMPARN $1 "!=" $3 }
           | not EXPRESSION             {% (checkSameType (tknPos $1) (expType $2) OKBoolean) >>= return . NOTN $2 }
           | EXPRESSION and EXPRESSION  {% (checkBooleanOpType (tknPos $2) (expType $1) (expType $3)) >>= return . LOGICN $1 "and" $3 }
           | EXPRESSION or EXPRESSION   {% (checkBooleanOpType (tknPos $2) (expType $1) (expType $3)) >>= return . LOGICN $1 "or" $3 }
           | '-' EXPRESSION             {% (checkSameType (tknPos $1) (expType $2) OKFloat) >>= return . MINUSN $2 }
           | EXPRESSION '+' EXPRESSION  {% (checkNumOpType (tknPos $2) (expType $1) (expType $3)) >>= return . ARITN $1 "+" $3 }
           | EXPRESSION '-' EXPRESSION  {% (checkNumOpType (tknPos $2) (expType $1) (expType $3)) >>= return . ARITN $1 "-" $3 }
           | EXPRESSION '*' EXPRESSION  {% (checkNumOpType (tknPos $2) (expType $1) (expType $3)) >>= return . ARITN $1 "*" $3 }
           | EXPRESSION '/' EXPRESSION  {% (checkNumOpType (tknPos $2) (expType $1) (expType $3)) >>= return . ARITN $1 "/" $3 }
           | EXPRESSION '%' EXPRESSION  {% (checkNumOpType (tknPos $2) (expType $1) (expType $3)) >>= return . ARITN $1 "%" $3 }
           | EXPRESSION mod EXPRESSION  {% (checkIntOpType (tknPos $2) (expType $1) (expType $3)) >>= return . ARITN $1 "mod" $3 }
           | EXPRESSION div EXPRESSION  {% (checkIntOpType (tknPos $2) (expType $1) (expType $3)) >>= return . ARITN $1 "div" $3 }
           | ARRAYPOSITION              { $1 } -- TODO check sym
           | EXPRESSIONSTRUCT           { $1 } -- TODO check sym
           | FUNCTIONCALL               { $1 }
           | newlife '(' EXPRESSION ')' { NEWLIFEN $3 $ OKPointer (expType $3)} -- TODO ??????????????????????
           | '^' EXPRESSION             {% (checkAndGetPointerType (tknPos $1) (expType $2)) >>= return . POINTERN $2 }
           | LVAL '=' EXPRESSION        { ASSIGNN $1 $3 (expType $3)} --Lookup for type

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
ARRAYPOSITION : EXPRESSION '[' EXPRESSION ']'                   { ARRAYPOSN $1 $3 (arrayType.expType $ $1)}

EXPRESSIONSTRUCT : EXPRESSION '.' id                            { EXPRESSIONSTRUCTN $1 (tknString $3) (expType $1)} --TODO NOOOOOOOOOOOOOOOOOOOOOOOOO

FUNCTIONCALL : id '(' EXPRESSIONS ')'                                {%
                                                    do  sym <- stateFindSym (tknString $1) (tknPos $1)
                                                        return $ FUNCTIONCALLN (tknString $1) $3 (funcRetType.sym_type $ sym)} -- TODO Flat

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

declarationAction :: OKType -> [(Token, Maybe EXPRESSIONN)] -> ParseM ([EXPRESSIONN])
declarationAction t l =
      do  let decls = reverse l
              assigns = filter (isJust.snd) decls
              vars = map (\x -> (tknString.fst $ x, tknPos.fst $ x)) decls
          mapM_ (uncurry (addToSymTable t)) vars
          mapM createAssign (map (\(x,y) -> (x,fromJust y)) assigns)

createAssign :: (Token, EXPRESSIONN) -> ParseM (EXPRESSIONN)
createAssign (tkn, exp) = do
        scope <- stateFindSymScope (tknString tkn) (tknPos tkn)
        --TODO Checktype of Sym with type of exp
        return $ ASSIGNN (tknString tkn, scope) exp (expType exp)


checkSameType :: Pos -> OKType -> OKType -> ParseM (OKType)
checkSameType (line, _) OKErrorT _ = return OKErrorT
checkSameType (line, _) _ OKErrorT = return OKErrorT
checkSameType (line, _) t1 t2 = if (t1 /= t2) then return OKErrorT --TODO throw some error
                                              else return t1

checkOrdCompType :: Pos -> OKType -> OKType -> ParseM (OKType)
checkOrdCompType (line, _) OKErrorT _ = return OKErrorT
checkOrdCompType (line, _) _ OKErrorT = return OKErrorT
checkOrdCompType (line, _) t1 t2 = if t1 == t2 && isNumericalType t2 then return OKBoolean
                                                                     else return OKErrorT --TODO throw some error
checkCompType :: Pos -> OKType -> OKType -> ParseM (OKType)
checkCompType (line, _) OKErrorT _ = return OKErrorT
checkCompType (line, _) _ OKErrorT = return OKErrorT
checkCompType (line, _) t1 t2 = if t1 == t2 then return OKBoolean
                                            else return OKErrorT --TODO throw some error

checkBooleanOpType :: Pos -> OKType -> OKType -> ParseM (OKType)
checkBooleanOpType (line, _) OKErrorT _ = return OKErrorT
checkBooleanOpType (line, _) _ OKErrorT = return OKErrorT
checkBooleanOpType (line, _) t1 t2 = if t1 == t2 && t1 == OKBoolean then return OKBoolean
                                                                    else return OKErrorT --TODO throw some error

checkNumOpType :: Pos -> OKType -> OKType -> ParseM (OKType)
checkNumOpType (line, _) OKErrorT _ = return OKErrorT
checkNumOpType (line, _) _ OKErrorT = return OKErrorT
checkNumOpType (line, _) t1 t2 = if t1 == t2 && isNumericalType t1 then return t1
                                                                   else return OKErrorT --TODO throw some error


checkIntOpType :: Pos -> OKType -> OKType -> ParseM (OKType)
checkIntOpType (line, _) OKErrorT _ = return OKErrorT
checkIntOpType (line, _) _ OKErrorT = return OKErrorT
checkIntOpType (line, _) t1 t2 = if t1 == t2 && t1 == OKInt then return t1
                                                            else return OKErrorT --TODO throw some error

checkAndGetPointerType :: Pos -> OKType -> ParseM (OKType)
checkAndGetPointerType (line, _) OKErrorT = return OKErrorT
checkAndGetPointerType (line, _) (OKPointer t) = return t
checkAndGetPointerType (line, _) _ = return OKErrorT --TODO throw some error

lexwrap :: (Token -> ParseM a) -> ParseM a
lexwrap cont = do
  tkn <- alexGetToken
  --liftIO $ putStrLn $ "    " ++ show tkn
  cont tkn
}
