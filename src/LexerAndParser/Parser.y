-- OKComputer
-- Parser

{
module Parser where

import LowLevelAlex
import Tokens
import Lexer
import ParseMonad (ParseM, ParseMError(..))
import qualified ParseMonad as P
import qualified AST
import AST (exp_type, param_type, param_id)
import SymTable
import Scope
import OKTypes

import Control.Monad.Except
import Data.Maybe
}

%name parse
%tokentype { Token }
%monad { ParseM }
%error { (throwError . ParseError . show) }
%lexer { lexwrap }{ EOFTkn }

-- Tokens
-- {{{1
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

-- 1}}}

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

-- Grammar
-- {{{1
%%
START :: { AST.START }
START : IMPORTS OUTSIDEFUNCTION         { AST.START (reverse $1) $2 }


IMPORTS :: { [AST.IMPORT] }
IMPORTS : IMPORTS newline IMPORT        { (AST.IMPORT $3):$1 }
        | {- empty -}                   { [] }

IMPORT :: { [Id] }
IMPORT : aroundtheworld IDS             { reverse $2 }

IDS :: { [Id] }
IDS : IDS ',' id                             { (tkn_string $3):$1 }
  | id                                       { [tkn_string $1] }


OUTSIDEFUNCTION :: { [AST.OUTSIDE] }
OUTSIDEFUNCTION : FUNCTIONDEF OUTSIDEFUNCTION            { $2 }
                | DECLARATION newline OUTSIDEFUNCTION     { (AST.OUTASSIGN $1):$3 }
                | {- empty -}                             { [] }

FUNCTIONDEF :: { () }
FUNCTIONDEF : FUNCTIONSIGN BLOCK {% functionDefAction $1 $2}

FUNCTIONSIGN :: { (Token, [AST.Parameter], OKType) }
FUNCTIONSIGN : dafunk BEGIN id '(' LPARAMETERSFUNC ')' ':' RETURNTYPE     {% functionSignAction $3 $5 $8 }
-- Creates the function symbol and inserts it to the sym table

RETURNTYPE :: { OKType }
RETURNTYPE: intothevoid                                                 { OKVoid }
          | TYPE                                                        { $1 }

LPARAMETERSFUNC :: { [AST.Parameter] }
LPARAMETERSFUNC : {- empty -}                                           { [] }
                | NONEMPTYLPARAMETERSFUNC                               { reverse $1 }

NONEMPTYLPARAMETERSFUNC :: { [AST.Parameter] }
NONEMPTYLPARAMETERSFUNC : NONEMPTYLPARAMETERSFUNC ',' FUNCTIONPARAMETER { $3:($1) }
                        | FUNCTIONPARAMETER                             { [$1] }

FUNCTIONPARAMETER :: { AST.Parameter }
FUNCTIONPARAMETER : TYPE id         {% functionParameterAction $1 $2}

BLOCK :: { [AST.INSTRUCTION] }
BLOCK : MAYBELINE youbegin MAYBELINE INSIDEFUNCTION END                    { reverse $4 }
      -- | MAYBELINE INSTRUCTION                                           { [] } -- TODO

BEGIN : {- empty -}                                                       {% P.beginScope }

END : whereiend                                                           {% P.endScope }

INSIDEFUNCTION :: { [AST.INSTRUCTION] }
INSIDEFUNCTION : INSIDEFUNCTION INSTRUCTION                             { $2:$1 }
              | INSIDEFUNCTION DECLARATION newline                      { (map AST.EXPRESSIONINST $ reverse $2)++$1 }
              | {- empty -}                                             { [] }


-- Get all the assignments and then do the declarations
DECLARATION :: { [AST.EXPRESSION] } -- All Expressions are assignments
DECLARATION : TYPE DECLARATIONVARS {% declarationAction $1 $2 }


-- TODO Arrays and other things
-- Symbols are added in parent rule
DECLARATIONVARS :: { [(Token, Maybe AST.EXPRESSION)] }
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
   | id                                       { OKNameType $ tkn_string $1 }


INSTRUCTION :: { AST.INSTRUCTION }
INSTRUCTION : go '(' PRINT ')' newline                                          { AST.GOING $ reverse $3 }
            | goslowly '(' PRINT ')' newline                                    { AST.GOINGSLOWLY $ reverse $3 }
            | gomental '(' PRINT ')' newline                                    { AST.GOINGMENTAL $ reverse $3 }
            | readmymind '(' LVALS ')' newline                                  { AST.READMYMIND $3 }
            | amnesiac '(' id ')' newline                                       { AST.AMNESIAC $ tkn_string $3 }
            | if EXPRESSION BEGIN BLOCK IFELSE                                  {% ifAction $1 $2 (reverse $4) $5 }
            | cantstop EXPRESSION BEGIN BLOCK                                   {% cantStopAction $1 $2 (reverse $4) }
            | onemoretime BEGIN DECLARATION ';' EXPRESSION ';' EXPRESSION BLOCK {% oneMoreTimeAction $1 $5 $3 $7 (reverse $8) }
            | getback EXPRESSION newline                                        { AST.GETBACK $2 } --TODO should we check we're inside a function with that type?
            | breakthru newline                                                 { AST.BREAKTHRU }
            | exitmusic newline                                                 { AST.EXITMUSIC }
            | EXPRESSION newline                                                { AST.EXPRESSIONINST $1 }

IFELSE : ifyouhavetoask EXPRESSION BEGIN BLOCK IFELSE                           {% ifYouHaveToAskAction $1 $2 (reverse $4) $5 }
       | otherside BEGIN BLOCK                                                  { AST.OTHERSIDE $3 }
       | {- empty -}                                                            { AST.IFELSEVOID }

PRINT : PRINT ',' EXPRESSION                     { (AST.PRINTSTRING $3):($1)  }
      | EXPRESSION                               { [AST.PRINTSTRING $ $1] }

EXPRESSION :: { AST.EXPRESSION }
EXPRESSION : id {% do
                    sym <- P.findSym (tkn_string $1) (tkn_pos $1)
                    return $ AST.IDEXPRESSION (tkn_string $1, sym_scope sym) (sym_type sym)}
           | n                          { AST.NUMBEREXP (tkn_string $1) OKFloat}
           | string                     { AST.STRINGEXP (tkn_string $1) OKString}
           | c                          { AST.CHAREXP (tkn_char $1) OKChar}
           | ok                         { AST.BOOLEANEXP True OKBoolean}
           | notok                      { AST.BOOLEANEXP False OKBoolean}
           | '(' EXPRESSION ')'         { $2 }
           | EXPRESSION '<' EXPRESSION  {% (checkOrdCompType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.COMPAR $1 "<" $3 }
           | EXPRESSION '>' EXPRESSION  {% (checkOrdCompType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.COMPAR $1 ">" $3 }
           | EXPRESSION '<=' EXPRESSION {% (checkOrdCompType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.COMPAR $1 "<=" $3 }
           | EXPRESSION '>=' EXPRESSION {% (checkOrdCompType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.COMPAR $1 ">=" $3 }
           | EXPRESSION '==' EXPRESSION {% (checkCompType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.COMPAR $1 "==" $3 }
           | EXPRESSION '!=' EXPRESSION {% (checkCompType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.COMPAR $1 "!=" $3 }
           | not EXPRESSION             {% (checkSameType (tkn_pos $1) (exp_type $2) OKBoolean) >>= return . AST.NOT $2 }
           | EXPRESSION and EXPRESSION  {% (checkBooleanOpType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.LOGIC $1 "and" $3 }
           | EXPRESSION or EXPRESSION   {% (checkBooleanOpType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.LOGIC $1 "or" $3 }
           | '-' EXPRESSION             {% (checkSameType (tkn_pos $1) (exp_type $2) OKFloat) >>= return . AST.MINUS $2 }
           | EXPRESSION '+' EXPRESSION  {% (checkNumOpType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.ARIT $1 "+" $3 }
           | EXPRESSION '-' EXPRESSION  {% (checkNumOpType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.ARIT $1 "-" $3 }
           | EXPRESSION '*' EXPRESSION  {% (checkNumOpType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.ARIT $1 "*" $3 }
           | EXPRESSION '/' EXPRESSION  {% (checkNumOpType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.ARIT $1 "/" $3 }
           | EXPRESSION '%' EXPRESSION  {% (checkNumOpType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.ARIT $1 "%" $3 }
           | EXPRESSION mod EXPRESSION  {% (checkIntOpType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.ARIT $1 "mod" $3 }
           | EXPRESSION div EXPRESSION  {% (checkIntOpType (tkn_pos $2) (exp_type $1) (exp_type $3)) >>= return . AST.ARIT $1 "div" $3 }
           | ARRAYPOSITION              { $1 } -- TODO check sym
           | EXPRESSIONSTRUCT           { $1 } -- TODO check sym
           | FUNCTIONCALL               { $1 }
           | newlife '(' EXPRESSION ')' { AST.NEWLIFE $3 $ OKPointer (exp_type $3)} -- TODO ??????????????????????
           | '^' EXPRESSION             {% (checkAndGetPointerType (tkn_pos $1) (exp_type $2)) >>= return . AST.POINTER $2 }
           | LVAL '=' EXPRESSION        { AST.ASSIGN $1 $3 (exp_type $3)} --Lookup for type

EXPRESSIONS :: { [AST.EXPRESSION] }
EXPRESSIONS :                       { [] }
            | NONEMPTYEXPRESSIONS   { reverse $ $1 }

NONEMPTYEXPRESSIONS :: { [AST.EXPRESSION] }
NONEMPTYEXPRESSIONS : NONEMPTYEXPRESSIONS ',' EXPRESSION        { $3 : $1 }
                    | EXPRESSION                                { [$1] }

-- Anything with L-Value: variables, record.member, array[position]...
LVAL :: { SymId }
LVAL : id                               {% do
                                          scope <- P.findSymScope (tkn_string $1) (tkn_pos $1)
                                          return (tkn_string $1, scope)}

LVALS :: { [SymId] }
LVALS :                                 { [] }
      | NONEMPTYLVALS                   { reverse ($1) }

NONEMPTYLVALS :: { [SymId] }
NONEMPTYLVALS : NONEMPTYLVALS ',' LVAL    { $3 : $1 }
              | LVAL                      { [$1] }

-- Things that can evaluate to array:
-- id, struct.member
ARRAYPOSITION : EXPRESSION '[' EXPRESSION ']'                   { AST.ARRAYPOS $1 $3 (array_Type.exp_type $ $1)} -- TODO Check E1 type for array

                                                                    --TODO find type of E1, has to be a record, add its scope in the dot, look id, pop scope
EXPRESSIONSTRUCT : EXPRESSION '.' id                            { AST.EXPRESSIONSTRUCT $1 (tkn_string $3) (exp_type $1)}

FUNCTIONCALL : id '(' EXPRESSIONS ')'                                {%
                                                    do  sym <- P.findSym (tkn_string $1) (tkn_pos $1)
                                                        return $ AST.FUNCTIONCALL (tkn_string $1) $3 (func_RetType.sym_type $ sym)}

MAYBELINE : {- empty -}                   { }
          | newline                       { }

--- 1}}}
{

addToSymTable :: OKType -> Id -> Pos -> ParseM ()
addToSymTable t id pos = do
        scope <- P.topScope
        P.insertSym $ Sym scope id pos t -- TODO real symbol (type...)

-- Actions
-- {{{1
functionDefAction :: (Token, [AST.Parameter], OKType) -> [AST.INSTRUCTION] -> ParseM ()
functionDefAction (tkn, params, ret) instrs = do
        let oktype = OKFunc (map param_type params) ret
        P.completeFunctionDef tkn oktype instrs
        return ()

functionSignAction :: Token -> [AST.Parameter] -> OKType -> ParseM (Token, [AST.Parameter], OKType)
functionSignAction tkn params ret = do
        let oktype = OKFunc (map param_type params) ret
            id = tkn_string tkn
            pos = tkn_pos tkn
            param_ids = map param_id params
        P.insertSym $ FuncSym 1 id pos oktype param_ids []
        return (tkn, params, ret)

functionParameterAction :: OKType -> Token -> ParseM AST.Parameter
functionParameterAction oktype id = do
                             scope <- P.topScope
                             addToSymTable oktype (tkn_string id) (tkn_pos id)
                             return $ AST.Parameter oktype (tkn_string id, scope)

declarationAction :: OKType -> [(Token, Maybe AST.EXPRESSION)] -> ParseM ([AST.EXPRESSION])
declarationAction t l =
      do  let decls = reverse l
              assigns = filter (isJust.snd) decls
              vars = map (\x -> (tkn_string.fst $ x, tkn_pos.fst $ x)) decls
          mapM_ (uncurry (addToSymTable t)) vars
          mapM createAssign (map (\(x,y) -> (x,fromJust y)) assigns)
  where
    createAssign :: (Token, AST.EXPRESSION) -> ParseM (AST.EXPRESSION)
    createAssign (tkn, exp) = do
            sym <- P.findSym (tkn_string tkn) (tkn_pos tkn)
            --TODO Checktype of Sym with type of exp
            return $ AST.ASSIGN (tkn_string tkn, sym_scope sym) exp (exp_type exp)

ifAction :: Token -> AST.EXPRESSION -> [AST.INSTRUCTION] -> AST.IFELSE -> ParseM AST.INSTRUCTION
ifAction tkn condition blk ifelse = do
          checkExpectedType (tkn_pos tkn) OKBoolean (exp_type condition)
          return $ AST.IF condition blk ifelse

cantStopAction :: Token -> AST.EXPRESSION -> [AST.INSTRUCTION] -> ParseM AST.INSTRUCTION
cantStopAction tkn condition blk = do
          checkExpectedType (tkn_pos tkn) OKBoolean (exp_type condition)
          return $ AST.CANTSTOP condition blk

-- TODO The step should be an instruccion?
oneMoreTimeAction :: Token -> AST.EXPRESSION -> [AST.EXPRESSION] -> AST.EXPRESSION -> [AST.INSTRUCTION] -> ParseM AST.INSTRUCTION
oneMoreTimeAction tkn condition init step blk = do
          checkExpectedType (tkn_pos tkn) OKBoolean (exp_type condition)
          return $ AST.ONEMORETIME init condition step blk


ifYouHaveToAskAction :: Token -> AST.EXPRESSION -> [AST.INSTRUCTION] -> AST.IFELSE -> ParseM AST.IFELSE
ifYouHaveToAskAction tkn condition blk ifelse = do
          checkExpectedType (tkn_pos tkn) OKBoolean (exp_type condition)
          return $ AST.IFASK condition blk ifelse

--- 1}}}

-- Type Checking
-- {{{1
checkExpectedType :: Pos -> OKType -> OKType -> ParseM OKType
checkExpectedType pos oktype found = do
    case found of
         OKErrorT -> return OKErrorT
         oktype -> return oktype
         _ -> throwNotWhatIExpectedAndImNotSatisfied pos oktype found

checkSameType :: Pos -> OKType -> OKType -> ParseM (OKType)
checkSameType (line, _) OKErrorT _ = return OKErrorT
checkSameType (line, _) _ OKErrorT = return OKErrorT
checkSameType (line, _) t1 t2 = if (t1 /= t2) then throwDifferentTypeError line t1 t2
                                              else return t1

checkOrdCompType :: Pos -> OKType -> OKType -> ParseM (OKType)
checkOrdCompType (line, _) OKErrorT _ = return OKErrorT
checkOrdCompType (line, _) _ OKErrorT = return OKErrorT
checkOrdCompType (line, _) t1 t2 = if t1 /= t2 then throwDifferentTypeError line t1 t2
                                               else if isNumericalType t1 then return OKBoolean
                                                    else throwNotNumericalTypeError line t1 t2

checkCompType :: Pos -> OKType -> OKType -> ParseM (OKType)
checkCompType (line, _) OKErrorT _ = return OKErrorT
checkCompType (line, _) _ OKErrorT = return OKErrorT
checkCompType (line, _) t1 t2 = if t1 == t2 then return OKBoolean
                                            else throwDifferentTypeError line t1 t2

checkBooleanOpType :: Pos -> OKType -> OKType -> ParseM (OKType)
checkBooleanOpType (line, _) OKErrorT _ = return OKErrorT
checkBooleanOpType (line, _) _ OKErrorT = return OKErrorT
checkBooleanOpType (line, _) t1 t2 = if t1 == t2 && t1 == OKBoolean then return OKBoolean
                                                                    else throwNotBooleanError line t1 t2

checkNumOpType :: Pos -> OKType -> OKType -> ParseM (OKType)
checkNumOpType (line, _) OKErrorT _ = return OKErrorT
checkNumOpType (line, _) _ OKErrorT = return OKErrorT
checkNumOpType (line, _) t1 t2 = if t1 == t2 && isNumericalType t1 then return t1
                                                                   else throwNotNumericalTypeError line t1 t2


checkIntOpType :: Pos -> OKType -> OKType -> ParseM (OKType)
checkIntOpType (line, _) OKErrorT _ = return OKErrorT
checkIntOpType (line, _) _ OKErrorT = return OKErrorT
checkIntOpType (line, _) t1 t2 = if t1 == t2 && t1 == OKInt then return t1
                                                            else throwNotIntType line t1 t2

checkAndGetPointerType :: Pos -> OKType -> ParseM (OKType)
checkAndGetPointerType (line, _) OKErrorT = return OKErrorT
checkAndGetPointerType (line, _) (OKPointer t) = return t
checkAndGetPointerType (line, _) t = throwNotPointerType line t

-- 1}}}

-- Errors (Horrors)
-- {{{1
throwDifferentTypeError line t1 t2 = do
    liftIO $ putStrLn $ "Line " ++ show line ++ ". Expected same type, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    return OKErrorT

throwNotNumericalTypeError line t1 t2 = do
    liftIO $ putStrLn $ "Line " ++ show line ++ ". Expected a number types, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    return OKErrorT

throwNotWhatIExpectedAndImNotSatisfied line t1 t2 = do
    liftIO $ putStrLn $ "Line " ++ show line ++ ". Expected " ++ show t1 ++ ", found " ++ show t2 ++ "."
    liftIO $ putStrLn $ "Not What I Expected And I'm Not Satisfied."
    return OKErrorT

throwNotBooleanError line t1 t2 = do
    liftIO $ putStrLn $ "Line " ++ show line ++ ". Expected boolean values, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    return OKErrorT

throwNotIntType line t1 t2 = do
    liftIO $ putStrLn $ "Line " ++ show line ++ ". Expected int values, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    return OKErrorT

throwNotPointerType line t = do
    liftIO $ putStrLn $ "Line " ++ show line ++ ". Expected pointer value, found " ++ show t ++ "."
    return OKErrorT

--- 1}}}

lexwrap :: (Token -> ParseM a) -> ParseM a
lexwrap cont = do
  tkn <- alexGetToken
  --liftIO $ putStrLn $ "    " ++ show tkn
  cont tkn
}
