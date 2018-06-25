-- OKComputer
-- Parser

{
module Parser where

-- imports {{{
import LowLevelAlex
import Tokens
import Lexer
import ParseMonad (ParseM, ParseMError(..))
import qualified ParseMonad as P
import qualified AST
import AST (exp_type)
import SymTable
import Scope
import OKTypes

import Control.Monad.Except
import Data.Maybe
-- }}}

}

%name parse
%tokentype { Token }
%monad { ParseM }
%error { (throwError . ParseError . show) }
%lexer { lexwrap }{ EOFTkn }

-- Tokens{{{1
%token
  typedef                                 { TypedefTkn _ }
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
  '{'                                     { OpenBraceTkn _ }
  '}'                                     { CloseBraceTkn _ }
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
  f                                       { FloatLiteralTkn _ _ }
  n                                       { IntLiteralTkn _ _ }
  newline                                 { NewLineTkn _ }
  c                                       { LiteralCharTkn _ _ } -- char
  string                                  { StringTkn _ _ }

-- 1}}}

-- TODO:
-- tuples in general.
-- char
-- braces
-- string operations
--
-- Array expression  {1,2,3}
-- Array type

%right '='
%nonassoc '!='
%left or
%left and
%nonassoc '>' '<' '==' '!=' '>=' '<='
%left '+' '-'
%left '*' '/' '%' mod div
%right not
%right '^'
%left '['
%nonassoc '.'

-- TODO
-- unary minus sign

-- Grammar{{{1
%%
START :: { AST.START }
START : IMPORTS OUTSIDE_FUNCTION         { AST.START (reverse $1) $2 }


IMPORTS :: { [AST.IMPORT] }
IMPORTS : IMPORTS newline IMPORT        { (AST.IMPORT $3):$1 }
        | {- empty -}                   { [] }

IMPORT :: { [Id] }
IMPORT : aroundtheworld IDS             { reverse $2 }

IDS :: { [Id] }
IDS : IDS ',' id                             { (tkn_string $3):$1 }
  | id                                       { [tkn_string $1] }


OUTSIDE_FUNCTION :: { [AST.OUTSIDE] }
OUTSIDE_FUNCTION : FUNCTION_DEF OUTSIDE_FUNCTION           { $2 }
                | DECLARATION newline OUTSIDE_FUNCTION     { (AST.OUTASSIGN $1):$3 }
                | TYPEDEF OUTSIDE_FUNCTION                 { $2 }
                | {- empty -}                              { [] }

TYPEDEF :: { () }
TYPEDEF : typedef id TYPE newline                          {% typedefAction $2 $3 }

FUNCTION_DEF :: { () }
FUNCTION_DEF : FUNCTION_SIGN BLOCK {% functionDefAction $1 $2}

FUNCTION_SIGN :: { (Token, [Parameter], OKType) }
FUNCTION_SIGN : dafunk BEGIN id '(' LPARAMETERSFUNC ')' ':' RETURNTYPE     {% functionSignAction $3 $5 $8 }
-- Creates the function symbol and inserts it to the sym table

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
FUNCTIONPARAMETER : TYPE id         {% functionParameterAction $1 $2}

BLOCK :: { [AST.INSTRUCTION] }
BLOCK : MAYBELINE youbegin MAYBELINE INSIDEFUNCTION END                    { reverse $4 }

BEGIN : {- empty -}                                                       {% P.beginScope }

END : whereiend                                                           {% P.endScope }

INSIDEFUNCTION :: { [AST.INSTRUCTION] }
INSIDEFUNCTION : INSIDEFUNCTION INSTRUCTION                             { $2:$1 }
              | INSIDEFUNCTION DECLARATION newline                      { (map AST.EXPRESSIONINST $ reverse $2)++$1 }
              | {- empty -}                                             { [] }


-- Get all the assignments and then do the declarations
DECLARATION :: { [AST.EXPRESSION] } -- All Expressions are assignments
DECLARATION : TYPE DECLARATIONVARS {% declarationAction $1 $2 }


-- TODO Arrays, lists, tuples
-- Symbols are added in parent rule
DECLARATIONVARS :: { [(Token, Maybe AST.EXPRESSION, Maybe AST.EXPRESSION)] }
DECLARATIONVARS : id '=' EXPRESSION                                    { [($1, Just $3, Nothing)] }
            | DECLARATIONVARS ',' id '=' EXPRESSION                    { ($3, Just $5, Nothing):($1) }
            | id                                                       { [($1, Nothing, Nothing)] }
            | DECLARATIONVARS ',' id                                   { ($3, Nothing, Nothing):($1) }

            | id '[' EXPRESSION ']' '=' EXPRESSION                     { [($1, Just $6, Just $3)] }
            | DECLARATIONVARS ',' id '[' EXPRESSION ']' '=' EXPRESSION { ($3, Just $8, Just $5):($1) }
            | id '[' EXPRESSION ']'                                    { [($1, Nothing, Just $3)] }
            | DECLARATIONVARS ',' id '[' EXPRESSION ']'                { ($3, Nothing, Just $5):($1) }


TYPE :: { OKType }
TYPE : TYPE '^'                                 { OKPointer $1 }
     | int                                      { OKInt }
     | float                                    { OKFloat }
     | boolean                                  { OKBoolean }
     | char                                     { OKChar }
     | string                                   { OKString }
     | id                                       {% nameTypeAction $1 }




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
EXPRESSION : LVAL                       { $1 }
           | n                          { AST.NUMBEREXP (tkn_string $1) OKInt}
           | f                          { AST.NUMBEREXP (tkn_string $1) OKFloat}
           | string                     { AST.STRINGEXP (tkn_string $1) OKString}
           | c                          { AST.CHAREXP (tkn_char $1) OKChar}
           | ok                         { AST.BOOLEANEXP True OKBoolean}
           | notok                      { AST.BOOLEANEXP False OKBoolean}
           | '(' EXPRESSION ')'         { $2 }
           | '{' NONEMPTYEXPRESSIONS '}'{% arrayLiteralAction $1 (reverse $2) }
           | EXPRESSION '<' EXPRESSION  {% orderCompAction $2 $1 $3 "<" }
           | EXPRESSION '>' EXPRESSION  {% orderCompAction $2 $1 $3 ">" }
           | EXPRESSION '<=' EXPRESSION {% orderCompAction $2 $1 $3 "<=" }
           | EXPRESSION '>=' EXPRESSION {% orderCompAction $2 $1 $3 ">=" }
           | EXPRESSION '==' EXPRESSION {% equalityComparAction $2 $1 $3 "==" }
           | EXPRESSION '!=' EXPRESSION {% equalityComparAction $2 $1 $3 "!=" }
           | not EXPRESSION             {% notAction $1 $2 }
           | EXPRESSION and EXPRESSION  {% booleanOperationAction $2 $1 $3 "and" }
           | EXPRESSION or EXPRESSION   {% booleanOperationAction $2 $1 $3 "or" }
           | '-' EXPRESSION             {% minusAction $1 $2 }
           | EXPRESSION '+' EXPRESSION  {% numOperationAction $2 $1 $3 "+" }
           | EXPRESSION '-' EXPRESSION  {% numOperationAction $2 $1 $3 "-" }
           | EXPRESSION '*' EXPRESSION  {% numOperationAction $2 $1 $3 "*" }
           | EXPRESSION '/' EXPRESSION  {% numOperationAction $2 $1 $3 "/" }
           | EXPRESSION '%' EXPRESSION  {% numOperationAction $2 $1 $3 "%" }
           | EXPRESSION mod EXPRESSION  {% intOperationAction $2 $1 $3 "mod" }
           | EXPRESSION div EXPRESSION  {% intOperationAction $2 $1 $3 "div" }
           | FUNCTIONCALL               { $1 }
           | newlife '(' EXPRESSION ')' { AST.NEWLIFE $3 $ OKPointer (exp_type $3)} -- TODO FFFFFFFFFFF??????????????????????
           | LVAL '=' EXPRESSION        {% assignAction $2 $1 $3 }

EXPRESSIONS :: { [AST.EXPRESSION] }
EXPRESSIONS :                       { [] }
            | NONEMPTYEXPRESSIONS   { reverse $ $1 }

NONEMPTYEXPRESSIONS :: { [AST.EXPRESSION] }
NONEMPTYEXPRESSIONS : NONEMPTYEXPRESSIONS ',' EXPRESSION        { $3 : $1 }
                    | EXPRESSION                                { [$1] }

-- Anything with L-Value: variables, record.member, array[position]...
LVAL :: { AST.EXPRESSION }
LVAL :  id {% idAction $1 }
           | EXPRESSION '[' EXPRESSION ']'                   {% arrayAction (tkn_pos $2) $1 $3 }
           | EXPRESSION '.' id                            { AST.EXPRESSIONSTRUCT $1 (tkn_string $3) (exp_type $1)} -- TODO
           | '^' EXPRESSION           {% pointerAction $1 $2 }

LVALS :: { [AST.EXPRESSION] }
LVALS :                                 { [] }
      | NONEMPTYLVALS                   { reverse ($1) }

NONEMPTYLVALS :: { [AST.EXPRESSION] }
NONEMPTYLVALS : NONEMPTYLVALS ',' LVAL    { $3 : $1 }
              | LVAL                      { [$1] }


FUNCTIONCALL : id '(' EXPRESSIONS ')'          {%  do
                                                       sym <- P.findSym (tkn_string $1) (tkn_pos $1) -- TODO Check function parameters
                                                       return $ AST.FUNCTIONCALL (tkn_string $1) $3 (func_RetType.sym_type $ sym)}

MAYBELINE : {- empty -}                   { }
          | newline                       { }

--- 1}}}
{


data Parameter = Parameter{param_type :: OKType, param_id :: SymId} deriving Show

-- Actions{{{1

typedefAction :: Token -> OKType -> ParseM ()
typedefAction tkn oktype = do
  liftIO $ putStrLn ("agregando un nombre de TIPO " ++ tkn_string tkn)
  P.insertSym $ NameTypeSym 0 (tkn_string tkn) (tkn_pos tkn) (OKNameType (tkn_string tkn) oktype)

functionDefAction :: (Token, [Parameter], OKType) -> [AST.INSTRUCTION] -> ParseM ()
functionDefAction (tkn, params, ret) instrs = do
        let oktype = OKFunc (map param_type params) ret
        P.completeFunctionDef $ FuncSym 1 (tkn_string tkn) (tkn_pos tkn) oktype (map param_id params) instrs

functionSignAction :: Token -> [Parameter] -> OKType -> ParseM (Token, [Parameter], OKType)
functionSignAction tkn params ret = do
        let oktype = OKFunc (map param_type params) ret
            id = tkn_string tkn
            pos = tkn_pos tkn
            param_ids = map param_id params
        P.insertSym $ FuncSym 1 id pos oktype param_ids []
        return (tkn, params, ret)

functionParameterAction :: OKType -> Token -> ParseM Parameter
functionParameterAction oktype id = do
       scope <- P.topScope
       P.insertSym $ Sym scope (tkn_string id) (tkn_pos id) oktype
       return $ Parameter oktype (tkn_string id, scope)

declarationAction :: OKType -> [(Token, Maybe AST.EXPRESSION, Maybe AST.EXPRESSION)] -> ParseM ([AST.EXPRESSION])
declarationAction oktype l =
      do  let decls = reverse l                                                       :: [(Token, Maybe AST.EXPRESSION, Maybe AST.EXPRESSION)]
              assigns = filter (isJust.mySnd) decls                                   :: [(Token, Maybe AST.EXPRESSION, Maybe AST.EXPRESSION)]
              allVars = map (\x -> (tkn_string.myFst $ x, tkn_pos.myFst $ x, myThrd x)) decls   :: [(String, Pos, Maybe AST.EXPRESSION)]
              tkn = (myFst.head) decls
          vars <- mapM checkIfArray allVars

          scope <- P.topScope
          mapM_ (\(id, pos, okt) -> P.insertSym (Sym scope id pos okt)) vars

          ids <- mapM idAction (map myFst assigns)
          let exps = map (fromJust.mySnd) assigns
          mapM (uncurry $ assignAction tkn) (zip ids exps)
    where myFst (a,_,_)=a
          mySnd (_,b,_)=b
          myThrd (_,_,c)=c
          checkIfArray :: (Id, Pos, Maybe AST.EXPRESSION) -> ParseM (Id, Pos, OKType)
          checkIfArray (id, pos, Nothing) = return (id, pos, oktype)
          checkIfArray (id, pos, Just exp) = do
            checkExpectedType pos OKInt (exp_type exp)
            return (id, pos, OKArray 0 oktype)

arrayAction :: Pos -> AST.EXPRESSION -> AST.EXPRESSION -> ParseM AST.EXPRESSION
arrayAction pos arrExp posExp = do
      checkExpectedType pos OKInt (exp_type posExp)
      -- check arrExp for array type
      -- TODO Check E1 type for array
      return $ AST.ARRAYPOS arrExp posExp (OKArray 0 (array_Type.exp_type $ arrExp)) -- TODO Real size F

nameTypeAction :: Token -> ParseM OKType
nameTypeAction tkn = do
  sym <- P.findSym (tkn_string tkn) (tkn_pos tkn)
  case sym of
      NameTypeSym _ _ _ _ -> return $ sym_type sym
      _ -> do -- throwError $ IsNotType (tkn_string tkn) (tkn_pos tkn) --TODO Check
              return OKErrorT

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


arrayLiteralAction :: Token -> [AST.EXPRESSION] -> ParseM AST.EXPRESSION
arrayLiteralAction tkn exps = do
    let types = map exp_type exps
        hd = head types
        hasError = any ((==) OKErrorT) types
    let oktype = if hasError
                     then OKErrorT
                     else if all ((==) hd) types then hd
                          else OKErrorT
    return $ AST.ARRAYEXP exps (OKArray 0 oktype)


orderCompAction :: Token -> AST.EXPRESSION -> AST.EXPRESSION -> String -> ParseM AST.EXPRESSION
orderCompAction tkn exp1 exp2 op = do
          oktype <- checkOrdCompType (tkn_pos tkn) (exp_type exp1) (exp_type exp2)
          return $ AST.COMPAR exp1 op exp2 oktype

equalityComparAction :: Token -> AST.EXPRESSION -> AST.EXPRESSION -> String -> ParseM AST.EXPRESSION
equalityComparAction tkn exp1 exp2 op = do
          oktype <- checkCompType (tkn_pos tkn) (exp_type exp1) (exp_type exp2)
          return $ AST.COMPAR exp1 op exp2 oktype

notAction :: Token -> AST.EXPRESSION -> ParseM AST.EXPRESSION
notAction tkn exp = do
          oktype <- checkExpectedType (tkn_pos tkn) OKBoolean (exp_type exp)
          return $ AST.NOT exp oktype

booleanOperationAction :: Token -> AST.EXPRESSION -> AST.EXPRESSION -> String -> ParseM AST.EXPRESSION
booleanOperationAction tkn exp1 exp2 op = do
          oktype <- checkBooleanOpType (tkn_pos tkn) (exp_type exp1) (exp_type exp2)
          return $ AST.LOGIC exp1 op exp2 oktype

minusAction :: Token -> AST.EXPRESSION -> ParseM AST.EXPRESSION
minusAction tkn exp = do
          oktype <- checkNumericalType (tkn_pos tkn) (exp_type exp)
          return $ AST.MINUS exp oktype

numOperationAction :: Token -> AST.EXPRESSION -> AST.EXPRESSION -> String -> ParseM AST.EXPRESSION
numOperationAction tkn exp1 exp2 op = do
          oktype <- checkNumOpType (tkn_pos tkn) (exp_type exp1) (exp_type exp2)
          return $ AST.ARIT exp1 op exp2 oktype

intOperationAction :: Token -> AST.EXPRESSION -> AST.EXPRESSION -> String -> ParseM AST.EXPRESSION
intOperationAction tkn exp1 exp2 op = do
          oktype <- checkIntOpType (tkn_pos tkn) (exp_type exp1) (exp_type exp2)
          return $ AST.ARIT exp1 op exp2 oktype

-- TODO CAST
assignAction :: Token -> AST.EXPRESSION -> AST.EXPRESSION -> ParseM AST.EXPRESSION
assignAction tkn lhs rhs = do
  oktype <- case (exp_type lhs, exp_type rhs) of
                  (OKErrorT, _) -> return OKErrorT
                  (_, OKErrorT) -> return OKErrorT
                  (lhstype, rhstype) ->
                          if lhstype == rhstype then return rhstype
                                                else throwNotWhatIExpectedAndImNotSatisfied (fst.tkn_pos $ tkn) (exp_type lhs) (exp_type rhs)
  return $ AST.ASSIGN lhs rhs (oktype)

idAction :: Token -> ParseM AST.EXPRESSION
idAction tkn = do
          sym <- P.findSym (tkn_string tkn) (tkn_pos tkn)
          return $ AST.IDEXPRESSION (tkn_string tkn, sym_scope sym) (sym_type sym)

pointerAction :: Token -> AST.EXPRESSION -> ParseM AST.EXPRESSION
pointerAction tkn exp = do
          oktype <- checkAndGetPointerType (tkn_pos tkn) (exp_type exp)
          return $ AST.POINTER exp oktype

--- 1}}}

-- Type Checking{{{1

checkNumericalType :: Pos -> OKType -> ParseM OKType
checkNumericalType pos found = do
    if isNumericalType found then return found
                             else return OKErrorT

checkExpectedType :: Pos -> OKType -> OKType -> ParseM OKType
checkExpectedType pos expected found = do
    if found == OKErrorT
       then return OKErrorT
       else if found==expected then return expected
                               else throwNotWhatIExpectedAndImNotSatisfied (fst pos) expected found

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

-- Errors (Horrors){{{1
throwDifferentTypeError :: Int -> OKType -> OKType -> ParseM OKType
throwDifferentTypeError line t1 t2 = do
    liftIO $ putStrLn $ "Line " ++ show line ++ ". Expected same type, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    return OKErrorT

throwNotNumericalTypeError :: Int -> OKType -> OKType -> ParseM OKType
throwNotNumericalTypeError line t1 t2 = do
    liftIO $ putStrLn $ "Line " ++ show line ++ ". Expected a number types, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    return OKErrorT

throwNotWhatIExpectedAndImNotSatisfied :: Int -> OKType -> OKType -> ParseM OKType
throwNotWhatIExpectedAndImNotSatisfied line t1 t2 = do
    liftIO $ putStr $ "Line " ++ show line ++ ". Expected " ++ show t1 ++ ", found " ++ show t2 ++ "."
    liftIO $ putStrLn $ " Not What I Expected And I'm Not Satisfied."
    return OKErrorT

throwNotBooleanError :: Int -> OKType -> OKType -> ParseM OKType
throwNotBooleanError line t1 t2 = do
    liftIO $ putStrLn $ "Line " ++ show line ++ ". Expected boolean values, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    return OKErrorT

throwNotIntType :: Int -> OKType -> OKType -> ParseM OKType
throwNotIntType line t1 t2 = do
    liftIO $ putStrLn $ "Line " ++ show line ++ ". Expected int values, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    return OKErrorT

throwNotPointerType :: Int -> OKType -> ParseM OKType
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
