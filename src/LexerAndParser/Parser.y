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
import Data.List
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
  string                                  { StringTkn _ }
  ok                                      { OkTkn _ }              -- True
  notok                                   { NotOkTkn _ }           -- False
  '{'                                     { OpenBraceTkn _ }
  '}'                                     { CloseBraceTkn _ }
  list                                    { ListTypeTkn _ }
  '++'                                    { ConcatTkn _ }
  tuple                                   { TupleTypeTkn _ }
  '<<'                                    { OpenTupleTkn _ }
  '>>'                                    { CloseTupleTkn _ }
  '['                                     { ArrayStartTkn _ }
  ']'                                     { ArrayEndTkn _ }
  record                                  { RecordTkn _ }            -- Registers/structs
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
  s                                  { LiteralStringTkn _ _ }

-- 1}}}


%right '='
%nonassoc '!='
%left or
%left and
%nonassoc '>' '<' '==' '!=' '>=' '<='
%left '+' '-' '++'
%left '*' '/' '%' mod div
%right not
%right '^'
%left '[' '<<'
%nonassoc '.'

-- Grammar{{{1
%%
START :: { AST.START }
START : IMPORTS OUTSIDE_FUNCTION         { AST.START (reverse $1) $2 }


IMPORTS :: { [AST.IMPORT] } --{{{
IMPORTS : IMPORTS newline IMPORT        { (AST.IMPORT $3):$1 }
        | {- empty -}                   { [] }

IMPORT :: { [Id] }
IMPORT : aroundtheworld IDS             { reverse $2 }

IDS :: { [Id] }
IDS : IDS ',' id                             { (tkn_string $3):$1 }
  | id                                       { [tkn_string $1] }
-- }}}

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
BLOCK : MAYBELINE youbegin MAYBELINE INSIDEFUNCTION whereiend END                    { reverse $4 }

BEGIN : {- empty -}                                                       {% P.beginScope }

END : {- empty -}                                                           {% P.endScope }

INSIDEFUNCTION :: { [AST.INSTRUCTION] }
INSIDEFUNCTION : INSIDEFUNCTION INSTRUCTION                             { $2:$1 }
              | INSIDEFUNCTION DECLARATION newline                      { (map AST.EXPRESSIONINST $ reverse $2)++$1 }
              | {- empty -}                                             { [] }


-- Get all the assignments and then do the declarations
DECLARATION :: { [AST.EXPRESSION] } -- All Expressions are assignments
DECLARATION : TYPE DECLARATIONVARS {% declarationAction $1 $2 }


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
TYPE : TYPE '^'                                                                      { OKPointer $1 }
     | int                                                                           { OKInt }
     | float                                                                         { OKFloat }
     | boolean                                                                       { OKBoolean }
     | char                                                                          { OKChar }
     | string                                                                        { OKString }
     | record BEGIN '{' MAYBELINE INSIDERECORD '}' END                               { OKRecord $2 }
     | tuple '(' TYPES ')'                                                           { OKTuple (reverse $3) }
     | list '(' TYPE ')'                                                             { OKList $3 }
     | id                                                                            {% nameTypeAction $1 }

TYPES :: { [OKType] }
TYPES : TYPE                                              { [$1] }
      | TYPES ',' TYPE                                    { $3 : $1 }

INSIDERECORD :: { [AST.INSTRUCTION] }
INSIDERECORD : INSIDERECORD DECLARATION newline                 { (map AST.EXPRESSIONINST $ reverse $2)++$1 }
              | DECLARATION MAYBELINE                           { map AST.EXPRESSIONINST $ reverse $1 }


INSTRUCTION :: { AST.INSTRUCTION }
INSTRUCTION : go '(' NONEMPTYEXPRESSIONS ')' newline                            { AST.GOING $ reverse $3 }
            | goslowly '(' NONEMPTYEXPRESSIONS ')' newline                      { AST.GOINGSLOWLY $ reverse $3 }
            | gomental '(' NONEMPTYEXPRESSIONS ')' newline                      { AST.GOINGMENTAL $ reverse $3 }
            | readmymind '(' LVALS ')' newline                                  { AST.READMYMIND $3 }
            | amnesiac '(' EXPRESSION ')' newline                               { AST.AMNESIAC $ $3 }
            | if EXPRESSION BEGIN BLOCK IFELSE                                  {% ifAction $1 $2 (reverse $4) $5 }
            | cantstop EXPRESSION BEGIN BLOCK                                   {% cantStopAction $1 $2 (reverse $4) }
            | onemoretime BEGIN DECLARATION ';' EXPRESSION ';' EXPRESSION BLOCK {% oneMoreTimeAction $1 $5 $3 $7 (reverse $8) }
            | getback EXPRESSION newline                                        {% getBackAction $1 (Just $2) }
            | getback newline                                                   {% getBackAction $1 Nothing }
            | breakthru newline                                                 { AST.BREAKTHRU }
            | exitmusic newline                                                 { AST.EXITMUSIC }
            | EXPRESSION newline                                                { AST.EXPRESSIONINST $1 }

IFELSE : ifyouhavetoask EXPRESSION BEGIN BLOCK IFELSE                           {% ifYouHaveToAskAction $1 $2 (reverse $4) $5 }
       | otherside BEGIN BLOCK                                                  { AST.OTHERSIDE $3 }
       | {- empty -}                                                            { AST.IFELSEVOID }

EXPRESSION :: { AST.EXPRESSION }
EXPRESSION : LVAL                               { $1 }
           | n                                  { AST.INTEXP (read $ tkn_string $1) OKInt}
           | f                                  { AST.FLOATEXP (read $ tkn_string $1) OKFloat}
           | s                                  { AST.STRINGEXP (tkn_string $1) OKString}
           | c                                  { AST.CHAREXP (tkn_char $1) OKChar}
           | ok                                 { AST.BOOLEANEXP True OKBoolean}
           | notok                              { AST.BOOLEANEXP False OKBoolean}
           | '(' EXPRESSION ')'                 { $2 }
           | '{' NONEMPTYEXPRESSIONS '}'        {% arrayLiteralAction $1 (reverse $2) }
           | '<<' NONEMPTYEXPRESSIONS '>>'      { tupleLiteralAction $1 (reverse $2) }
           | '[' ']'                            { AST.LISTEXP [] (OKList OKVoid) }
           | '[' NONEMPTYEXPRESSIONS ']'        {% listLiteralAction $1 (reverse $2) }
           | EXPRESSION '++' EXPRESSION         {% listConcatAction $2 $1 $3 }
           | EXPRESSION '.' n                   {% tupleAccessAction $2 $1 (read $ tkn_string $3) }
           | EXPRESSION '<' EXPRESSION          {% orderCompAction $2 $1 $3 "<" }
           | EXPRESSION '>' EXPRESSION          {% orderCompAction $2 $1 $3 ">" }
           | EXPRESSION '<=' EXPRESSION         {% orderCompAction $2 $1 $3 "<=" }
           | EXPRESSION '>=' EXPRESSION         {% orderCompAction $2 $1 $3 ">=" }
           | EXPRESSION '==' EXPRESSION         {% equalityComparAction $2 $1 $3 "==" }
           | EXPRESSION '!=' EXPRESSION         {% equalityComparAction $2 $1 $3 "!=" }
           | not EXPRESSION                     {% notAction $1 $2 }
           | EXPRESSION and EXPRESSION          {% booleanOperationAction $2 $1 $3 "and" }
           | EXPRESSION or EXPRESSION           {% booleanOperationAction $2 $1 $3 "or" }
           | '-' EXPRESSION                     {% minusAction $1 $2 }
           | EXPRESSION '+' EXPRESSION          {% numOperationAction $2 $1 $3 "+" }
           | EXPRESSION '-' EXPRESSION          {% numOperationAction $2 $1 $3 "-" }
           | EXPRESSION '*' EXPRESSION          {% numOperationAction $2 $1 $3 "*" }
           | EXPRESSION '/' EXPRESSION          {% numOperationAction $2 $1 $3 "/" }
           | EXPRESSION '%' EXPRESSION          {% numOperationAction $2 $1 $3 "%" }
           | EXPRESSION mod EXPRESSION          {% intOperationAction $2 $1 $3 "mod" }
           | EXPRESSION div EXPRESSION          {% intOperationAction $2 $1 $3 "div" }
           | id '(' EXPRESSIONS ')'             {% functionCallAction $1 $3 }
           | newlife '(' EXPRESSION ')'         { AST.NEWLIFE $3 $ OKPointer (exp_type $3)}
           | LVAL '=' EXPRESSION                {% assignAction $2 $1 $3 }

EXPRESSIONS :: { [AST.EXPRESSION] }
EXPRESSIONS :                       { [] }
            | NONEMPTYEXPRESSIONS   { reverse $ $1 }

NONEMPTYEXPRESSIONS :: { [AST.EXPRESSION] }
NONEMPTYEXPRESSIONS : NONEMPTYEXPRESSIONS ',' EXPRESSION        { $3 : $1 }
                    | EXPRESSION                                { [$1] }

-- Anything with L-Value: variables, record.member, array[position]...
LVAL :: { AST.EXPRESSION }
LVAL :  id {% idAction $1 }
           | EXPRESSION '[' EXPRESSION ']'                {% accessAction (tkn_pos $2) $1 $3 }
           | EXPRESSION '.' id                            {% recordMemberAction $1 $3 }
           | '^'  EXPRESSION            {% pointerAction $1 $2 }

LVALS :: { [AST.EXPRESSION] }
LVALS :                                 { [] }
      | NONEMPTYLVALS                   { reverse ($1) }

NONEMPTYLVALS :: { [AST.EXPRESSION] }
NONEMPTYLVALS : NONEMPTYLVALS ',' LVAL    { $3 : $1 }
              | LVAL                      { [$1] }

MAYBELINE : {- empty -}                   { }
          | newline                       { }

--- 1}}}
{


data Parameter = Parameter{param_type :: OKType, param_id :: SymId} deriving Show

-- Actions{{{1

-- Adds the name to the symtable
typedefAction :: Token -> OKType -> ParseM ()
typedefAction tkn oktype = do
  P.insertSym $ NameTypeSym 0 (tkn_string tkn) (tkn_pos tkn) (OKNameType (tkn_string tkn) oktype)


-- Adds the body of the function to the sym table
functionDefAction :: (Token, [Parameter], OKType) -> [AST.INSTRUCTION] -> ParseM ()
functionDefAction (tkn, params, ret) instrs = do
        let oktype = OKFunc (map param_type params) ret
        P.completeFunctionDef $ FuncSym 1 (tkn_string tkn) (tkn_pos tkn) oktype (map param_id params) instrs

-- Adds the function, without body, to the sym table
functionSignAction :: Token -> [Parameter] -> OKType -> ParseM (Token, [Parameter], OKType)
functionSignAction tkn params retType = do
        let oktype = OKFunc (map param_type params) retType
            id = tkn_string tkn
            pos = tkn_pos tkn
            param_ids = map param_id params
        P.insertSym $ FuncSym 1 id pos oktype param_ids []
        P.setReturnType retType
        return (tkn, params, retType)

functionParameterAction :: OKType -> Token -> ParseM Parameter
functionParameterAction oktype id = do
       scope <- P.topScope
       P.insertSym $ VarSym scope (tkn_string id) (tkn_pos id) oktype
       return $ Parameter oktype (tkn_string id, scope)

declarationAction :: OKType -> [(Token, Maybe AST.EXPRESSION, Maybe AST.EXPRESSION)] -> ParseM ([AST.EXPRESSION])
declarationAction oktype l =
      do  let decls = reverse l                                                       :: [(Token, Maybe AST.EXPRESSION, Maybe AST.EXPRESSION)]
              assigns = filter (isJust.mySnd) decls                                   :: [(Token, Maybe AST.EXPRESSION, Maybe AST.EXPRESSION)]
              allVars = map (\x -> (tkn_string.myFst $ x, tkn_pos.myFst $ x, myThrd x)) decls   :: [(String, Pos, Maybe AST.EXPRESSION)]
              tkn = (myFst.head) decls
          vars <- mapM checkIfArray allVars

          scope <- P.topScope
          mapM_ (\(id, pos, okt) -> P.insertSym (VarSym scope id pos okt)) vars

          ids <- mapM idAction (map myFst assigns)
          let exps = map (fromJust.mySnd) assigns
          mapM (uncurry $ assignAction tkn) (zip ids exps)
    where myFst (a,_,_)=a
          mySnd (_,b,_)=b
          myThrd (_,_,c)=c
          checkIfArray :: (Id, Pos, Maybe AST.EXPRESSION) -> ParseM (Id, Pos, OKType)
          checkIfArray (id, pos, Nothing) = return (id, pos, oktype)
          checkIfArray (id, pos, Just exp) = do
            checkExpectedType pos OKInt (exp_type exp) " for array size"
            return (id, pos, OKArray 0 oktype)

-- REVISAR RECORDS AQUI
accessAction :: Pos -> AST.EXPRESSION -> AST.EXPRESSION -> ParseM AST.EXPRESSION
accessAction pos exp posExp = do
      checkExpectedType pos OKInt (exp_type posExp) " for index"
      oktype <- checkAndGetArrayOrListType pos (exp_type exp)
      if isListType (exp_type exp) then return $ AST.LISTACCESS exp posExp oktype
                                   else return $ AST.ARRAYACCESS exp posExp oktype

-- REVISAR AQUI
recordMemberAction :: AST.EXPRESSION -> Token -> ParseM AST.EXPRESSION
recordMemberAction exp tkn = do
    let oktype = solveNameTypes (exp_type exp)
    case oktype of
         OKRecord scope -> do sym <- P.findSymInScope scope tkn (exp_type exp) `catchError` (\_ -> return $ ErrorSym (-1) (tkn_string tkn) (tkn_pos tkn) OKErrorT)
                              case sym of
                              	ErrorSym{} -> do -- showRecordMemberNotDefined (fst.tkn_pos $ tkn) (tkn_string tkn) 
                              			         return $ AST.RECORDACCESS exp (tkn_string tkn) OKErrorT -- return $ AST.FUNCTIONCALL (tkn_string tkn) exp OKErrorT
                              	_ -> return $ AST.RECORDACCESS exp (tkn_string tkn) (sym_type sym)
         OKErrorT -> return $ AST.RECORDACCESS exp (tkn_string tkn) OKErrorT
         _ -> do  showExpectedRecord (fst.tkn_pos $ tkn) (exp_type exp)
                  return $ AST.RECORDACCESS exp (tkn_string tkn) OKErrorT


nameTypeAction :: Token -> ParseM OKType
nameTypeAction tkn = do
  sym <- P.findSym (tkn_string tkn) (tkn_pos tkn) `catchError`
                      (\_ -> return $ ErrorSym (-1) "" (tkn_pos tkn) OKErrorT)
  case sym of
      NameTypeSym{} -> return $ sym_type sym
      ErrorSym{} -> do showNameTypeNotDefined (tkn_string tkn) (fst.tkn_pos $ tkn)
                       return OKErrorT
      _ -> do showNameIsNotNameType (tkn_string tkn) (fst.tkn_pos $ tkn) (fst.sym_pos $ sym)
              return OKErrorT

ifAction :: Token -> AST.EXPRESSION -> [AST.INSTRUCTION] -> AST.IFELSE -> ParseM AST.INSTRUCTION
ifAction tkn condition blk ifelse = do
          checkExpectedType (tkn_pos tkn) OKBoolean (exp_type condition) " for if condition"
          return $ AST.IF condition blk ifelse

cantStopAction :: Token -> AST.EXPRESSION -> [AST.INSTRUCTION] -> ParseM AST.INSTRUCTION
cantStopAction tkn condition blk = do
          checkExpectedType (tkn_pos tkn) OKBoolean (exp_type condition) " for cantstop condition"
          return $ AST.CANTSTOP condition blk


oneMoreTimeAction :: Token -> AST.EXPRESSION -> [AST.EXPRESSION] -> AST.EXPRESSION -> [AST.INSTRUCTION] -> ParseM AST.INSTRUCTION
oneMoreTimeAction tkn condition init step blk = do
          checkExpectedType (tkn_pos tkn) OKBoolean (exp_type condition) " for onemoretime condition"
          return $ AST.ONEMORETIME init condition step blk

getBackAction :: Token -> Maybe (AST.EXPRESSION) -> ParseM AST.INSTRUCTION
getBackAction tkn (Just exp) = do
          expected <- P.getReturnType
          checkExpectedType (tkn_pos tkn) expected (exp_type exp) " in getback instruction"
          return $ AST.GETBACK (Just exp)
getBackAction tkn Nothing = do
          expected <- P.getReturnType
          checkExpectedType (tkn_pos tkn) expected OKVoid " in getback instruction"
          return $ AST.GETBACK Nothing


ifYouHaveToAskAction :: Token -> AST.EXPRESSION -> [AST.INSTRUCTION] -> AST.IFELSE -> ParseM AST.IFELSE
ifYouHaveToAskAction tkn condition blk ifelse = do
          checkExpectedType (tkn_pos tkn) OKBoolean (exp_type condition) " for ifyouhavetoask condition"
          return $ AST.IFASK condition blk ifelse


-- checks if all types are the same
arrayLiteralAction :: Token -> [AST.EXPRESSION] -> ParseM AST.EXPRESSION
arrayLiteralAction tkn exps = do
    let types = nub $ map exp_type exps
        hd = head types
        hasError = any ((==) OKErrorT) types
    oktype <- if hasError
                     then return OKErrorT
                     else do  let merged = foldl1 mergeVoidType types
                              if merged == OKErrorT
                                  then do showFoundDifferentTypesInArray (fst $ tkn_pos tkn) types
                                          return OKErrorT
                                  else return (OKArray 0 merged)
    return $ AST.ARRAYEXP exps oktype

tupleLiteralAction :: Token -> [AST.EXPRESSION] -> AST.EXPRESSION
tupleLiteralAction tkn exps = AST.TUPLEEXP exps (OKTuple $ map exp_type exps)


-- checks if all types are the same
listLiteralAction :: Token -> [AST.EXPRESSION] -> ParseM AST.EXPRESSION
listLiteralAction tkn exps = do
    let types = nub $ map exp_type exps
        hd = head types
        hasError = any ((==) OKErrorT) types
    oktype <- if hasError
                     then return OKErrorT
                     else if all ((==) hd) types then return $ OKList hd
                          else do showFoundDifferentTypesInList (fst $ tkn_pos tkn) types
                                  return OKErrorT
    return $ AST.LISTEXP exps oktype

listConcatAction :: Token -> AST.EXPRESSION -> AST.EXPRESSION -> ParseM AST.EXPRESSION
listConcatAction tkn exp1 exp2 =
    case (exp_type exp1, exp_type exp2) of
         (OKErrorT, _) -> return $ AST.CONCAT exp1 exp2 (exp_type exp2)
         (_, OKErrorT) -> return $ AST.CONCAT exp1 exp2 (exp_type exp1)
         (t1, t2) ->
            if not (isListType t1) || not (isListType t2)
               then do  showConcatExpectedTwoLists (fst.tkn_pos $ tkn) t1 t2
                        return $ AST.CONCAT exp1 exp2 OKErrorT
               else case (elems_type t1, elems_type t2) of
                         (OKVoid, _) -> return exp2
                         (_, OKVoid) -> return exp1
                         (a, b) -> if a==b then return $ AST.CONCAT exp1 exp2 t1
                                           else do showConcatExpectedSameType (fst.tkn_pos $ tkn) t1 t2 --TODO
                                                   return $ AST.CONCAT exp1 exp2 OKErrorT

-- REVISAR AQUI
tupleAccessAction :: Token -> AST.EXPRESSION -> Int -> ParseM AST.EXPRESSION
tupleAccessAction tkn exp n =
    case exp_type exp of
         OKErrorT -> return exp
         OKTuple types -> do
            if n >= length types then do -- showTupleOutOfRange (fst.tkn_pos $ tkn)
            					         return $ AST.TUPLEACCESS exp n OKErrorT
                                 else return (AST.TUPLEACCESS exp n (types !! n))
         _ -> do -- showNotATuple (fst.tkn_pos $ tkn) (tkn_string tkn)
         	  return $ AST.TUPLEACCESS exp n OKErrorT

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
          oktype <- checkExpectedType (tkn_pos tkn) OKBoolean (exp_type exp) " in not expression"
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

assignAction :: Token -> AST.EXPRESSION -> AST.EXPRESSION -> ParseM AST.EXPRESSION
assignAction tkn lhs rhs = do
  oktype <- case (exp_type lhs, exp_type rhs) of
                  (OKErrorT, _) -> return OKErrorT
                  (_, OKErrorT) -> return OKErrorT
                  (lhstype, rhstype) ->
                          if listComp lhstype rhstype then return lhstype
                                                      else throwNotWhatIExpectedAndImNotSatisfied (fst.tkn_pos $ tkn) (exp_type lhs) (exp_type rhs) "" --TODO Check why
  return $ AST.ASSIGN lhs rhs (oktype)

idAction :: Token -> ParseM AST.EXPRESSION
idAction tkn = do
          sym <- P.findSym (tkn_string tkn) (tkn_pos tkn) `catchError` (\_ -> return $ ErrorSym (-1) (tkn_string tkn) (tkn_pos tkn) OKErrorT) --TODO
          return $ AST.IDEXPRESSION (tkn_string tkn, sym_scope sym) (sym_type sym)

pointerAction :: Token -> AST.EXPRESSION -> ParseM AST.EXPRESSION
pointerAction tkn exp = do
          oktype <- checkAndGetPointerType (tkn_pos tkn) (exp_type exp)
          return $ AST.POINTER exp oktype

functionCallAction :: Token -> [AST.EXPRESSION] -> ParseM AST.EXPRESSION
functionCallAction tkn exp = do
         sym <- P.findFunction (tkn_string tkn) (tkn_pos tkn) (map exp_type exp) `catchError` catcher
         case sym of
              FuncSym{} -> return $ AST.FUNCTIONCALL (tkn_string tkn) exp (func_RetType.sym_type $ sym)
              _ -> return $ AST.FUNCTIONCALL (tkn_string tkn) exp OKErrorT
  where
    catcher :: P.ParseMError -> ParseM Sym
    catcher FunctionNotDefined = do showFunctionNotDefined (fst.tkn_pos $ tkn) (tkn_string tkn)
                                    return $ ErrorSym (-1) (tkn_string tkn) (tkn_pos tkn) OKErrorT
    catcher (VariableInScopeIsNotFunction ln oktype) = do showFunctionVariableAlreadyDefined (fst.tkn_pos $ tkn) (tkn_string tkn) ln oktype 
                                                          return $ ErrorSym (-1) (tkn_string tkn) (tkn_pos tkn) OKErrorT
   
--- 1}}}






-- Type Checking{{{1

checkNumericalType :: Pos -> OKType -> ParseM OKType
checkNumericalType pos found = do
    if isNumericalType found then return found
                             else return OKErrorT

checkExpectedType :: Pos -> OKType -> OKType -> String -> ParseM OKType
checkExpectedType pos expected found msg = do
    if found == OKErrorT
       then return OKErrorT
       else if found==expected then return expected
                               else throwNotWhatIExpectedAndImNotSatisfied (fst pos) expected found msg

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

checkAndGetArrayOrListType :: Pos -> OKType -> ParseM (OKType)
checkAndGetArrayOrListType (line, _) OKErrorT = return OKErrorT
checkAndGetArrayOrListType (line, _) (OKArray _ t) = return t
checkAndGetArrayOrListType (line, _) (OKList t) = return t
checkAndGetArrayOrListType (line, _) t = throwNotArrayType line t

-- 1}}}

-- Errors (Horrors){{{1

showExpectedRecord :: Int -> OKType -> ParseM ()
showExpectedRecord ln t = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Expected a record, found " ++ show t ++ "."
    liftIO $ putStrLn $ "Not What I Expected And I'm Not Satisfied.\n"

throwDifferentTypeError :: Int -> OKType -> OKType -> ParseM OKType
throwDifferentTypeError line t1 t2 = do
    liftIO $ putStrLn $ "Line " ++ show line ++ ". Expected same type, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    return OKErrorT

throwNotNumericalTypeError :: Int -> OKType -> OKType -> ParseM OKType
throwNotNumericalTypeError line t1 t2 = do
    liftIO $ putStrLn $ "Line " ++ show line ++ ". Expected a number types, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    return OKErrorT

throwNotWhatIExpectedAndImNotSatisfied :: Int -> OKType -> OKType -> String -> ParseM OKType
throwNotWhatIExpectedAndImNotSatisfied ln t1 t2 msg = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Expected " ++ show t1 ++ msg ++ ", found " ++ show t2 ++ "."
    liftIO $ putStrLn $ "Not What I Expected And I'm Not Satisfied.\n"
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

throwNotArrayType :: Int -> OKType -> ParseM OKType
throwNotArrayType line t = do
    liftIO $ putStrLn $ "Line " ++ show line ++ ". Expected array value, found " ++ show t ++ "."
    return OKErrorT

showNameTypeNotDefined :: Id -> Int -> ParseM ()
showNameTypeNotDefined id ln = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Type " ++ id ++ " is not defined.\n"

showNameIsNotNameType :: Id -> Int -> Int -> ParseM ()
showNameIsNotNameType id ln ln2 = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ id ++ " defined in line " ++ show ln2 ++ " does not refer to a type.\n"

showFoundDifferentTypesInArray :: Int -> [OKType] -> ParseM ()
showFoundDifferentTypesInArray ln types = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Arrays must have a unique type. Found different types:"
    liftIO $ putStrLn $ show types ++ "\n"

showFoundDifferentTypesInList :: Int -> [OKType] -> ParseM ()
showFoundDifferentTypesInList ln types = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Lists must have a unique type. Found different types:"
    liftIO $ putStrLn $ show types ++ "\n"

showConcatExpectedTwoLists :: Int -> OKType -> OKType -> ParseM ()
showConcatExpectedTwoLists ln t1 t2 = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Concat operator expects two lists, found " ++ show t1 ++ " ++ " ++ show t2 ++ ".\n"

showConcatExpectedSameType :: Int -> OKType -> OKType -> ParseM ()
showConcatExpectedSameType ln t1 t2 = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Concat operator expects two lists of same type, found " ++ show t1 ++ " ++ " ++ show t2 ++ ".\n"

showTupleOutOfRange :: Int -> ParseM ()
showTupleOutOfRange ln = do
	liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
   -- liftIO $ putStrLn $ "The tuple " ++ show tuple ++ " has a range of " ++ show range2 ++ " and cant access an out of rangeposition .\n"

showNotATuple:: Int -> Id -> ParseM ()
showNotATuple ln id = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"

showFunctionNotDefined :: Int -> Id -> ParseM ()
showFunctionNotDefined ln id = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Function " ++ id ++ " not defined.\n"

showFunctionVariableAlreadyDefined :: Int -> Id -> Int -> OKType -> ParseM ()
showFunctionVariableAlreadyDefined ln id ln2 oktype = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Function " ++ show id ++ " is already defined in line " ++ show ln2 ++ " with type " ++ show oktype ++ ".\n"

showRecordMemberNotDefined :: Int -> Id -> ParseM ()
showRecordMemberNotDefined ln id = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"

--- 1}}}

lexwrap :: (Token -> ParseM a) -> ParseM a
lexwrap cont = do
  tkn <- alexGetToken
  --liftIO $ putStrLn $ "    " ++ show tkn
  cont tkn
}
