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
import OKTypes
import SymTable
import Scope

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
  continue                                { ContinueTkn _ }         -- Return
  intothevoid                             { IntoTheVoidTkn _ }     -- Void
  newlife                                 { NewLifeTkn _ }         -- Calloc
--  saveme                                  { SaveMeTkn }          -- Malloc
--  keepyourselfalive                       { KeepAliveTkn }       -- Realloc
  amnesiac                                { AmnesiacTkn _ }        -- Free
  exitmusic                               { ExitMusicTkn _ }       -- Exit
  --aroundtheworld                          { AroundTheWorldTkn _ }  -- Import
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
  --union                                   { UnionTkn _ }
  '.'                                     { DotTkn _ }
  '._'                                    { TupleAccessTkn _ }
  '^'                                     { PointerTkn _ }

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
  varId                                      { VarIdTkn _ _ }
  typeId                                      { TypeIdTkn _ _ }
  f                                       { FloatLiteralTkn _ _ }
  n                                       { IntLiteralTkn _ _ }
  newline                                 { NewLineTkn _ }
  c                                       { LiteralCharTkn _ _ } -- char
  s                                       { LiteralStringTkn _ _ }

-- 1}}}


%right '='
%left or
%left and
%nonassoc '>' '<' '==' '!=' '>=' '<='
%left '+' '-' '++'
%left '*' '/' '%' mod div
%right not
%right '^'
%left '[' '<<'
%nonassoc '.' '._' ','

%left '(' '{'

-- Grammar{{{1
%%
START :: { AST.START }
START : MAYBELINE OUTSIDE_FUNCTION         { AST.START $2 }


--IMPORTS :: { [AST.IMPORT] } --{{{
--IMPORTS : IMPORTS newline IMPORT        { (AST.IMPORT $3):$1 }
        -- | [>λ<]                         { [] }

--IMPORT :: { [Id] }
--IMPORT : aroundtheworld IDS             { reverse $2 }

--IDS :: { [Id] }
--IDS : IDS ',' varId                             { (tkn_string $3):$1 }
  -- | varId                                       { [tkn_string $1] }
-- }}}

OUTSIDE_FUNCTION :: { [AST.OUTSIDE] }
OUTSIDE_FUNCTION : FUNCTION_DEF OUTSIDE_FUNCTION           { $2 }
                | FUNCTION_SIGN_DECL OUTSIDE_FUNCTION           { $2 }
                | DECLARATION newline OUTSIDE_FUNCTION     { (AST.OUTASSIGN $1):$3 }
                | TYPEDEF OUTSIDE_FUNCTION                 { $2 }
                | {-λ-}                                    { [] }

TYPEDEF :: { () }
TYPEDEF : typedef typeId TYPE newline                          {% typedefAction $2 $3 }

-- Completes the definition of the function
FUNCTION_DEF :: { () }
FUNCTION_DEF : FUNCTION_SIGN BLOCK MAYBELINE {% functionDefAction (snd $1) (fst $1, $2)}

-- Creates the function symbol and inserts it to the sym table
FUNCTION_SIGN :: { (Scope, (Token, [Parameter], OKType)) }
FUNCTION_SIGN : dafunk BEGIN0 varId '(' LPARAMETERSFUNC ')' ':' RETURNTYPE MAYBELINE     {% functionSignAction $3 $5 $8 True $2 }

-- Creates the function symbol and inserts it to the sym table
FUNCTION_SIGN_DECL :: { (Scope, (Token, [Parameter], OKType)) }
FUNCTION_SIGN_DECL : dafunk BEGIN0 varId '(' LPARAMETERSFUNC ')' ':' RETURNTYPE MAYBELINE END   {% functionSignAction $3 $5 $8 False $2 }

RETURNTYPE :: { OKType }
RETURNTYPE: intothevoid                                                 { OKVoid }
          | TYPE                                                        { $1 }


LPARAMETERSFUNC :: { [Parameter] }
LPARAMETERSFUNC : NONEMPTYLPARAMETERSFUNC                               { reverse $1 }
                | {-λ-}                                                 { [] }


NONEMPTYLPARAMETERSFUNC :: { [Parameter] }
NONEMPTYLPARAMETERSFUNC : NONEMPTYLPARAMETERSFUNC ',' FUNCTIONPARAMETER { $3:($1) }
                        | FUNCTIONPARAMETER                             { [$1] }



FUNCTIONPARAMETER :: { Parameter }
FUNCTIONPARAMETER : TYPE varId         {% functionParameterAction $1 $2}

BLOCK :: { [AST.INSTRUCTION] }
BLOCK : youbegin MAYBELINE INSIDEFUNCTION whereiend END                   { reverse $3 }

BEGIN : {-λ-}                                                       {% P.beginScope }

BEGIN0 : {-λ-}                                                       {% P.begin0Scope }

END : {-λ-}                                                           {% P.endScope }

INSIDEFUNCTION :: { [AST.INSTRUCTION] }
INSIDEFUNCTION : INSIDEFUNCTION INSTRUCTION                             { $2:$1 }
              | INSIDEFUNCTION DECLARATION newline                      { (map AST.EXPRESSIONINST $ reverse $2)++$1 }
              | {-λ-}                                             { [] }


-- Get all the assignments and then do the declarations
DECLARATION :: { [AST.EXPRESSION] } -- All Expressions are assignments
DECLARATION : TYPE DECLARATIONVARS {% declarationAction $1 (reverse $2) }



-- Symbols are added in parent rule, this just gets everything and passes it up
DECLARATIONVARS :: { [(Token, Maybe AST.EXPRESSION)] }
DECLARATIONVARS : varId '=' EXPRESSION                                    { [($1, Just $3)] }
            | DECLARATIONVARS ',' varId '=' EXPRESSION                    { ($3, Just $5):($1) }
            | varId                                                       { [($1, Nothing)] }
            | DECLARATIONVARS ',' varId                                   { ($3, Nothing):($1) }



TYPE :: { OKType }
TYPE : TYPE '^'                                                                      { OKPointer $1 }
     | TYPE '[' n ']'                                                                {% arrayTypeAction (tkn_pos $2) $1 (read $ tkn_string $3) }
     | int                                                                           { OKInt }
     | float                                                                         { OKFloat }
     | boolean                                                                       { OKBoolean }
     | char                                                                          { OKChar }
     | string                                                                        { OKString }
     | record BEGIN0 '{' MAYBELINE INSIDERECORD '}' END                               { OKRecord $2 }
     -- | union '{' MAYBELINE INSIDEUNION '}'                                           { OKUnion $2 }
     | tuple '(' TYPES ')'                                                           { OKTuple (reverse $3) }
     | list '(' TYPE ')'                                                             { OKList $3 }
     | typeId                                                                            {% nameTypeAction $1 }


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
            | if EXPRESSION BEGIN MAYBELINE BLOCK newline IFELSE                                  {% ifAction $1 $2 ($3,$5) $7 }
            | cantstop EXPRESSION BEGIN MAYBELINE BLOCK newline                                  {% cantStopAction $1 $2 ($3,$5) }
            | onemoretime BEGIN DECLARATION ';' EXPRESSION ';' EXPRESSION MAYBELINE BLOCK newline {% oneMoreTimeAction $1 $5 $3 $7 ($2,$9) }
            | getback EXPRESSION newline                                        {% getBackAction $1 (Just $2) }
            | getback newline                                                   {% getBackAction $1 Nothing }
            | continue newline                                                  { AST.CONTINUE }
            | breakthru newline                                                 { AST.BREAKTHRU }
            | exitmusic newline                                                 { AST.EXITMUSIC }
            | EXPRESSION newline                                                { AST.EXPRESSIONINST $1 }


IFELSE : ifyouhavetoask EXPRESSION BEGIN MAYBELINE BLOCK newline IFELSE                           {% ifYouHaveToAskAction $1 $2 ($3,$5) $7 }
       | otherside BEGIN MAYBELINE BLOCK newline                                                { AST.OTHERSIDE ($2,$4) }
       | {-λ-}                                                                  { AST.IFELSEVOID }


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
           | EXPRESSION '+' EXPRESSION          {% numOperationAction (tkn_pos $2) $1 $3 "+" }
           | EXPRESSION '-' EXPRESSION          {% numOperationAction (tkn_pos $2) $1 $3 "-" }
           | EXPRESSION '*' EXPRESSION          {% numOperationAction (tkn_pos $2) $1 $3 "*" }
           | EXPRESSION '/' EXPRESSION          {% numOperationAction (tkn_pos $2) $1 $3 "/" }
           | EXPRESSION '%' EXPRESSION          {% numOperationAction (tkn_pos $2) $1 $3 "%" }
           | EXPRESSION mod EXPRESSION          {% intOperationAction (tkn_pos $2) $1 $3 "mod" }
           | EXPRESSION div EXPRESSION          {% intOperationAction (tkn_pos $2) $1 $3 "div" }
           | varId '(' EXPRESSIONS ')'          {% functionCallAction $1 $3 }
           | newlife '(' EXPRESSION ')'         { AST.NEWLIFE $3 $ OKPointer (exp_type $3)}
           | LVAL '=' EXPRESSION                {% assignAction $2 $1 $3 }

EXPRESSIONS :: { [AST.EXPRESSION] }
EXPRESSIONS : NONEMPTYEXPRESSIONS   { reverse $ $1 }
            | {-λ-}                 { [] }

NONEMPTYEXPRESSIONS :: { [AST.EXPRESSION] }
NONEMPTYEXPRESSIONS : NONEMPTYEXPRESSIONS ',' EXPRESSION        { $3 : $1 }
                    | EXPRESSION                                { [$1] }

-- Anything with L-Value: variables, record.member, array[position]...
LVAL :: { AST.EXPRESSION }
LVAL :  varId                                         {% idAction $1 }
       | EXPRESSION '[' EXPRESSION ']'                {% arrayOrListAccessAction (tkn_pos $2) $1 $3 }
       | EXPRESSION '.' varId                         {% recordMemberAction $1 $3 }
       | '^' '(' EXPRESSION ')'                              {% pointerAction $1 $3 }
       | EXPRESSION '._' n                            {% tupleAccessAction $2 $1 (read $ tkn_string $3) }


LVALS :: { [AST.EXPRESSION] }
LVALS :                                 { [] }
      | NONEMPTYLVALS                   { reverse ($1) }

NONEMPTYLVALS :: { [AST.EXPRESSION] }
NONEMPTYLVALS : NONEMPTYLVALS ',' LVAL    { $3 : $1 }
              | LVAL                      { [$1] }

MAYBELINE : {-λ-}                   { }
          | newline                       { }

--- 1}}}
{


data Parameter = Parameter{param_type :: OKType, param_id :: SymId} deriving Show

-- Actions{{{1

-- Adds the name to the symtable as a type alias
typedefAction :: Token -> OKType -> ParseM ()
typedefAction tkn_id oktype = do
  P.insertNameTypeSym (tkn_string tkn_id) (tkn_pos tkn_id) (OKNameType (tkn_string tkn_id) oktype)


-- Adds the body of the function to the sym table
functionDefAction :: (Token, [Parameter], OKType) -> (Scope, [AST.INSTRUCTION]) -> ParseM ()
functionDefAction (tkn, params, ret) instrs = do
        let oktype = OKFunc (map param_type params) ret
        P.completeFunctionDef (tkn_string tkn) oktype instrs

-- Adds the function, without body, to the sym table
functionSignAction :: Token -> [Parameter] -> OKType -> Bool -> Scope -> ParseM (Scope, (Token, [Parameter], OKType))
functionSignAction tkn params retType defining insideScope = do
        let oktype = OKFunc (map param_type params) retType
            id = tkn_string tkn
            pos = tkn_pos tkn
            param_ids = map param_id params
        P.insertFuncSym id pos oktype param_ids defining
        P.setReturnType retType
        return (insideScope, (tkn, params, retType))

-- Adds parameter to the symtable
functionParameterAction :: OKType -> Token -> ParseM Parameter
functionParameterAction oktype id = do
       scope <- P.topScope
       P.insertVarSym scope (tkn_string id) (tkn_pos id) oktype
       return $ Parameter oktype (tkn_string id, scope)


-- Takes a type and a list of ids with maybe expressions, adds the ids to the table
-- and builds the expression nodes.
declarationAction :: OKType -> [(Token, Maybe AST.EXPRESSION)] -> ParseM [AST.EXPRESSION]
declarationAction oktype l' = do
    let l = map (\(tok, maybe_exp) -> (tok, substituteNothing maybe_exp (default_val oktype))) l'
        assigns = filter (isJust . snd) l
        ids = map (\x -> (tkn_string.fst $ x, tkn_pos.fst $ x)) l :: [(Id, Pos)]
        tkn = fst.head $ l

    scope <- P.topScope
    mapM_ (\(id, pos) -> P.insertVarSym scope id pos oktype) ids

    idExps <- mapM idAction $ map fst assigns
    let exps = map (fromJust . snd) assigns
    zipWithM (assignAction tkn) idExps exps
  where default_val OKInt = Just $ AST.INTEXP 0 OKInt
        default_val OKFloat = Just $ AST.FLOATEXP 0.0 OKFloat
        default_val _ = Nothing
        substituteNothing Nothing a = a
        substituteNothing just _ = just




arrayOrListAccessAction :: Pos -> AST.EXPRESSION -> AST.EXPRESSION -> ParseM AST.EXPRESSION
arrayOrListAccessAction pos exp posExp = do
      checkExpectedType pos OKInt (exp_type posExp) " for index"
      oktype <- checkAndGetArrayOrListType pos (exp_type exp)
      if isListType (exp_type exp) then return $ AST.LISTACCESS exp posExp oktype
                                   else return $ AST.ARRAYACCESS exp posExp oktype

recordMemberAction :: AST.EXPRESSION -> Token -> ParseM AST.EXPRESSION
recordMemberAction exp tkn = do
    case solveNameTypes (exp_type exp) of
         OKRecord scope -> do sym <- P.findSymInRecord scope (tkn_string tkn) (tkn_pos tkn)
                              return $ AST.RECORDACCESS exp (tkn_string tkn) (sym_type sym)
         OKErrorT -> return $ AST.RECORDACCESS exp (tkn_string tkn) OKErrorT
         _ -> do  showExpectedRecord (fst.tkn_pos $ tkn) (exp_type exp)
                  return $ AST.RECORDACCESS exp (tkn_string tkn) OKErrorT

arrayTypeAction :: Pos -> OKType -> Int -> ParseM OKType
arrayTypeAction pos oktype size = do
      return $ elParcheToRowMajor size oktype

elParcheToRowMajor :: Int -> OKType -> OKType
elParcheToRowMajor n (OKArray ni t) = OKArray ni (elParcheToRowMajor n t)
elParcheToRowMajor n t = OKArray n t



nameTypeAction :: Token -> ParseM OKType
nameTypeAction tkn = do
  sym <- P.findNameTypeSym (tkn_string tkn) (tkn_pos tkn)
  case sym of
      NameTypeSym{} -> return $ sym_type sym
      _ -> do return OKErrorT

ifAction :: Token -> AST.EXPRESSION -> (Scope,[AST.INSTRUCTION]) -> AST.IFELSE -> ParseM AST.INSTRUCTION
ifAction tkn condition blk ifelse = do
          checkExpectedType (tkn_pos tkn) OKBoolean (exp_type condition) " for if condition"
          return $ AST.IF condition blk ifelse

cantStopAction :: Token -> AST.EXPRESSION -> (Scope, [AST.INSTRUCTION]) -> ParseM AST.INSTRUCTION
cantStopAction tkn condition blk = do
          checkExpectedType (tkn_pos tkn) OKBoolean (exp_type condition) " for cantstop condition"
          return $ AST.CANTSTOP condition blk


oneMoreTimeAction :: Token -> AST.EXPRESSION -> [AST.EXPRESSION] -> AST.EXPRESSION -> (Scope, [AST.INSTRUCTION]) -> ParseM AST.INSTRUCTION
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


ifYouHaveToAskAction :: Token -> AST.EXPRESSION -> (Scope, [AST.INSTRUCTION]) -> AST.IFELSE -> ParseM AST.IFELSE
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
                                  else return (OKArray (length exps) merged)
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
            if n >= length types then do     showTupleOutOfRange (fst.tkn_pos $ tkn) n (length types)
                                             return $ AST.TUPLEACCESS exp n OKErrorT 0
                                 else return (AST.TUPLEACCESS exp n (types !! n) (sum.(map type_width) $ take n types))
         _ -> do  showNotATuple (fst.tkn_pos $ tkn) (exp_type exp)
                  return $ AST.TUPLEACCESS exp n OKErrorT 0

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

numOperationAction :: Pos -> AST.EXPRESSION -> AST.EXPRESSION -> String -> ParseM AST.EXPRESSION
numOperationAction pos exp1 exp2 op = do
          oktype <- checkNumOpType pos (exp_type exp1) (exp_type exp2)
          return $ AST.ARIT exp1 op exp2 oktype

intOperationAction :: Pos -> AST.EXPRESSION -> AST.EXPRESSION -> String -> ParseM AST.EXPRESSION
intOperationAction pos exp1 exp2 op = do
          oktype <- checkIntOpType pos (exp_type exp1) (exp_type exp2)
          return $ AST.ARIT exp1 op exp2 oktype

-- Builds ASSIGN node, checking the types
assignAction :: Token -> AST.EXPRESSION -> AST.EXPRESSION -> ParseM AST.EXPRESSION
assignAction tkn lhs rhs = do
  oktype <- case (exp_type lhs, exp_type rhs) of
                  (OKErrorT, _) -> return OKErrorT
                  (_, OKErrorT) -> return OKErrorT
                  (lhstype, rhstype) ->
                          if listComp (solveNameTypes lhstype) (solveNameTypes rhstype)
                             then return lhstype
                             else throwNotWhatIExpectedAndImNotSatisfied (fst.tkn_pos $ tkn) (exp_type lhs) (exp_type rhs) "" --TODO Check why
  return $ AST.ASSIGN lhs rhs (oktype)


-- looks for an id in the sym table, returning the corresponging IDEXPRESSION or
idAction :: Token -> ParseM AST.EXPRESSION
idAction tkn = do
          sym <- P.findVarSym (tkn_string tkn) (tkn_pos tkn)
          return $ AST.IDEXPRESSION (tkn_string tkn, sym_scope sym) (sym_type sym)

pointerAction :: Token -> AST.EXPRESSION -> ParseM AST.EXPRESSION
pointerAction tkn exp = do
          oktype <- checkAndGetPointerType (tkn_pos tkn) (exp_type exp)
          return $ AST.POINTER exp oktype

functionCallAction :: Token -> [AST.EXPRESSION] -> ParseM AST.EXPRESSION
functionCallAction tkn exps = do
         sym <- P.findFunction (tkn_string tkn) (tkn_pos tkn) (map exp_type exps)
         case sym of
              FuncSym{} -> return $ AST.FUNCTIONCALL (sym_Id sym,sym_scope sym) (sym_label sym) exps (func_RetType.sym_type $ sym)
              _ -> return $ AST.FUNCTIONCALL (tkn_string tkn, -1) "error" exps OKErrorT

--- 1}}}






-- Type Checking{{{1

-- TODO Show error or maybe rewrite this mess
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
checkAndGetArrayOrListType (line, _) t = throwNotArrayType line t --TODO Or list

-- 1}}}

-- Errors (Horrors){{{1

showExpectedRecord :: Int -> OKType -> ParseM ()
showExpectedRecord ln t = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Expected a record, found " ++ show t ++ "."
    liftIO $ putStrLn $ "Not What I Expected And I'm Not Satisfied.\n"

throwDifferentTypeError :: Int -> OKType -> OKType -> ParseM OKType
throwDifferentTypeError line t1 t2 = do
    liftIO $ putStrLn $ "Error in line " ++ show line ++ ":"
    liftIO $ putStrLn $ "Expected same type, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    liftIO $ putStrLn $ "Where do we go from here? The words are coming out all weird \n"
    return OKErrorT

throwNotNumericalTypeError :: Int -> OKType -> OKType -> ParseM OKType
throwNotNumericalTypeError line t1 t2 = do
    liftIO $ putStrLn $ "Error in line " ++ show line ++ ":"
    liftIO $ putStrLn $ "Expected a number types, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    liftIO $ putStrLn $ "The numbers dont decide. Your system is a lie\n"
    return OKErrorT

throwNotWhatIExpectedAndImNotSatisfied :: Int -> OKType -> OKType -> String -> ParseM OKType
throwNotWhatIExpectedAndImNotSatisfied ln t1 t2 msg = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Expected " ++ show t1 ++ msg ++ ", found " ++ show t2 ++ "."
    liftIO $ putStrLn $ "Not What I Expected And I'm Not Satisfied.\n"
    return OKErrorT

throwNotBooleanError :: Int -> OKType -> OKType -> ParseM OKType
throwNotBooleanError line t1 t2 = do
    liftIO $ putStrLn $ "Error in line " ++ show line ++ ":"
    liftIO $ putStrLn $ "Expected boolean values, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    liftIO $ putStrLn $ "Where do we go from here? The words are coming out all weird \n"
    return OKErrorT

throwNotIntType :: Int -> OKType -> OKType -> ParseM OKType
throwNotIntType line t1 t2 = do
    liftIO $ putStrLn $ "Error in line " ++ show line ++ ":"
    liftIO $ putStrLn $ "Expected int values, found " ++ show t1 ++ " and " ++ show t2 ++ "."
    liftIO $ putStrLn $ "The numbers dont decide. Your system is a lie\n"
    return OKErrorT

throwNotPointerType :: Int -> OKType -> ParseM OKType
throwNotPointerType line t = do
    liftIO $ putStrLn $ "Error in line " ++ show line ++ ":"
    liftIO $ putStrLn $ "Expected pointer value, found " ++ show t ++ "."
    liftIO $ putStrLn $ "The numbers dont decide. Your system is a lie\n"
    return OKErrorT

throwNotArrayType :: Int -> OKType -> ParseM OKType
throwNotArrayType line t = do
    liftIO $ putStrLn $ "Error in line " ++ show line ++ ":"
    liftIO $ putStrLn $ "Expected array value, found " ++ show t ++ "."
    liftIO $ putStrLn $ "Where do we go from here? The words are coming out all weird \n"
    return OKErrorT

--showNameTypeNotDefined :: Id -> Int -> ParseM ()
--showNameTypeNotDefined id ln = do
    --liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    --liftIO $ putStrLn $ "Type " ++ id ++ " is not defined."
    --liftIO $ putStrLn $ "You dont remember. Why dont you remember my name? \n"

--showNameIsNotNameType :: Id -> Int -> Int -> ParseM ()
--showNameIsNotNameType id ln ln2 = do
    --liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    --liftIO $ putStrLn $ id ++ " defined in line " ++ show ln2 ++ " does not refer to a type."
    --liftIO $ putStrLn $ "You dont remember. Why dont you remember my name? \n"


showFoundDifferentTypesInArray :: Int -> [OKType] -> ParseM ()
showFoundDifferentTypesInArray ln types = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Arrays must have a unique type. Found different types:"
    liftIO $ putStrLn $ show types ++ "."
    liftIO $ putStrLn $ "Im amazed that I survived. An airbag saved my life. \n"

showFoundDifferentTypesInList :: Int -> [OKType] -> ParseM ()
showFoundDifferentTypesInList ln types = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Lists must have a unique type. Found different types:"
    liftIO $ putStrLn $ show types ++ "."
    liftIO $ putStrLn $ "Im amazed that I survived. An airbag saved my life. \n"

showConcatExpectedTwoLists :: Int -> OKType -> OKType -> ParseM ()
showConcatExpectedTwoLists ln t1 t2 = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Concat operator expects two lists, found " ++ show t1 ++ " ++ " ++ show t2 ++ "."
    liftIO $ putStrLn $ "Im amazed that I survived. An airbag saved my life. \n"

showConcatExpectedSameType :: Int -> OKType -> OKType -> ParseM ()
showConcatExpectedSameType ln t1 t2 = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Concat operator expects two lists of same type, found " ++ show t1 ++ " ++ " ++ show t2 ++ "."
    liftIO $ putStrLn $ "Let down and hanging around. Crushed like a bug in the ground. \n"

showTupleOutOfRange :: Int -> Int -> Int -> ParseM ()
showTupleOutOfRange ln range2 range = do
   liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
   liftIO $ putStrLn $ "Tuple position out of range. The tuple has only " ++ show range ++ " positions (0-indexed) and tried to access position number " ++ show range ++ "."
   liftIO $ putStrLn $ "Let down and hanging around. Crushed like a bug in the ground. \n"

showNotATuple:: Int -> OKType -> ParseM ()
showNotATuple ln oktype = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Expected a tuple, found a " ++ show oktype ++ "."
    liftIO $ putStrLn $ "Let down and hanging around. Crushed like a bug in the ground. \n"

--showFunctionNotDefined :: Int -> Id -> ParseM ()
--showFunctionNotDefined ln id = do
    --liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    --liftIO $ putStrLn $ "Function " ++ id ++ " not defined."
    --liftIO $ putStrLn $ "You dont remember. Why dont you remember my name?\n"

--showFunctionVariableAlreadyDefined :: Int -> Id -> Int -> OKType -> ParseM ()
--showFunctionVariableAlreadyDefined ln id ln2 oktype = do
    --liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    --liftIO $ putStrLn $ "Function " ++ show id ++ " is already defined in line " ++ show ln2 ++ " with type " ++ show oktype ++ "."
    --liftIO $ putStrLn $ "Picked over by the worms and weird fishes. \n"


--showIDActionNotFound :: Id -> Int -> ParseM ()
--showIDActionNotFound id ln = do
    --liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    --liftIO $ putStrLn $ "ID " ++ id ++ " on left side of assign not found."
    --liftIO $ putStrLn $ "Picked over by the worms and weird fishes. \n"
--- 1}}}

lexwrap :: (Token -> ParseM a) -> ParseM a
lexwrap cont = do
  tkn <- alexGetToken
  --liftIO $ putStrLn $ "    " ++ show tkn
  cont tkn
}
