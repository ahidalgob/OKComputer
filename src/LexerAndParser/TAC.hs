module TAC where

import OKTypes
import SymTable
import qualified AST
import AST

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

type Scope = Int
type SymId = (Id, Scope)
type Width = Int

data X = Name SymId
       | IntCons Int
       | FloatCons Float
       | BoolCons Bool
       | CharCons Char
       | Temporal Int Width

type Label = String

type TAC = [Instruction]

data Instruction =
    BinOpInstr X X BinOp X    -- X = Y op Z              (1)
  | UnOpInstr X UnOp X        -- X = op Y                (2)
  | CopyInstr X X             -- X = Y                   (3)
  | Goto Label                -- goto Label              (4)
  | IfGoto X Label            -- if X goto Label         (5)
  | IfFalseGoto X Label       -- ifFalse X goto Label
  | IfRelGoto X RelOp X Label -- if X relop Y goto Label (6)
  | Param X                   -- param X                 (7)
  | Call SymId Int            -- call p, n
  | CallAssign X SymId Int    -- X = call p, n
  | Return X                  -- return X
  | ArrayGetPos X X X         -- x = y[i]                (8)
  | ArraySetPos X X X         -- x[i] = y
  | GetAddress X X            -- x = &y                  (9)
  | GetContents X X           -- x = *y

data BinOp = Add | Sub | Mul | Div | Mod | And | Or
data UnOp = Not | Minus
data RelOp = LTOET | LT | GTOET | GT | ET | NET





data TACkerState = TACkerState { tmpCounter :: Int
                               , tmpWidths :: [Width]
                               , backPatchMap :: Int
                               }
initTACkerState = TACkerState{ tmpCounter = 0
                             , tmpWidths = []
                             , backPatchMap = 0
                             }


type TACkerM a = WriterT TAC (StateT TACkerState IO) a

fresh :: Width -> TACkerM X
fresh w = do
  tmpCnt <- gets tmpCounter
  tmpWs <- gets tmpWidths
  modify (\s -> s{tmpCounter = tmpCnt+1, tmpWidths = w:tmpWs})
  return $ Temporal tmpCnt w


-- tacExpression {{{1

tacExpression :: EXPRESSION -> TACkerM X
tacExpression IDEXPRESSION{expId = symId}         = return $ Name symId
tacExpression INTEXP{expInt = val}                = return $ IntCons val
tacExpression FLOATEXP{expFloat = val}            = return $ FloatCons val
tacExpression CHAREXP{expChar = val}              = return $ CharCons val
tacExpression BOOLEANEXP{expBooleanVal = val}     = return $ BoolCons val

-- e can be a simple value, an array, a tuple, a list, string
tacExpression (ASSIGN le re t) = do
  (base, tshift) <- tacLval le
  t1 <- tacExpression re
  let width = type_width t
  if isSimpleType t
    then
      case tshift of
        Nothing -> tell [CopyInstr base t1]
        Just shift -> tell [ArraySetPos base shift t1]
    else
      case tshift of
        Nothing -> do
          let setPos i = do
                  t <- fresh (type_width OKInt)
                  tell [ ArrayGetPos t t1 (IntCons i)
                       , ArraySetPos base (IntCons i) t]
          mapM_ setPos [0,4..(width-1)]
        Just shift -> do
          let setPos i = do
                  t <- fresh (type_width OKInt)
                  t2 <- fresh (type_width OKInt)
                  tell [ BinOpInstr t2 shift Add (IntCons i)
                       , ArrayGetPos t t1 (IntCons i)
                       , ArraySetPos base t2 t]
          mapM_ setPos [0,4..(width-1)]
  return t1

  where
    isSimpleType :: OKType -> Bool
    isSimpleType OKArray{} = False
    isSimpleType OKRecord{} = False
    isSimpleType OKTuple{} = False
    isSimpleType _ = True


tacExpression (MINUS exp t) = do
  t1 <- tacExpression exp
  t <- fresh (type_width t)
  tell [UnOpInstr t Minus t1]
  return t

tacExpression (ARIT exp1 opStr exp2 t) = do
  t1 <- tacExpression exp1
  t2 <- tacExpression exp2
  let op = f opStr
  t <- fresh (type_width t)
  tell [BinOpInstr t t1 op t2]
  return t
  where
  f "+" = Add
  f "-" = Sub
  f "*" = Mul
  f "/" = Div
  f "%" = Mod
  f "div" = Div
  f "mod" = Mod
  f _ = error "what()"

tacExpression COMPAR{}           = undefined
tacExpression NOT{}              = undefined
tacExpression LOGIC{}            = undefined

tacExpression FUNCTIONCALL{}     = undefined

tacExpression e@(ARRAYACCESS exp expIn t) = do
  (base, Just shift) <- tacLval e
  let width = type_width t
  t <- fresh (type_width t)
  let setPos i = do
          t1 <- fresh (type_width OKInt)
          t2 <- fresh (type_width OKInt)
          tell [ BinOpInstr t1 shift Add (IntCons i)
               , ArrayGetPos t2 base t1
               , ArraySetPos t (IntCons i) t2]
  mapM_ setPos [0,4..(width-1)]
  return t

tacExpression TUPLEACCESS{}      = undefined

tacExpression RECORDACCESS{}     = undefined

tacExpression NEWLIFE{}          = undefined
tacExpression POINTER{}          = undefined

tacExpression ARRAYEXP{}         = undefined
tacExpression TUPLEEXP{}         = undefined

tacExpression STRINGEXP{}        = undefined
tacExpression LISTEXP{}          = undefined

tacExpression LISTACCESS{}       = undefined
tacExpression CONCAT{}           = undefined




-- tacLval {{{1
tacLval :: EXPRESSION -> TACkerM (X, Maybe X)
tacLval IDEXPRESSION{expId = symId} = return (Name symId, Nothing)

tacLval (ARRAYACCESS exp expIn t) = do
  (base, mshift) <- tacLval exp
  tin <- tacExpression expIn
  let w = type_width t
  t_newshift <- fresh (type_width OKInt)
  tell [BinOpInstr t_newshift tin Mul (IntCons w)]
  case mshift of
    Nothing -> return (base, Just t_newshift)
    Just t_shift -> do
      t_completeshift <- fresh (type_width OKInt)
      tell [BinOpInstr t_completeshift t_newshift Add t_shift]
      return (base, Just t_completeshift)

tacLval (TUPLEACCESS exp pos t new_shift) = do
  (base, mshift) <- tacLval exp
  case mshift of
    Nothing -> return (base, Just (IntCons new_shift))
    Just t_shift -> do
      t_completeshift <- fresh (type_width OKInt)
      tell [BinOpInstr t_completeshift t_shift Add (IntCons new_shift)]
      return (base, Just t_completeshift)


tacLval RECORDACCESS{} = undefined
tacLval LISTACCESS{} = undefined

