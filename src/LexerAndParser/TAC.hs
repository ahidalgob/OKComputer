module TAC where

import OKTypes
import SymTable
import qualified AST
import AST

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

type Scope = Int
type SymId = (Id, Scope)

data X = Name SymId
       | IntCons Int
       | FloatCons Float
       | BoolCons Bool
       | CharCons Char
       | Temporal Id

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





data TACkerState = TACkerState { tempCounter :: Int
                               , backPatchMap :: Int
                               }
initTACkerState = TACkerState{ tempCounter = 0
                             , backPatchMap = 0
                             }


type TACkerM a = WriterT TAC (StateT TACkerState IO) a

tacExpression :: EXPRESSION -> TACkerM X
tacExpression IDEXPRESSION{expId = symId}         = return $ Name symId
tacExpression INTEXP{expInt = val}                = return $ IntCons val
tacExpression FLOATEXP{expFloat = val}            = return $ FloatCons val
tacExpression CHAREXP{expChar = val}              = return $ CharCons val
tacExpression BOOLEANEXP{expBooleanVal = val}     = return $ BoolCons val

-- e can be a simple value, an array, a tuple, a list, string
tacExpression (ASSIGN le re t)
  | isSimpleType t = do
      (base, tshift) <- tacLval le
      t1 <- tacExpression re
      case tshift of
        Nothing -> do
          tell [CopyInstr base t1]
          return t1
        Just shift -> do
          tell [ArraySetPos base shift t1]
          return t1
  | otherwise = do
    simplifyAssign le re t
    return $ IntCons 0

  where
    isSimpleType :: OKType -> Bool -- TODO pointer?
    isSimpleType OKArray{} = False
    isSimpleType OKRecord{} = False
    isSimpleType OKTuple{} = False
    isSimpleType _ = True

    -- simplifies x = {1,2,3} into x[0]=1; x[1]=2; x[2]=3; and alike
    simplifyAssign :: EXPRESSION -> EXPRESSION -> OKType -> TACkerM ()

    -- chained assigns
    simplifyAssign e1 (ASSIGN e2 e3 _) oktype = do
      simplifyAssign e2 e3 oktype
      simplifyAssign e1 e2 oktype

    --for literal arrays we transform a = {e1, e2, ...} in a[0]=e1, a[1]=e2...
    simplifyAssign e1 ARRAYEXP{expVals = list} (OKArray n oktype) =
      mapM_ tacExpression [ASSIGN (ARRAYACCESS e1 i oktype) e oktype |
        (i, e) <- zip (map (flip INTEXP OKInt) [0..(n-1)]) list]

    --otherwise we transform a = b in a[0]=b[0], a[1]=b[1]...
    simplifyAssign e1 e2 (OKArray n oktype) =
      mapM_ tacExpression [ASSIGN (ARRAYACCESS e1 i oktype) (ARRAYACCESS e2 i oktype) oktype | i <- map (flip INTEXP OKInt) [0..(n-1)]]


tacExpression COMPAR{}           = undefined
tacExpression NOT{}              = undefined
tacExpression LOGIC{}            = undefined
tacExpression MINUS{}            = undefined
tacExpression ARIT{}             = undefined

tacExpression FUNCTIONCALL{}     = undefined

tacExpression ARRAYACCESS{}      = undefined
tacExpression RECORDACCESS{}     = undefined
tacExpression TUPLEACCESS{}      = undefined

tacExpression NEWLIFE{}          = undefined
tacExpression POINTER{}          = undefined

tacExpression STRINGEXP{}        = undefined
tacExpression ARRAYEXP{}         = undefined {- just generate each expression it will happen when the array is not used but the evaluation can have border cases -}

tacExpression TUPLEEXP{}         = undefined {- can this happen anywhere else other than assigns, returns and params? -}

tacExpression LISTEXP{}          = undefined

tacExpression LISTACCESS{}       = undefined
tacExpression CONCAT{}           = undefined









tacLval :: EXPRESSION -> TACkerM (X, Maybe X)
tacLval = undefined
