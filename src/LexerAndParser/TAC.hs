module TAC where

import OKTypes
import SymTable
import qualified AST
import AST

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Data.Map.Strict

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
  | PopParam X                -- popParam X                 (7)
  | Call Label Int            -- call p, n
  | CallAssign X Label Int    -- X = call p, n
  | ReturnVoid                -- return
  | Return X                  -- return X
  | ArrayGetPos X X X         -- x = y[i]                (8)
  | ArraySetPos X X X         -- x[i] = y
  | GetAddress X X            -- x = &y                  (9)
  | GetContents X X           -- x = *y
  | PutLabel Label            -- Label:

data BinOp = Add | Sub | Mul | Div | Mod | And | Or
data UnOp = Not | Minus
data RelOp = LTOET | LT2 | GTOET | GT2 | ET | NET




-- TACkerM {{{1
data TACkerState = TACkerState { tmpCounter :: Int
                               , tmpWidths :: [Width]
                               , labelCounter :: Int
                               , fakeLabelCounter :: Int
                               , startLabels :: [Label]
                               , endLabels :: [Label]
                               , backPatchMap :: Map Label Label
                               }
initTACkerState = TACkerState{ tmpCounter = 0
                             , tmpWidths = []
                             , labelCounter = 0
                             , fakeLabelCounter = 0
                             , startLabels = []
                             , endLabels = []
                             , backPatchMap = empty
                             }


type TACkerM a = WriterT TAC (StateT TACkerState IO) a

backPatch :: [Label] -> Label -> TACkerM ()
backPatch [] _ = return ()

backPatch (fl:fls) l = do
  oldMap <- gets backPatchMap
  let newMap = insert fl l oldMap
  modify (\s -> s{backPatchMap=newMap})
  backPatch fls l

fresh :: Width -> TACkerM X
fresh w = do
  tmpCnt <- gets tmpCounter
  tmpWs <- gets tmpWidths
  modify (\s -> s{tmpCounter = tmpCnt+1, tmpWidths = w:tmpWs})
  return $ Temporal tmpCnt w

freshLabel :: String -> TACkerM Label
freshLabel str = do
  labelCnt <- gets labelCounter
  return $ str ++ "L" ++ show labelCnt

increaseLabelCounter :: TACkerM ()
increaseLabelCounter = do
  labelCnt <- gets labelCounter
  modify (\s -> s{labelCounter = labelCnt+1})

freshFakeLabel :: TACkerM Label
freshFakeLabel = do
  labelCnt <- gets fakeLabelCounter
  modify (\s -> s{fakeLabelCounter = labelCnt+1})
  return $ "FL" ++ show labelCnt

popCycleLabel :: TACkerM ()
popCycleLabel = do
  startLs <- gets startLabels
  endLs <- gets endLabels
  modify (\s -> s{startLabels = tail startLs})
  modify (\s -> s{endLabels = tail endLs})

topStartCycleLabel :: TACkerM Label
topStartCycleLabel = head <$> gets startLabels

topEndCycleLabel :: TACkerM Label
topEndCycleLabel = head <$> gets endLabels

pushCycleLabel :: Label -> Label -> TACkerM ()
pushCycleLabel sl el = do
  startLs <- gets startLabels
  endLs <- gets endLabels
  modify (\s -> s{startLabels = sl:startLs})
  modify (\s -> s{endLabels = el:endLs})


-- tacInstruction {{{1

tacInstruction :: INSTRUCTION -> TACkerM ()
tacInstruction (EXPRESSIONINST e) = void $ tacExpression e
tacInstruction (IF exp instrs ifelse) = do
  (tl, fl) <- tacBoolean exp
  trueLabel <- freshLabel "ifTrue"
  falseLabel <- freshLabel "ifFalse"
  endLabel <- freshLabel "ifEnd"
  increaseLabelCounter

  backPatch tl trueLabel
  backPatch fl falseLabel

  tell [ PutLabel trueLabel ]
  mapM_ tacInstruction instrs
  tell [ Goto endLabel ]
  tell [ PutLabel falseLabel ]
  tacIfElse ifelse endLabel
  tell [ PutLabel endLabel ]


tacInstruction (CANTSTOP exp instrs) = do
  startLabel <- freshLabel "startCantStop"
  enterLabel <- freshLabel "enterCantStop"
  endLabel <- freshLabel "endCantStop"
  increaseLabelCounter

  pushCycleLabel enterLabel endLabel

  tell [ PutLabel startLabel ]
  (tl, fl) <- tacBoolean exp
  tell [ PutLabel enterLabel ]
  mapM_ tacInstruction instrs
  tell [ Goto startLabel
       , PutLabel endLabel ]

  popCycleLabel

  backPatch tl enterLabel
  backPatch fl endLabel


tacInstruction (ONEMORETIME iniExps condExp stepExp instrs) = do
  startLabel <- freshLabel "startOneMoreTime"
  enterLabel <- freshLabel "enterOneMoreTime"
  endLabel <- freshLabel "endOneMoreTime"
  increaseLabelCounter

  mapM_ tacExpression iniExps
  pushCycleLabel enterLabel endLabel
  tell [ PutLabel startLabel ]
  (tl, fl) <- tacBoolean condExp
  tell [ PutLabel enterLabel ]
  mapM_ tacInstruction instrs
  tacExpression stepExp
  tell [ Goto startLabel
       , PutLabel endLabel ]

  popCycleLabel

  backPatch tl enterLabel
  backPatch fl endLabel

tacInstruction BREAKTHRU = do
  endLabel <- topEndCycleLabel
  tell [ Goto endLabel ]

tacInstruction CONTINUE = do
  startLabel <- topStartCycleLabel
  tell [ Goto startLabel ]

tacInstruction (GETBACK (Just exp)) = do
  t <- tacExpression exp
  tell [ Return t ]

tacInstruction (GETBACK Nothing) =
  tell [ ReturnVoid ]

tacInstruction EXITMUSIC{} = undefined
tacInstruction GOING{} = undefined
tacInstruction GOINGSLOWLY{} = undefined
tacInstruction GOINGMENTAL{} = undefined
tacInstruction READMYMIND{} = undefined
tacInstruction AMNESIAC{} = undefined


-- tacIfElse {{{2
tacIfElse :: IFELSE -> Label -> TACkerM ()
tacIfElse (IFELSEVOID) endLabel = tell [ Goto endLabel ]
tacIfElse (IFASK exp instrs ifelse) endLabel = do
  (tl, fl) <- tacBoolean exp
  trueLabel <- freshLabel "elseTrue"
  falseLabel <- freshLabel "elseFalse"
  increaseLabelCounter

  backPatch tl trueLabel
  backPatch fl falseLabel

  tell [ PutLabel trueLabel ]
  mapM_ tacInstruction instrs
  tell [ Goto endLabel ]
  tell [ PutLabel falseLabel ]
  tacIfElse ifelse endLabel

tacIfElse (OTHERSIDE instrs) endLabel = do
  mapM_ tacInstruction instrs
  tell [ Goto endLabel ]

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

tacExpression e@COMPAR{} = booleanToTemporal e

tacExpression e@NOT{} = booleanToTemporal e

tacExpression e@LOGIC{} = booleanToTemporal e

tacExpression (FUNCTIONCALL label args tpe) = do
  let n = length args
  ts <- mapM tacExpression args
  mapM_ (\t -> tell [ Param t]) ts
  case func_RetType tpe of
    OKVoid -> do
      tell [ Call label n ]
      mapM_ (\t -> tell [ PopParam t ]) (reverse ts)
      return (IntCons 0)
    retT -> do
      t <- fresh (type_width retT)
      tell [ CallAssign t label n ]
      mapM_ (\t -> tell [ PopParam t ]) (reverse ts)
      return t



tacExpression e@ARRAYACCESS{exp_type=t} = do
  (base, Just shift) <- tacLval e
  copyFromShift base shift $ type_width t

tacExpression e@TUPLEACCESS{exp_type=t} = do
  (base, Just shift) <- tacLval e
  copyFromShift base shift $ type_width t

tacExpression e@RECORDACCESS{exp_type=t} = do
  (base, Just shift) <- tacLval e
  copyFromShift base shift $ type_width t

tacExpression (ARRAYEXP exps tpe) =
  copyListOfExpressions exps (type_width tpe)

tacExpression (TUPLEEXP exps tpe) =
  copyListOfExpressions exps (type_width tpe)

tacExpression NEWLIFE{}          = undefined
tacExpression POINTER{}          = undefined

tacExpression STRINGEXP{}        = undefined

tacExpression LISTEXP{}          = undefined
tacExpression LISTACCESS{}       = undefined
tacExpression CONCAT{}           = undefined


-- Auxiliary {{{1

booleanToTemporal :: EXPRESSION -> TACkerM X
booleanToTemporal e = do
  t <- fresh (type_width OKBoolean)
  (trueList, falseList) <- tacBoolean e
  trueLabel <- freshLabel "booleanToTempTrue"
  falseLabel <- freshLabel "booleanToTempFalse"
  exitLabel <- freshLabel "booleanToTempExit"
  increaseLabelCounter

  backPatch trueList trueLabel
  backPatch falseList falseLabel
  tell [ PutLabel trueLabel
       , CopyInstr t (BoolCons True)
       , Goto exitLabel
       , PutLabel falseLabel
       , CopyInstr t (BoolCons False)
       , PutLabel exitLabel ]
  return t

-- t <- fresh
-- t[0] = base[shift]
-- t[4] = base[shift+4]
-- ...
copyFromShift :: X -> X -> Int -> TACkerM X
copyFromShift base shift width = do
  t <- fresh width -- temporal where everything is saved
  let setPos i = do
          t1 <- fresh (type_width OKInt)
          t2 <- fresh (type_width OKInt)
          tell [ BinOpInstr t1 shift Add (IntCons i)
               , ArrayGetPos t2 base t1
               , ArraySetPos t (IntCons i) t2]
  mapM_ setPos [0,4..(width-1)]
  return t


-- t[shift] = t1[0]
-- t[shift+4] = t1[4]
-- ...
copyToShift :: X -> Int -> X -> Int -> TACkerM () -- Very naive
copyToShift t shift t1 width = do
  let setPos i = do
          t2 <- fresh (type_width OKInt)
          t3 <- fresh (type_width OKInt)
          tell [ BinOpInstr t2 (IntCons shift) Add (IntCons i)
               , ArrayGetPos t3 t1 (IntCons i)
               , ArraySetPos t t2 t3]
  mapM_ setPos [0,4..(width-1)]

copyListOfExpressions :: [EXPRESSION] -> Int -> TACkerM X
copyListOfExpressions exps totalWidth = do
  t <- fresh totalWidth
  let f shift exp = do
      t1 <- tacExpression exp
      let expWidth = type_width.exp_type $ exp
      copyToShift t shift t1 expWidth
      return $ shift + expWidth
  foldM_ f 0 exps
  return t

-- tacBoolean {{{1
tacBoolean :: EXPRESSION -> TACkerM ([Label], [Label])
tacBoolean (BOOLEANEXP True _) = do
  fl <- freshFakeLabel
  tell [ Goto fl ]
  return ([fl],[])

tacBoolean (BOOLEANEXP False _) = do
  fl <- freshFakeLabel
  tell [ Goto fl ]
  return ([],[fl])

tacBoolean (COMPAR exp1 comp exp2 _) = do
  t1 <- tacExpression exp1
  t2 <- tacExpression exp2
  trueFl <- freshFakeLabel
  falseFl <- freshFakeLabel
  tell [ IfRelGoto t1 (f comp) t2 trueFl
       , Goto falseFl ]
  return ([trueFl], [falseFl])
  where
  f "<=" = LTOET
  f "<" = LT2
  f ">=" = GTOET
  f ">" = GT2
  f "==" = ET
  f "!=" = NET

tacBoolean (NOT exp _) = do
  (tl, fl) <- tacBoolean exp
  return (fl, tl)

tacBoolean (LOGIC exp1 compar exp2 _)
  | compar == "and" = do
    label <- freshLabel "andFirstTrue"
    increaseLabelCounter
    (tl1, fl1) <- tacBoolean exp1
    tell [ PutLabel label ]
    (tl2, fl2) <- tacBoolean exp2
    backPatch tl1 label
    return (tl2, fl1++fl2)

  | compar == "or" = do
    label <- freshLabel "orFirstFalse"
    increaseLabelCounter
    (tl1, fl1) <- tacBoolean exp1
    tell [ PutLabel label ]
    (tl2, fl2) <- tacBoolean exp2
    backPatch fl1 label
    return (tl1++tl2, fl2)

tacBoolean e = do
  t <- tacExpression e
  trueFl <- freshFakeLabel
  falseFl <- freshFakeLabel
  tell [ IfGoto t trueFl
       , Goto falseFl ]
  return ([trueFl], [falseFl])


-- tacLval {{{1
tacLval :: EXPRESSION -> TACkerM (X, Maybe X)
tacLval IDEXPRESSION{expId = symId} = return (Name symId, Nothing)

tacLval (ARRAYACCESS exp expIn t) = do
  (base, mshift) <- tacLval exp
  tin <- tacExpression expIn -- position expression
  let w = type_width t -- width
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

