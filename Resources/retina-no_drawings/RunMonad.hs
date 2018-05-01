-- Retina - Proyecto de Traductores
-- OurMonad, Monad para realizar el analisis de contexto y sus funciones respectivas.
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

module RunMonad where

import AST
import OurType
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Either
import Data.List
import Data.Maybe
import Data.Fixed (mod')

data CursorStatus = On | Off deriving (Show,Eq)

data Val = BooleanVal Bool | VoidVal | NumberVal Double deriving (Eq)
instance Show Val where
    show (BooleanVal b) = show b
    show (NumberVal d) = if mod' d 1 == 0 
                            then show.floor $ d
                            else show d
    show (VoidVal) = "void"

type Pos = (Double, Double)

type Segment = ((Double, Double),(Double, Double))

type Direction = Double

type VarDescript = (String,Val,Bool,OurType)

type FuncDescript = (String,[String],[InstrN],OurType)

data Cursor = Cursor {getPosition:: Pos, getDirection::Direction, getStatus::CursorStatus } deriving Show

data Scope = Scope {getList::[VarDescript]} deriving Show

data SymTable = SymTable {getScopes::[Scope]} deriving Show

data OurState = OurState {getSymTable::SymTable, getCursor::Cursor,getFunDec::FuncDec, getMaxDown::Double, getMaxUp::Double, getMaxRight::Double,getMaxLeft::Double} deriving Show

ourEmptyState = OurState (SymTable []) (Cursor (0,0) 0 On) (FuncDec []) 0 0 0 0

data FuncDec = FuncDec { getDec:: [FuncDescript]} deriving Show

type RunMonad a = StateT OurState (WriterT [Segment] IO ) a

runRunMonad :: RunMonad a -> OurState -> IO (((a,OurState),[Segment]))
runRunMonad f a = runWriterT (runStateT f a)

addToSymTable :: VarDescript -> RunMonad ()
addToSymTable tuple = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        oldScopeList = getScopes oldSymTable
        newLastScope = Scope $ tuple:(getList $ head oldScopeList)
        newScopeList = newLastScope:(tail oldScopeList)
        newSymTable = oldSymTable { getScopes = newScopeList }
    put $ oldState { getSymTable = newSymTable }

fst' :: (String,t,t1,t2) -> String
fst' (s,_,_,_) = s

thrd' :: (t,Val,t1,t2) -> Val
thrd' (_,v,_,_) = v

toRadian :: Double -> Double
toRadian num = num*pi/180 

lookInList :: String -> [VarDescript] -> Maybe VarDescript
lookInList s l = find ((==s).fst') l

lookInSymTable :: String -> RunMonad (VarDescript)
lookInSymTable s = fromJust.msum . map (lookInList s) . (map getList).getScopes.getSymTable <$> get

removeLastScope :: RunMonad ()
removeLastScope = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        newSymTable = oldSymTable {getScopes = tail.getScopes $ oldSymTable }
    put $ oldState { getSymTable = newSymTable}

newScope :: RunMonad ()
newScope = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        oldScopes = getScopes oldSymTable   
        newSymTable = oldSymTable {getScopes = (Scope []):oldScopes}
    put $ oldState {getSymTable = newSymTable}

changeValInScope :: [VarDescript] -> String -> Val -> (Bool,[VarDescript])
changeValInScope scope string newVal = 
    foldl changeFun (False,[]) scope
        where changeFun (changed,list) (name,val,mutable,typeV) = case name==string of 
                        True -> (True,(name,newVal,mutable,typeV):list) 
                        _ -> (changed,(name,val,mutable,typeV):list) 

changeValInSymTable :: (String,Val) -> RunMonad ()
changeValInSymTable (string,newVal) = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        newScopes = snd $ foldl fun (False,[]) (getScopes oldSymTable)
        newSymTable = oldSymTable {getScopes = reverse newScopes}
    put $ oldState {getSymTable = newSymTable}
    where fun (changed,scopes) list = case changed of
                                        True -> (changed,list:scopes)
                                        _ -> case (changeValInScope (getList list) string newVal) of
                                                (True,changedScope) -> (True,Scope changedScope:scopes)
                                                (False,equalScope) ->  (changed,Scope equalScope:scopes)

getSymTable' :: RunMonad (SymTable)
getSymTable' = do
    getSymTable <$> get

createSymTable :: RunMonad ()
createSymTable = do
    oldState <- get
    put $ oldState { getSymTable = SymTable $ [] }

setSymTable :: SymTable -> RunMonad ()
setSymTable newSymTable = do
    oldState <- get
    put $ oldState { getSymTable = newSymTable }

---------------------------------------- FUNDEC -----------------------------------------

addToFunDec :: FuncDescript -> RunMonad ()
addToFunDec tuple = do
    oldState <- get
    let oldFunDec = getFunDec oldState
        newFunDec = FuncDec $ tuple:(getDec oldFunDec)
    put $ oldState { getFunDec = newFunDec }

findFunDec :: String -> RunMonad FuncDescript
findFunDec s = do
    fromJust.find ((==s).fst').getDec.getFunDec <$> get


---------------------------------------- CURSOR -----------------------------------------

onCursor ::  RunMonad () 
onCursor = do
    oldState <- get
    let oldCursor = getCursor oldState
        newCursor = oldCursor { getStatus = On }
    put $ oldState { getCursor = newCursor }

offCursor :: RunMonad () 
offCursor = do
    oldState <- get
    let oldCursor = getCursor oldState
        newCursor = oldCursor { getStatus = Off }
    put $ oldState { getCursor = newCursor }

forward :: Double -> RunMonad ()
forward steps = do
    oldState <- get
    let cursor = getCursor oldState
        (x,y) = getPosition cursor
        (nx, ny) = (x+steps*cos direction, y+steps*sin direction)
        direc = getDirection cursor
        direction = toRadian direc
        down = (getMaxDown oldState) `min` y `min` (ny)
        up = (getMaxUp oldState) `max` y `max` (ny)
        left = getMaxLeft oldState `min` x `min` (nx)
        right = getMaxRight oldState `max` x `max` (nx)
    case (getStatus cursor) of 
        Off -> return ()
        On -> do
            tell [((x,y),(nx, ny))]
            put $ oldState { getMaxRight = right, getMaxLeft = left, getMaxUp = up, getMaxDown = down }
    setPosition (x+ steps*cos direction,y+ steps*sin direction)

backward :: Double -> RunMonad ()
backward steps = do
    oldState <- get
    let cursor = getCursor oldState
        (x,y) = getPosition cursor
        (nx, ny) = (x-steps*cos direction, y-steps*sin direction)
        direc = getDirection cursor
        direction = toRadian direc
        down = (getMaxDown oldState) `min` y `min` (ny)
        up = (getMaxUp oldState) `max` y `max` (ny)
        left = getMaxLeft oldState `min` x `min` (nx)
        right = getMaxRight oldState `max` x `max` (nx)
    case (getStatus cursor) of 
        Off -> return ()
        On -> do
            tell [((x,y),(nx,ny))]
            put $ oldState { getMaxRight = right, getMaxLeft = left, getMaxUp = up, getMaxDown = down }
    setPosition (x- steps*cos direction,y- steps*sin direction)

setPosition :: Pos -> RunMonad()
setPosition (x,y) = do
    oldState <- get
    let oldCursor = getCursor oldState
        newCursor = oldCursor { getPosition = (x,y) }
    put $ oldState { getCursor = newCursor } 


rotater :: Double -> RunMonad ()
rotater deegre = do
    oldState <- get
    let oldCursor = getCursor oldState
        oldDirection = getDirection oldCursor 
        newCursor = oldCursor {getDirection = oldDirection - deegre}
    put $ oldState { getCursor = newCursor }

rotatel :: Double -> RunMonad ()
rotatel deegre = do
    oldState <- get
    let oldCursor = getCursor oldState
        oldDirection = getDirection oldCursor 
        newCursor = oldCursor {getDirection = oldDirection + deegre}
    put $ oldState { getCursor = newCursor }



runMonad f a = runWriterT (runStateT f a)
