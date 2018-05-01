-- Retina - Proyecto de Traductores
-- OurContextMonad, Monad para realizar el analisis de contexto y sus funciones respectivas.
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

module OurContextMonad where

import OurType
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Either
import Data.List

data OurError = OurError Int String | OurErrorNoPos String
instance Error OurError


data Scope = Scope {getList::[(String, OurType)]}

data FuncSign = FuncSign {getId::String, getType:: OurType, getParamList::[(String,OurType)]}

data ContextSymTable = ContextSymTable {getScopes::[Scope], getFuncSigns::[FuncSign]}

data OurContextState = OurContextState {getSymTable::ContextSymTable, getReturnT::(Maybe OurType)}

emptyContextState = OurContextState (ContextSymTable [] []) Nothing

data OurLog = OurLog {getScopesLog::String, getWarningsLog::String}

instance Monoid OurLog where
  mempty = OurLog "" ""
  mappend (OurLog a b) (OurLog c d) = OurLog (a++c) (b++d)

instance Show OurLog where
  show (OurLog a "") = "Alcances:\n"++a++"\n"
  show (OurLog a b) = "Alcances:\n"++a++"\n"++"Advertencias:\n"++b

scopeToOurLog s = OurLog s ""
warningToOurLog s = OurLog "" s


type OurContextMonad a = StateT OurContextState (WriterT OurLog (Either OurError)) a

runOurContextMonad :: OurContextMonad a -> OurContextState -> Either OurError ((a, OurContextState), OurLog)
runOurContextMonad f a = runWriterT (runStateT f a)

getContextLog :: OurContextMonad a -> OurContextState -> String
getContextLog f a = show $ snd $ getRight $ runOurContextMonad f a `catchError` (\(OurError pos s) -> error $ "\nError en linea "++show pos++":\n"++s)
    where
        getRight (Right x) = x

getContextError :: OurContextMonad a -> OurContextState -> Maybe String
getContextError f a = do
    case runOurContextMonad f a of
        Left (OurError pos s) -> return $ "\nError en linea "++show pos++":\n"++s
        Right _ -> Nothing


lookInList :: String -> [(String, OurType)] -> Maybe OurType
lookInList s l = snd <$> find ((==s).fst) l

lookInLastScope :: String -> OurContextMonad (Maybe OurType)
lookInLastScope s = lookInList s.getList.head'.getScopes.getSymTable <$> get
                    where head' [] = Scope []
                          head' (a:as) = a

lookInSymTable :: String -> OurContextMonad (Maybe OurType)
lookInSymTable s = msum . map (lookInList s) . (map getList).getScopes.getSymTable <$> get

newScope :: OurContextMonad ()
newScope = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        oldScopes = getScopes oldSymTable
        newSymTable = oldSymTable {getScopes = (Scope []):oldScopes}
    put $ oldState {getSymTable = newSymTable}

removeLastScope :: OurContextMonad ()
removeLastScope = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        newSymTable = oldSymTable {getScopes = tail.getScopes $ oldSymTable }
    put $ oldState { getSymTable = newSymTable}
        
addToSymTable :: (String, OurType) -> OurContextMonad ()
addToSymTable pair = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        oldScopeList = getScopes oldSymTable
        newLastScope = Scope $ pair:(getList $ head oldScopeList)
        newScopeList = newLastScope:(tail oldScopeList)
        newSymTable = oldSymTable { getScopes = newScopeList }
    put $ oldState { getSymTable = newSymTable }

addFunctionSign :: String -> [(String, OurType)] -> OurType -> OurContextMonad () -- No crea el scope
addFunctionSign s params typ = do 
    oldState <- get
    let newFunc = FuncSign s typ params
        oldSymTable = getSymTable oldState
        newSymTable = oldSymTable { getFuncSigns = newFunc:(getFuncSigns oldSymTable) }
    put $ oldState { getSymTable = newSymTable }

lookFunction :: String -> OurContextMonad Bool
lookFunction s = do
    oldState <- get  
    let funcsList = map (getId) (getFuncSigns.getSymTable $ oldState)
        xs = filter (==s) funcsList
        ans = case xs of 
            [] -> False
            _ -> True
    return ans

getFunctionReturnType :: String -> OurContextMonad OurType
getFunctionReturnType s = do
    oldState <- get
    let listFunc = getFuncSigns.getSymTable $ oldState
        func = filter ((==s).getId) listFunc 
    return $ getType.head $ func

checkFunction :: String -> [OurType] -> OurContextMonad (Bool,Int)
checkFunction s list = do
    oldState <- get
    let listF = getParamList $ head $ filter ((==s).getId) (getFuncSigns.getSymTable $ oldState)
        listType = map snd listF
    return (listType==list,length listType)

setReturnType :: Maybe OurType -> OurContextMonad ()
setReturnType typeR = do 
    oldState <- get
    put $ oldState { getReturnT = typeR }

getReturnType :: OurContextMonad (Maybe OurType)
getReturnType = get >>= (return.getReturnT)

lastScopeToLog :: String -> OurContextMonad ()
lastScopeToLog scopeName = do
    scopes <- getScopes.getSymTable <$> get
    let nested = length scopes
        lastScope = head scopes
        ident = concat (replicate nested "|   ")
    tell.scopeToOurLog $ ident++"Alcance "++scopeName++":\n"
    tell.scopeToOurLog $ concatMap (\s -> ident++"|> "++s++"\n" ) $ map showVarAndType $ reverse.getList $ lastScope
    where
        showVarAndType (s, t) = s ++" : "++ show t
warningToLog :: String -> OurContextMonad ()
warningToLog warning = do
    tell.warningToOurLog $ warning++"\n"