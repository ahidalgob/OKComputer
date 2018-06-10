module ParseMonad where
import Tokens
import LowLevelAlex
import SymTable

import Control.Monad.State.Lazy
import Control.Monad.Except

import Data.List(find)

# 1 "/usr/include/stdc-predef.h" 1 3 4
# 17 "/usr/include/stdc-predef.h" 3 4
{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/home/hp/haskell-platform/build/ghc-bindist/local/lib/ghc-8.2.2/include/ghcversion.h" #-}
{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc9791_0/ghc_2.h" #-}
{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------

import Data.Char (ord)



data ParseState = ParseState {
        alex_inp :: AlexInput,    -- the current input
        alex_scd :: !Int,          -- the current startcode

        alex_invalidC :: [(Char, Pos)],
        alex_strPos :: Pos,
        alex_str :: String,

        last_new_line :: Bool,

        state_ScopeStack :: ScopeStack,
        state_ScopeSet :: ScopeSet,
        state_NextScope :: Int,
        state_SymTable :: SymTable
} deriving Show


initParseState :: String -> ParseState
initParseState s = ParseState{alex_inp = (alexStartPos, '\n', [], s),
                              alex_scd = 0,
                              alex_invalidC = [],
                              alex_strPos = (0,0),
                              alex_str = "",
                              last_new_line = True,

                              state_ScopeStack = emptyScopeStack,
                              state_ScopeSet = emptyScopeSet,
                              state_NextScope = 2,  -- TODO we need to insert every predefined function on 0
                              state_SymTable = emptySymTable}

--------------------------------------------------------
----------------- Error --------------------------------
--------------------------------------------------------

data ParseMError = IdNotFound Id Pos |
                   AlreadyDefinedInScope Sym |
                   ParseError String
                 deriving Show

catchIdNotFound :: ParseMError -> ParseM Scope
catchIdNotFound (IdNotFound id pos) = do
  liftIO $ putStrLn $ "Id " ++ id ++ " is not defined. Line " ++ show (fst pos) ++ "."
  return (-1)

catchAlreadyDefinedInScope (AlreadyDefinedInScope sym) = do
  liftIO $ putStrLn $ "Id " ++ (sym_Id sym) ++ " is already defined in the same scope."
  liftIO $ putStrLn $ "Line " ++ show (fst.sym_pos $ sym) ++ ". Original definition at line __" -- TODO
--------------------------------------------------------
--------------------------------------------------------

type ParseM a = ExceptT ParseMError (StateT ParseState IO) a


runParseM :: ParseM a -> String -> IO(Either ParseMError a, ParseState)
runParseM f s = runStateT (runExceptT f) (initParseState s)

--------------------------------------------------------
--------------------------------------------------------
--------------------------------------------------------
--------------------------------------------------------

getAlexInput :: ParseM AlexInput
getAlexInput = gets alex_inp

setAlexInput :: AlexInput -> ParseM ()
setAlexInput inp = modify (\s -> s{alex_inp=inp})

getAlexStartCode :: ParseM Int
getAlexStartCode = gets alex_scd

setAlexStartCode:: Int -> ParseM ()
setAlexStartCode sc = modify (\s -> s{alex_scd=sc})



pushStrC :: Char -> ParseM ()
pushStrC c = modify (\s -> s{alex_str = c:alex_str s})

getAndClearStr :: ParseM String
getAndClearStr = do
  str <- gets alex_str
  modify (\s -> s{alex_str = ""})
  return $ reverse str

setStrPos :: Pos -> ParseM ()
setStrPos pos = modify (\s -> s{alex_strPos = pos})

getStrPos :: ParseM Pos
getStrPos = gets alex_strPos

pushInvalidC :: Char -> Pos -> ParseM ()
pushInvalidC c pos = modify (\s -> s{alex_invalidC = (c,pos):alex_invalidC s})

getLastNewLine :: ParseM Bool
getLastNewLine = gets last_new_line

setLastNewLine :: Bool -> ParseM ()
setLastNewLine b = modify (\s -> s{last_new_line = b})

--------------------------------------------------------
--------------------------------------------------------
--------------------------------------------------------


-----------------------------------------------
----------------------- SYM TABLE
-----------------------------------------------

---------------------- ScopeStack
stateScopeStackTop :: ParseM Scope
stateScopeStackTop = head <$> (gets state_ScopeStack)

stateScopeStackPop :: ParseM ()
stateScopeStackPop = modify
      (\s -> s{state_ScopeStack = tail (state_ScopeStack s)})

stateScopeStackPush :: Scope -> ParseM ()
stateScopeStackPush sc = modify
      (\s -> s{state_ScopeStack = sc:(state_ScopeStack s)})


------------------- ScopeSet
stateScopesMember :: Scope -> ParseM Bool
stateScopesMember sc = (scopeSetMember sc) <$> (gets state_ScopeSet)

stateScopesInsert :: Scope -> ParseM ()
stateScopesInsert sc = do
  set <- gets state_ScopeSet
  modify (\s -> s{state_ScopeSet = scopeSetInsert sc set})

stateScopesDelete :: Scope -> ParseM ()
stateScopesDelete sc = do
  set <- gets state_ScopeSet
  modify (\s -> s{state_ScopeSet = scopeSetDelete sc set})


stateBeginScope :: ParseM ()
stateBeginScope = do
  nextScope <- gets state_NextScope
  --liftIO $ putStrLn $ "Enter Scope: " ++ show nextScope
  stateScopeStackPush nextScope
  stateScopesInsert nextScope
  modify (\s -> s{state_NextScope = nextScope+1})

stateEndScope :: ParseM ()
stateEndScope = do
  topScope <- stateScopeStackTop
  --liftIO $ putStrLn $ "Exit Scope: " ++ show topScope
  stateScopeStackPop
  stateScopesDelete topScope

-- Find the first symbol scope of the list such that the scope is active
stateFindSymScope :: Id -> Pos -> ParseM Scope
stateFindSymScope id pos = (sym_scope <$> stateFindSym id pos)
                      `catchError` catchIdNotFound


stateFindSym :: Id -> Pos -> ParseM Sym
stateFindSym id pos = do
  maybeList <- (symTableLoopUp id) <$> (gets state_SymTable)
  case maybeList of
       Nothing -> throwError (IdNotFound id pos)
       Just l -> do
          scopes <- gets state_ScopeSet
          case find (scopeIsActive scopes) l of
               Nothing -> throwError (IdNotFound id pos)
               Just sym -> return sym
  where
    scopeIsActive :: ScopeSet -> Sym -> Bool
    scopeIsActive ss sym = scopeSetMember (sym_scope sym) ss


stateInsertSym :: Sym -> ParseM ()
stateInsertSym sym = case sym_type sym of
                          OKFunc _ _ -> insertFunctionSym sym
                          _ -> insertNonFunctionSym sym

insertNonFunctionSym :: Sym -> ParseM ()
insertNonFunctionSym sym = do
  prevScope <- (sym_scope <$> stateFindSym (sym_Id sym) (0,0))
                `catchError` (\_ -> return (-1))
  case prevScope == (sym_scope sym) of
       True -> catchAlreadyDefinedInScope (AlreadyDefinedInScope sym)
       False -> do
          symTable <- gets state_SymTable
          let newSymTable = symTableInsert sym symTable
          modify (\s -> s{state_SymTable = newSymTable})



insertFunctionSym :: Sym -> ParseM ()
insertFunctionSym sym@(Sym scp id _ (OKFunc prms ret) []) = do
  maybeList <- (symTableLoopUp id) <$> (gets state_SymTable)
  symTable <- gets state_SymTable
  case maybeList of
       Nothing -> do
          let newSymTable = symTableInsert sym symTable
          modify (\s -> s{state_SymTable = newSymTable})
       Just l -> do
          case find (sameParams prms) l of
               Just x -> catchAlreadyDefinedInScope (AlreadyDefinedInScope sym)
               Nothing -> do
                  let newSymTable = symTableInsert sym symTable
                  modify (\s -> s{state_SymTable = newSymTable})
  where
    sameParms :: [OKType] -> Sym -> Bool
    sameParams l (Sym _ _ _ (OKFunc prms _) []) = l==prms
    sameParms _ _ = False

