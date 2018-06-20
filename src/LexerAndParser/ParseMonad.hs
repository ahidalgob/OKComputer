module ParseMonad where
import Tokens
import LowLevelAlex

import AST
import SymTable
import OKTypes
import Scope

import Control.Monad.State.Lazy
import Control.Monad.Except

import Data.List(find)

{-
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 17 "/usr/include/stdc-predef.h" 3 4
{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/home/hp/haskell-platform/build/ghc-bindist/local/lib/ghc-8.2.2/include/ghcversion.h" #-}
{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc9791_0/ghc_2.h" #-}
{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
-}
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
                   IdNotInScope Id Pos |
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
----------------------- Alex
--------------------------------------------------------
-- {{{

getAlexInput :: ParseM AlexInput
getAlexInput = gets alex_inp

setAlexInput :: AlexInput -> ParseM ()
setAlexInput inp = modify (\s -> s{alex_inp=inp})

getAlexStartCode :: ParseM Int
getAlexStartCode = gets alex_scd

setAlexStartCode:: Int -> ParseM ()
setAlexStartCode sc = modify (\s -> s{alex_scd=sc})

-- {{{2
--
--
-- 22}}}

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

--}}}
--------------------------------------------------------


-----------------------------------------------
----------------------- SYM TABLE
-----------------------------------------------
-- {{{1

---------------------- ScopeStack
--{{{2
topScope :: ParseM Scope
topScope = head <$> (gets state_ScopeStack)

popScope :: ParseM ()
popScope = modify
      (\s -> s{state_ScopeStack = tail (state_ScopeStack s)})

pushScope :: Scope -> ParseM ()
pushScope sc = modify
      (\s -> s{state_ScopeStack = sc:(state_ScopeStack s)})
--}}}

------------------- ScopeSet
-- {{{2
scopesMember :: Scope -> ParseM Bool
scopesMember sc = (scopeSetMember sc) <$> (gets state_ScopeSet)

insertScope :: Scope -> ParseM ()
insertScope sc = do
  set <- gets state_ScopeSet
  modify (\s -> s{state_ScopeSet = scopeSetInsert sc set})

deleteScope :: Scope -> ParseM ()
deleteScope sc = do
  set <- gets state_ScopeSet
  modify (\s -> s{state_ScopeSet = scopeSetDelete sc set})

--}}}

beginScope :: ParseM ()
beginScope = do
  nextScope <- gets state_NextScope
  --liftIO $ putStrLn $ "Enter Scope: " ++ show nextScope
  pushScope nextScope
  insertScope nextScope
  modify (\s -> s{state_NextScope = nextScope+1})

endScope :: ParseM ()
endScope = do
  topScope <- topScope
  --liftIO $ putStrLn $ "Exit Scope: " ++ show topScope
  popScope
  deleteScope topScope



-- Find the first symbol scope of the list such that the scope is active
findSymScope :: Id -> Pos -> ParseM Scope
findSymScope id pos = (sym_scope <$> findSym id pos)
                      `catchError` catchIdNotFound


-- Finds all the symbols associated with an Id
findAllSyms :: Id -> Pos -> ParseM [Sym]
findAllSyms id pos = do
  maybeList <- (symTableLookUp id) <$> (gets state_SymTable)
  case maybeList of
       Nothing -> throwError (IdNotFound id pos)
       Just syms -> return syms

-- Finds the first symbol on the list of the give Id such that its scope is active
findSym :: Id -> Pos -> ParseM Sym
findSym id pos = do
  l <- findAllSyms id pos `catchError` (\_ -> return [])
  activeScopes <- gets state_ScopeSet
  case find (scopeIsIn activeScopes) l of
       Nothing -> throwError (IdNotInScope id pos)
       Just sym -> return sym
  where
    scopeIsIn :: ScopeSet -> Sym -> Bool
    scopeIsIn ss sym = scopeSetMember (sym_scope sym) ss

--
insertSym :: Sym -> ParseM ()
insertSym sym = case sym_type sym of
                          OKFunc _ _ -> insertFunctionSym sym
                          _ -> insertNonFunctionSym sym

insertNonFunctionSym :: Sym -> ParseM ()
insertNonFunctionSym sym = do
  prevScope <- (sym_scope <$> findSym (sym_Id sym) (0,0))
                `catchError` (\_ -> return (-1))
  case prevScope == (sym_scope sym) of
       True -> catchAlreadyDefinedInScope (AlreadyDefinedInScope sym)
       False -> do
          symTable <- gets state_SymTable
          let newSymTable = symTableInsert sym symTable
          modify (\s -> s{state_SymTable = newSymTable})



insertFunctionSym :: Sym -> ParseM ()
insertFunctionSym sym@(FuncSym scp id _ (OKFunc prms ret) argsId instrs) = do
  maybeList <- (symTableLookUp id) <$> (gets state_SymTable)
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
    sameParams l (FuncSym _ _ _ (OKFunc prms _) _ _) = l==prms
    sameParms _ _ = False



-- TODO
completeFunctionDef :: Token -> OKType -> [INSTRUCTIONN] -> ParseM ()
completeFunctionDef = undefined

--}}}
