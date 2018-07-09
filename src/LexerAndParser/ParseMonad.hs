module ParseMonad where
import Tokens
import LowLevelAlex

import qualified AST
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
        state_SymTable :: SymTable,

        state_returnType :: OKType
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
                              state_NextScope = 2,
                              state_SymTable = emptySymTable,

                              state_returnType = OKVoid}

--------------------------------------------------------
----------------- Error --------------------------------
--------------------------------------------------------

data ParseMError = IdNotFound Id Pos |
                   IdNotInScope Id Pos |
                   AlreadyDefinedInScope Sym |
                   ParseError String |
                   VarWithFunctionName Sym |
                   NameIsUsedForType Id Pos |
                   NameTypeAlreadyUsed Id Pos |
                   FunctionNotDefined |
                   IsNotType String Int |
                   VariableInScopeIsNotFunction
                 deriving Show

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

-- TODO catch all errors
beginScope :: ParseM Scope
beginScope = do
  nextScope <- gets state_NextScope
  --liftIO $ putStrLn $ "Enter Scope: " ++ show nextScope
  pushScope nextScope
  insertScope nextScope
  modify (\s -> s{state_NextScope = nextScope+1})
  return nextScope

endScope :: ParseM ()
endScope = do
  topScope <- topScope
  --liftIO $ putStrLn $ "Exit Scope: " ++ show topScope
  popScope
  deleteScope topScope


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
  l <- findAllSyms id pos
  activeScopes <- gets state_ScopeSet
  case find (scopeIsIn activeScopes) l of
       Nothing -> throwError (IdNotInScope id pos)
       Just sym -> return sym
  where
    scopeIsIn :: ScopeSet -> Sym -> Bool
    scopeIsIn ss sym = scopeSetMember (sym_scope sym) ss

findSymInScope :: Scope -> Token -> ParseM Sym
findSymInScope scope idTkn = do
  l <- filter (\s -> sym_scope s == scope) <$> findAllSyms (tkn_string idTkn) (tkn_pos idTkn)
  if null l then error "NO ME USES MAL! TE ODIO" -- TODO
            else return $ head l


insertSym :: Sym -> ParseM ()
insertSym sym@(VarSym _ _ _ _) = insertVarSym sym
insertSym sym@(FuncSym _ _ _ _ _ _) = insertFunctionSym sym
insertSym sym@(NameTypeSym _ _ _ _) = insertNameTypeSym sym
insertSym sym@(ErrorSym _ _ _ _) = liftIO $ putStrLn "Trying to add an ErrorSym to SymTable. What ya trying?"

checkDefinedNameTypeSym :: Id -> Pos -> ParseM ()
checkDefinedNameTypeSym id pos = do
  syms <- findAllSyms id pos `catchError` (\_ -> return [])
  case find isNameTypeSym syms of
       Nothing -> return ()
       _ -> throwError $ NameIsUsedForType id pos

insertVarSym :: Sym -> ParseM ()
insertVarSym sym = do
  checkDefinedNameTypeSym (sym_Id sym) (sym_pos sym)
  prevScope <- (sym_scope <$> findSym (sym_Id sym) (sym_pos sym))
                `catchError` (\_ -> return (-1))
  case prevScope == (sym_scope sym) of
       True -> throwError $ AlreadyDefinedInScope sym
       False -> do
          symTable <- gets state_SymTable
          let newSymTable = symTableInsert sym symTable
          modify (\s -> s{state_SymTable = newSymTable})


insertFunctionSym :: Sym -> ParseM ()
insertFunctionSym sym@(FuncSym scp id pos (OKFunc prms ret) argsId instrs) = do
  checkDefinedNameTypeSym (sym_Id sym) (sym_pos sym)
  list <- (findAllSyms id pos) `catchError` (\_ -> return [])
  symTable <- gets state_SymTable

  let filterList = filter (\sym -> (sym_scope sym == 1) && isVarSym sym) list
  when (not (null filterList)) $ throwError (VarWithFunctionName sym)

  case find (sameParams (func_ParamTypes.sym_type $ sym)) list of
       Just x -> throwError (AlreadyDefinedInScope sym)
       Nothing -> do
          let newSymTable = symTableInsert sym symTable
          modify (\s -> s{state_SymTable = newSymTable})

insertNameTypeSym :: Sym -> ParseM ()
insertNameTypeSym sym = do
  syms <- findAllSyms (sym_Id sym) (sym_pos sym) `catchError` (\_ -> return [])
  symTable <- gets state_SymTable
  case null syms of
       False -> throwError $ NameTypeAlreadyUsed (sym_Id sym) (sym_pos sym)
       True -> do
          let newSymTable = symTableInsert sym symTable
          modify (\s -> s{state_SymTable = newSymTable})

sameParams :: [OKType] -> Sym -> Bool
sameParams prms1 (FuncSym{sym_type = OKFunc prms2 _ }) = prms1==prms2
sameParams _ _ = False

findFunction :: Id -> Pos -> [OKType] -> ParseM Sym
findFunction id pos paramTypes = do
  syms <- findAllSyms id pos `catchError` (\_ -> return [])
  case null syms of
   True -> throwError FunctionNotDefined -- TODO more information
   False -> do
      current <- findSym id pos
      case current of
        FuncSym{} -> do
          case findMatchingFunction paramTypes syms of
               Nothing -> throwError FunctionNotDefined
               Just sym -> return sym
        _ -> throwError VariableInScopeIsNotFunction -- TODO more information
  where
    findMatchingFunction :: [OKType] -> [Sym] -> Maybe Sym
    findMatchingFunction params syms = find (sameParams params) syms


completeFunctionDef :: Sym -> ParseM ()
completeFunctionDef sym = do
    newList <- updateSym sym <$> findAllSyms (sym_Id sym) (sym_pos sym)
    symTable <- gets state_SymTable
    let newSymTable = symTableModify (sym_Id sym) newList symTable
    modify (\s -> s{state_SymTable = newSymTable})
  where
        updateSym :: Sym -> [Sym] -> [Sym]
        updateSym _ [] = []
        updateSym s (s' : ss) = let ns = if sameParams ( func_ParamTypes.sym_type $ s) s' then s else s'
                                in ns : updateSym s ss

--}}}


setReturnType :: OKType -> ParseM ()
setReturnType oktype = modify (\s -> s{state_returnType = oktype})

getReturnType :: ParseM OKType
getReturnType = gets state_returnType
