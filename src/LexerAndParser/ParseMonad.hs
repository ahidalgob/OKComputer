module ParseMonad(
                   ParseM
                 , ParseMError(..)    -- TODO maybe shouldn't be here IDK

                  -- General
                 , runParseM
                 , ParseState(..)

                  -- Lexer
                 , setAlexInput
                 , getAlexInput
                 , setLastNewLine
                 , getLastNewLine
                 , setAlexStartCode
                 , getAlexStartCode
                 , setStrPos
                 , getStrPos
                 , getAndClearStr
                 , pushStrC
                 , pushInvalidC

                  -- Parser
                 , beginScope
                 , begin0Scope
                 , endScope
                 , topScope

                 , setReturnType
                 , getReturnType


                 , insertVarSym
                 , insertNameTypeSym
                 , insertFuncSym
                 , completeFunctionDef
                 , findFunction
                 , findVarSym
                 , findNameTypeSym
                 , findSymInRecord



                 )where
--import Tokens
import LowLevelAlex

import qualified AST(INSTRUCTION)
import OKTypes
import SymTable
import Scope

import qualified Data.Map.Strict as Map

import Data.Maybe

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

--import Data.Char (ord)


data ParseState = ParseState {
        alex_inp :: AlexInput,     -- the current input
        alex_scd :: !Int,          -- the current startcode

        alex_invalidC :: [(Char, Pos)], -- list of invalid characters
        alex_strPos :: Pos,             -- where string begins in string mode
        alex_str :: String,             -- the string in string mode

        last_new_line :: Bool,          -- whether the last token was a newline

        state_ScopeStack :: ScopeStack,       -- Stack of scopes
        state_ScopeSet :: ScopeSet,           -- Set of scopes
        state_NextScope :: Scope,               -- Next scope
        state_SymTable :: SymTable,           -- Sym table

        state_scparent :: Map.Map Scope Scope ,
        state_scwidth :: Map.Map Scope Int,
        state_offset :: Map.Map (Id, Int) Int,

        state_functionLabelNumber :: Int,

        state_returnType :: OKType            -- Return type when inside function
} deriving Show


initParseState :: String -> ParseState
initParseState s = ParseState{ alex_inp = (alexStartPos, '\n', [], s)
                             , alex_scd = 0
                             , alex_invalidC = []
                             , alex_strPos = (0,0)
                             , alex_str = ""
                             , last_new_line = True

                             , state_ScopeStack = emptyScopeStack
                             , state_ScopeSet = emptyScopeSet
                             , state_NextScope = 2
                             , state_SymTable = emptySymTable

                             , state_scparent = Map.singleton 1 0
                             , state_scwidth = Map.insert 1 0 $ Map.singleton 0 0
                             , state_offset = Map.empty

                             , state_functionLabelNumber = 0

                             , state_returnType = OKVoid
                             }

--------------------------------------------------------
----------------- Error --------------------------------
--------------------------------------------------------

-- TODO please don't read this
data ParseMError = ParseError String
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



-- exported
setReturnType :: OKType -> ParseM ()
setReturnType oktype = modify (\s -> s{state_returnType = oktype})

-- exported
getReturnType :: ParseM OKType
getReturnType = gets state_returnType

getNewFunctionLabelNumber :: ParseM String
getNewFunctionLabelNumber = do
  old <- gets state_functionLabelNumber
  modify (\s -> s{state_functionLabelNumber = old+1})
  return $ show (old+1)
-----------------------------------------------
----------------------- SYM TABLE
-----------------------------------------------
-- {{{1

---------------------- ScopeStack
--{{{2
-- exported
topScope :: ParseM Scope
topScope = head <$> gets state_ScopeStack

popScope :: ParseM ()
popScope = modify
      (\s -> s{state_ScopeStack = tail (state_ScopeStack s)})

pushScope :: Scope -> ParseM ()
pushScope sc = modify
      (\s -> s{state_ScopeStack = sc : state_ScopeStack s})
--}}}

------------------- ScopeSet
-- {{{2
--scopesMember :: Scope -> ParseM Bool
--scopesMember sc = scopeSetMember sc <$> gets state_ScopeSet

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
-- exported
beginScope :: ParseM Scope
beginScope = do
  nextScope <- gets state_NextScope
  topScope <- topScope
  new_scpar <- Map.insert nextScope topScope <$> gets state_scparent
  new_scwidth <- Map.insert nextScope 0 <$> gets state_scwidth
  pushScope nextScope
  insertScope nextScope
  modify (\s -> s{state_NextScope = nextScope+1, state_scparent = new_scpar, state_scwidth = new_scwidth})
  return nextScope

begin0Scope :: ParseM Scope
begin0Scope = do
  scope <- beginScope
  old_scpar <- gets state_scparent
  let new_scpar = Map.insert scope 0 old_scpar
  modify (\s -> s{state_scparent = new_scpar})
  return scope

-- exported
endScope :: ParseM ()
endScope = do
  topScope <- topScope
  popScope
  deleteScope topScope


-- Finds all the symbols associated with an Id
findAllSyms :: Id -> ParseM [Sym]
findAllSyms id = do
  maybeList <- symTableLookUp id <$> gets state_SymTable
  case maybeList of
       Nothing -> return []
       Just syms -> return syms

findAllSymsInActiveScopes :: Id -> ParseM [Sym]
findAllSymsInActiveScopes id = do
  l <- findAllSyms id
  activeScopes <- gets state_ScopeSet
  return $ filter (scopeIsIn activeScopes) l
  where
    scopeIsIn :: ScopeSet -> Sym -> Bool
    scopeIsIn ss sym = scopeSetMember (sym_scope sym) ss


findFirstSymInActiveScopes :: Id -> ParseM (Maybe Sym)
findFirstSymInActiveScopes id = do
  l <- findAllSymsInActiveScopes id
  if null l
     then return Nothing
     else return.Just $ head l -- TODO why not error


findSymInScope :: Scope -> Id -> ParseM (Maybe Sym)
findSymInScope scope id = do
  activeScopes <- gets state_ScopeSet
  modify (\s -> s{state_ScopeSet = scopeSetInsert scope emptyScopeSet})
  sym <- findFirstSymInActiveScopes id
  modify (\s -> s{state_ScopeSet = activeScopes})
  return sym

findAllSymsInScope :: Scope -> Id -> ParseM [Sym]
findAllSymsInScope scope id = do
  activeScopes <- gets state_ScopeSet
  modify (\s -> s{state_ScopeSet = scopeSetInsert scope emptyScopeSet})
  syms <- findAllSymsInActiveScopes id
  modify (\s -> s{state_ScopeSet = activeScopes})
  return syms


-- finds the symbol in the closest scope, it has to be a varSym
-- throws an error if not found or it's not a variable
findVarSym :: Id -> Pos -> ParseM Sym
findVarSym id pos = do
  sym <- findFirstSymInActiveScopes id
  case sym of
    Nothing -> showVariableNotFound id (fst pos) >> return (ErrorSym (-1) id pos OKErrorT) -- not defined
    Just s@FuncSym{} -> showVariableAlreadyUsedAsFunction id (fst pos) s >> return (ErrorSym (-1) id pos OKErrorT)-- it's a function
    --Just NameTypeSym{} -> undefined -- it's a type
    Just s@ErrorSym{} -> return s-- it was defined but with an error so don't show an error
    Just s@VarSym{} -> return s

findNameTypeSym :: Id -> Pos -> ParseM Sym
findNameTypeSym id pos = do
  sym <- findFirstSymInActiveScopes id
  case sym of
    Nothing -> showNameTypeNotFound id (fst pos) >> return (ErrorSym (-1) id pos OKErrorT) -- not defined
    --Just FuncSym{} -> undefined -- it's a function
    --Just VarSym{} -> undefined -- it's a variable
    Just s@ErrorSym{} -> return s -- defined with error, don't show error
    Just s@NameTypeSym{} -> return s


findSymInRecord :: Scope -> String -> Pos -> ParseM Sym
findSymInRecord scope id pos = do
  sym <- findSymInScope scope id
  case sym of
    Nothing -> do showMemberNotFound id (fst pos)  >> return (ErrorSym (-1) id pos OKErrorT)
    --Just FuncSym{} -> undefined -- it's a function
    --Just NameTypeSym{} -> undefined -- it's a type
    Just s@ErrorSym{} -> return s -- it was defined but with an error so don't show an error
    Just s@VarSym{} -> return s

insertVarSym :: Scope -> Id -> Pos -> OKType -> ParseM ()
insertVarSym scope id pos oktype = do
  -- the error is just a dummy variable
  prevSym <- fromMaybe (ErrorSym (-1) id pos OKErrorT) <$> findSymInScope scope id
  if sym_scope prevSym == scope
       then showVariableRedeclaredInScope id (fst pos) prevSym
       else  do symTable <- gets state_SymTable
                let newSymTable = symTableInsert (VarSym scope id pos oktype) symTable

                Just varOffset <- Map.lookup scope <$> gets state_scwidth
                new_offset <- Map.insert (id, scope) varOffset <$> gets state_offset

                new_scwidth <- Map.adjust (+ type_width oktype) scope <$> gets state_scwidth

                modify (\s -> s{state_SymTable = newSymTable, state_offset = new_offset, state_scwidth = new_scwidth})


modifySymTableList :: Id -> [Sym] -> ParseM()
modifySymTableList id newList = do
  symTable <- gets state_SymTable
  let newSymTable = symTableModify id newList symTable
  modify (\s -> s{state_SymTable = newSymTable})


insertFuncSym :: Id -> Pos -> OKType -> [SymId] -> Bool -> ParseM ()
insertFuncSym id pos oktype@(OKFunc paramTypes _) paramIds defining = do
  syms <- findAllSymsInScope 1 id
  if not.null $ filter isVarSym syms
     then showFunctionNameUsedAsGlobalVariable id (fst pos) (head syms) -- already defined as variable
     else case find (sameParams paramTypes) syms of
          Nothing -> do
              symTable <- gets state_SymTable
              labelid <- getNewFunctionLabelNumber
              let newSymTable = symTableInsert (FuncSym 1 id pos oktype paramIds (0,[]) False (labelid++id)) symTable
              modify (\s -> s{state_SymTable = newSymTable})
          Just FuncSym{sym_defined=def}
            | def -> showRedeclarationOfDefinedFunction id (fst pos) (head $ filter (sameParams paramTypes) syms) -- already defined with same arguments
            | not defining -> showRedeclarationOfDeclaredFunction id (fst pos) (head $ filter (sameParams paramTypes) syms) -- already defined with same arguments
            | otherwise -> do
                newList <- updateFunc oktype paramIds <$> findAllSyms id
                modifySymTableList id newList
                where
                      updateFunc :: OKType -> [SymId] -> [Sym] -> [Sym]
                      updateFunc _ _ [] = []
                      updateFunc funcType ids (sym : syms) =
                        (if funcType == sym_type sym
                            then sym{sym_argsId = ids}
                            else sym
                            ) : updateFunc funcType ids syms



-- Checks if the name is already defined (as anything else)
insertNameTypeSym :: Id -> Pos -> OKType -> ParseM ()
insertNameTypeSym id pos oktype = do
  prevSym <- findFirstSymInActiveScopes id
  case prevSym of
    Nothing -> do
          symTable <- gets state_SymTable
          let newSymTable = symTableInsert (NameTypeSym 0 id pos oktype) symTable
          modify (\s -> s{state_SymTable = newSymTable})
    Just sym -> showNameTypeAlreadyUsed (sym_Id sym) (fst pos) sym-- alias already defined


sameParams :: [OKType] -> Sym -> Bool
sameParams prms1 FuncSym{sym_type = OKFunc prms2 _ } = prms1==prms2
sameParams _ _ = False

-- exported
findFunction :: Id -> Pos -> [OKType] -> ParseM Sym
findFunction id pos paramTypes = do
  current <- findFirstSymInActiveScopes id
  case current of
    Nothing -> showFunctionNotFound id (fst pos)  >> return (ErrorSym (-1) id pos OKErrorT)-- not defined
    Just s@VarSym{} -> showFunctionAlreadyUsedAsVariable id (fst pos) s  >> return (ErrorSym (-1) id pos OKErrorT) -- defined as variable
    Just FuncSym{} -> do
        syms <- findAllSymsInActiveScopes id
        case findMatchingFunction paramTypes syms of
             Nothing ->  showFunctionNotFoundWithArguments id (fst pos)  >> return (ErrorSym (-1) id pos OKErrorT)-- not found with those arguments
             Just sym -> return sym
  where
    findMatchingFunction :: [OKType] -> [Sym] -> Maybe Sym
    findMatchingFunction params = find (sameParams params)



-- exported
completeFunctionDef :: Id -> OKType -> (Scope, [AST.INSTRUCTION]) -> ParseM ()
completeFunctionDef id oktype instrs = do
  newList <- updateFunc oktype instrs <$> findAllSyms id
  modifySymTableList id newList
  where
        updateFunc :: OKType -> (Scope, [AST.INSTRUCTION]) -> [Sym] -> [Sym]
        updateFunc _ _ [] = []
        updateFunc funcType instrs (sym : syms) =
          (if funcType == sym_type sym
              then sym{sym_AST = instrs, sym_defined = True}
              else sym
              ) : updateFunc funcType instrs syms







--Errors{{{1

showNameTypeAlreadyUsed :: Id -> Int -> Sym -> ParseM ()
showNameTypeAlreadyUsed id ln sym = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Name for typedef " ++ id ++ " is already used in line " ++ show (fst.sym_pos $ sym) ++ "."
    liftIO $ putStrLn $ "You're not to blame for bittersweet distractors \n"

showFunctionNameUsedAsGlobalVariable :: Id -> Int -> Sym -> ParseM ()
showFunctionNameUsedAsGlobalVariable id ln sym = do
     liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
     liftIO $ putStrLn $ "Name for function " ++ id ++ " is already used as a global variable in line " ++ show (fst.sym_pos $ sym) ++ "."
     liftIO $ putStrLn $ "You're not to blame for bittersweet distractors \n"

showRedeclarationOfDefinedFunction :: Id -> Int -> Sym -> ParseM ()
showRedeclarationOfDefinedFunction id ln sym = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Function " ++ id ++ " with same signature (arguments type) already defined in line " ++ show (fst.sym_pos $ sym) ++ "."
    liftIO $ putStrLn $ "For a minute there I lost myself \n"

showRedeclarationOfDeclaredFunction :: Id -> Int -> Sym -> ParseM ()
showRedeclarationOfDeclaredFunction id ln sym = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Function " ++ id ++ " with same signature (arguments type) already declared in line " ++ show (fst.sym_pos $ sym) ++ "."
    liftIO $ putStrLn $ "For a minute there I lost myself \n"

showVariableRedeclaredInScope :: Id -> Int -> Sym -> ParseM ()
showVariableRedeclaredInScope id ln sym = do
     liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
     liftIO $ putStrLn $ "Variable " ++ id ++ " is already defined in same scope in line " ++ show (fst.sym_pos $ sym) ++ "."
     liftIO $ putStrLn $ "For a minute there I lost myself \n"

showVariableNotFound :: Id -> Int -> ParseM ()
showVariableNotFound id ln = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Variable " ++ id ++ " was not found."
    liftIO $ putStrLn $ "For a minute there I lost myself \n"

showNameTypeNotFound :: Id -> Int -> ParseM ()
showNameTypeNotFound id ln = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Nametype " ++ id ++ " was not found."
    liftIO $ putStrLn $ "You’ve been stuck in a lift, we’ve been trying to reach you \n"

showVariableAlreadyUsedAsFunction :: Id -> Int -> Sym -> ParseM ()
showVariableAlreadyUsedAsFunction id ln sym = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Variable " ++ id ++ " is already used as a function " ++ show (fst.sym_pos $ sym) ++ "."
    liftIO $ putStrLn $ "You’ve been stuck in a lift, we’ve been trying to reach you \n"

showMemberNotFound :: Id -> Int -> ParseM ()
showMemberNotFound id ln = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ id ++ " is not a member of the record."
    liftIO $ putStrLn $ "I have seen too much, I haven't seen enough\n"

showFunctionNotFound :: Id -> Int -> ParseM ()
showFunctionNotFound id ln = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Function " ++ id ++ " was not found."
    liftIO $ putStrLn $ "I have seen too much, I haven't seen enough\n"

showFunctionAlreadyUsedAsVariable :: Id -> Int -> Sym -> ParseM ()
showFunctionAlreadyUsedAsVariable id ln sym = do
     liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
     liftIO $ putStrLn $ "Function " ++ id ++ " is already used as a variable in line " ++ show (fst.sym_pos $ sym) ++ "."
     liftIO $ putStrLn $ "You’ve been stuck in a lift, we’ve been trying to reach you \n"

showFunctionNotFoundWithArguments :: Id -> Int -> ParseM ()
showFunctionNotFoundWithArguments id ln = do
    liftIO $ putStrLn $ "Error in line " ++ show ln ++ ":"
    liftIO $ putStrLn $ "Function " ++ id ++ " was not found with same arguments."
    liftIO $ putStrLn $ "I have seen too much, I haven't seen enough\n"


--}}
