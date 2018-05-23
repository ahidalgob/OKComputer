module ParseMonad where
import Tokens
import LowLevelAlex
import SymTable

import Control.Monad.State.Lazy
import Control.Monad.Except

# 1 "/usr/include/stdc-predef.h" 1 3 4
# 17 "/usr/include/stdc-predef.h" 3 4
{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/home/hp/haskell-platform/build/ghc-bindist/local/lib/ghc-8.2.2/include/ghcversion.h" #-}
{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc9791_0/ghc_2.h" #-}
{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- Compile with -funbox-strict-fields for best results!

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
                              state_SymTable = emptySymTable}

type ParseMError = String

type ParseM a = ExceptT ParseMError (StateT ParseState IO) a


runParseM :: ParseM a -> String -> IO(Either String a, ParseState)
runParseM f s = runStateT (runExceptT f) (initParseState s)


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






stateScopesTop :: ParseM Scope
stateScopesTop = head <$> (gets state_ScopeStack)

stateScopesPop :: ParseM ()
stateScopesPop = modify
      (\s -> s{state_ScopeStack = tail (state_ScopeStack s)})

stateScopesPush :: Scope -> ParseM ()
stateScopesPush sc = modify
      (\s -> s{state_ScopeStack = sc:(state_ScopeStack s)})

stateScopesMember :: Scope -> ParseM Bool
stateScopesMember sc = (scopeSetMember sc) <$> (gets state_ScopeSet)

-- Find the first symbol of the list such that the scope is active
-- msum!
stateFindSym :: Id -> ParseM (Maybe SymId)
stateFindSym = undefined

-- If there's no symbol with the same name, insert it as a unitary list
-- otherwise insert it at the head of the list
stateSymInsert :: Sym -> ParseM ()
stateSymInsert sym = undefined


