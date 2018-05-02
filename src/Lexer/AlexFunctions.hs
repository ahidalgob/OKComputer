{-
  This file is just a concise compilation of the types and functions we're going to be using with Alex.
-}

data AlexPosn = AlexPn !Int  -- absolute character offset
                       !Int  -- line number
                       !Int  -- column number


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- rest of the bytes for the current char
                  String)       -- current input string

-- Note that AlexState refers to an actual state and not to a
-- "stateful computation" as in a State Monad.
-- The same goes for AlexUserState, which has to be defined by us.

data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],   -- rest of the bytes for the current char
        alex_scd :: !Int,       -- the current startcode
        alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
    }

StateT s m a = State (s -> m (a, s))
push :: Int -> StateT Stack Maybe ()
pop :: StateT Stack Maybe Int

runStateT manejadorStack [] :: Maybe ((), Stack)


-- This is basically a StateT AlexState (Either String) a,
-- the order of the (state, result) pair is backwards, though
newtype Alex a = Alex { unAlex :: AlexState
                               -> Either String (AlexState, a) }


-- This is the main function if we want to make a full lexer with alex
-- We should make actions  AlexAction [token] and call
-- runAlex input (alexMonadScan)
runAlex          :: String -> Alex a -> Either String a

-- If we want to integrate alex and happy, alexMonadScan should
-- have type Alex token (?)

alexGetInput     :: Alex AlexInput
alexSetInput     :: AlexInput -> Alex ()

alexError        :: String -> Alex a

alexGetStartCode :: Alex Int
alexSetStartCode :: Int -> Alex ()




alexMonadScan :: Alex result


-- The token actions should have the following type:
type AlexAction result = AlexInput -> Int -> Alex result
{ ... }  :: AlexAction result

  do
    take len input
    resto <- alexMonadScan
    return token:resto

