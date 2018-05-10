module ParseMonad where
import Tokens
import LowLevelAlex




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


import Control.Applicative as App (Applicative (..))
import qualified Control.Monad (ap)

import Data.Char (ord)





-- -----------------------------------------------------------------------------
-- Default monad


data ParseState = ParseState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
        alex_scd :: !Int        -- the current startcode

      , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program

    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Alex a -> Either String a
runAlex input__ (Alex f)
   = case f (ParseState {alex_pos = alexStartPos,
                        alex_inp = input__,
                        alex_chr = '\n',
                        alex_bytes = [],

                        alex_ust = alexInitUserState,

                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: ParseState -> Either String (ParseState, a) }

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
                            Left msg -> Left msg
                            Right (s', a') -> Right (s', f a')

instance Applicative Alex where
  pure a   = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unAlex a s' of
                                               Left msg -> Left msg
                                               Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return = App.pure

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@ParseState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} ->
        Right (s, (pos,c,bs,inp__))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,bs,inp__)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} of
                  state__@(ParseState{}) -> Right (state__, ())

alexError :: String -> Alex a
alexError message = Alex $ const $ Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@ParseState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())


alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@ParseState{alex_ust=ust} -> Right (s,ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ss = Alex $ \s -> Right (s{alex_ust=ss}, ())











alexEOF :: Alex [Token]
alexEOF = return []


--------- User State
data AlexUserState = AlexUserState {
                        invalidC :: [(Char, Pos)],
                        strPos :: Pos,
                        str :: String
                        } deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState{invalidC=[], strPos=(0,0), str=""}


pushStrC :: Char -> Alex ()
pushStrC c = do
  ust <- alexGetUserState
  alexSetUserState ust{str = c:str ust}

getAndClearStr :: Alex String
getAndClearStr = do
  ust <- alexGetUserState
  let s = str ust
  alexSetUserState ust{str=""}
  return $ reverse s

setStrPos :: Pos -> Alex ()
setStrPos pos = do
  ust <- alexGetUserState
  alexSetUserState ust{strPos = pos}

getStrPos :: Alex Pos
getStrPos = do
  ust <- alexGetUserState
  return $ strPos ust



pushInvalidC :: Char -> Pos -> Alex ()
pushInvalidC c pos = do
  ust <- alexGetUserState
  alexSetUserState ust{invalidC = (c,pos):invalidC ust}



getPos :: AlexInput -> Pos
getPos ((AlexPn _ line col), _, _, _) = (line, col)

getCurrentInput :: AlexInput -> String
getCurrentInput (_, _, _, s) = s





