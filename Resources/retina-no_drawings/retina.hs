-- Retina - Proyecto de Traductores
-- Programa Principal
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

import Lexer
import AST
import RunAST
import RunMonad
import qualified Parser as P
import ContextChecker
import OurContextMonad
import System.Environment

import Control.Monad

import System.IO


main = do
    hSetBuffering stdout NoBuffering
    programFile <- getArgs >>= (return.head)
    let programName = extractName programFile
    when (length programName < 5) $ error "El archivo debe ser un archivo .rtn"
    let ext = reverse.take 4.reverse $ programName
    when (ext/=".rtn") $ error "El archivo debe ser un archivo .rtn"
    s <- readFile programFile
    let res = runAlexScan s
    case res of
        Right ls -> if invalidTokens ls then do
                        putStrLn "Error lexicografico (alex isn't happy)"
                        mapM_ printToken $ reverse $ tokenList ls
                    else do
                        --printConstrN 0 . (P.parse) . reverse . tokenList $ ls
                        let ast = (P.parse) . reverse . tokenList $ ls
                        case getContextError (checkConstrN ast) emptyContextState of
                            Nothing -> do
                                result <- runRunMonad (runConstrN ast) ourEmptyState
                                mapM_ (putStrLn . show) (snd result)
                            Just e -> error e
        Left e -> putStrLn e
    where
        extractName :: String -> String
        extractName s = reverse $ takeWhile ((/=) '/') $ reverse s
