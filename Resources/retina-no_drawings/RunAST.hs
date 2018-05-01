
module RunAST where

import RunMonad
import OurType
import AST
import Control.Monad
import Control.Applicative
import Data.Maybe (fromJust)
import Data.Fixed
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)

----------------------------------------------------------
-- runConstrN ------------------------------------------
----------------------------------------------------------
runConstrN :: ConstrN -> RunMonad ()
runConstrN (PN ldfn iln) = do --Construccion de Programa
    runConstrN ldfn
    _ <- runInstrListN iln
    return ()

runConstrN (LDFN l) = do --Construccion de Lista de Definicion de Funciones
    mapM_ runFuncDefN l

runConstrN (LDN l) = do --Construccion de Lista de Declaracion de variables
    mapM_ runTypeAndList l
    where 
        runTypeAndList :: (TypeN, [VarN]) -> RunMonad ()
        runTypeAndList (tN, varNList) = do
            let t = typeNConvert tN
            mapM_ (adder t) varNList
        adder :: OurType -> VarN -> RunMonad ()
        adder t varN = do
            let (s, me) = case varN of
                    (VarN st) -> (st, Nothing)
                    (VarValN st e) -> (st, Just e)
            case me of
                Just e -> do
                    val <- runExpN e
                    addToSymTable (s,val,True,t)
                Nothing -> do
                    let val = case t of
                            Boolean -> BooleanVal False
                            Number -> NumberVal 0
                    addToSymTable (s,val,True,t)

----------------------------------------------------------
-- runFuncDefN -----------------------------------------
----------------------------------------------------------
runFuncDefN :: FuncDefN -> RunMonad ()
runFuncDefN funcDefN = do
    let (funId, paramList, instrListN, retType) = case funcDefN of
            DFN s p i _ -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i, Void)
            RDFN s p ret i _ -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i, typeNConvert ret)

    addToFunDec (funId, map fst paramList, listLIN instrListN, retType)


----------------------------------------------------------
-- runInstrListN ---------------------------------------
----------------------------------------------------------
runInstrListN :: InstrListN -> RunMonad (Maybe Val)
runInstrListN (LIN instrList) = do
    msum <$> mapM runInstrN instrList
    

----------------------------------------------------------
-- runInstrN -------------------------------------------
----------------------------------------------------------
repeatCycle n lin = do
    if n > 0
        then do ret <- runInstrListN lin
                case ret of
                    Nothing -> repeatCycle (n-1) lin
                    Just x -> return $ Just x
        else return Nothing

forCycle s it to by lin = do
    if it <= to
        then do
            ret <- runInstrListN lin
            case ret of
                Nothing -> do
                    changeValInSymTable (s, NumberVal (it+by))
                    forCycle s (it+by) to by lin
                Just x -> return $ Just x
        else return Nothing

whileCycle expn lin = do
    BooleanVal b <- runExpN expn
    if b
        then do 
            ret <- runInstrListN lin
            case ret of
                Nothing -> whileCycle expn lin
                Just x -> return $ Just x
        else return Nothing

runInstrN :: InstrN -> RunMonad (Maybe Val)
runInstrN (WithDoN ldn lin _) = do
    newScope
    runConstrN ldn
    ret <- runInstrListN lin
    removeLastScope
    return ret 

runInstrN (RepeatN expn lin _) = do
    NumberVal n <- runExpN expn  --------- PISO DE n?
    repeatCycle (fromInteger $ floor n) lin
    

runInstrN (AssignN s expn _) = do
    (_,_,mutable,_) <- lookInSymTable s
    when (not mutable) $ error $ "Runtime Error: la variable "++s++" no es mutable :("
    val <- runExpN expn
    changeValInSymTable (s, val)
    return Nothing

runInstrN (ForN s expn1 expn2 lin p) = do
    e11 <- (fromIntegral.floor.fromNumberVal) <$> runExpN expn1
    e22 <- (fromIntegral.floor.fromNumberVal) <$> runExpN expn2
    newScope
    addToSymTable (s,NumberVal e11,False,Number)
    ret <- forCycle s e11 e22 1 lin
    removeLastScope
    return ret

runInstrN (ForByN s expn1 expn2 expn3 lin _) = do
    NumberVal e11 <- runExpN expn1
    NumberVal e22 <- runExpN expn2
    NumberVal e33 <- runExpN expn3
    newScope
    addToSymTable (s,NumberVal e11,False,Number)
    ret <- forCycle s e11 e22 e33 lin
    removeLastScope
    return ret

runInstrN (IfThenN expn lin _) = do
    BooleanVal b <- runExpN expn
    if (b == True) 
        then runInstrListN lin
        else return Nothing

runInstrN (IfThenElseN expn lin1 lin2 _) = do
    BooleanVal b <- runExpN expn
    if (b == True) 
        then runInstrListN lin1
        else runInstrListN lin2

runInstrN (WhileN expn lin _) = do
    whileCycle expn lin

runInstrN (WriteN wordList _) = do
    mapM_ (myPutStr) wordList
    return Nothing
    where
        myPutStr :: WordN -> RunMonad ()
        myPutStr (PWEN exp) = runExpN exp >>= (liftIO . putStr . show)
        myPutStr (PWSN s) = liftIO $ putStr (reverse.drop 1.reverse.drop 1 $ s)
    

runInstrN (WritelnN wordList p) = do
    runInstrN $ WriteN wordList p
    liftIO $ putStrLn ""
    return Nothing

runInstrN (ReadN s _) = do
    (s,_,mutable,t) <- lookInSymTable s
    when (not mutable) $ error $ "Runtime Error: la variable "++s++" no es mutable :("
    input <- liftIO $ getLine
    case t of
        Number -> do
            let nval = readMaybe input :: Maybe Double
            case nval of
                Just d -> changeValInSymTable (s, NumberVal d)
                Nothing -> cantReadError s t
        Boolean -> do
            let nval =  input 
            case nval of
                "true" -> changeValInSymTable (s, BooleanVal True)
                "false" -> changeValInSymTable (s, BooleanVal False)
                _ -> cantReadError s t
    return Nothing
    where
        cantReadError :: String -> OurType -> RunMonad ()
        cantReadError s t = error $ "Runtime error: No se pudo leer variable "++s++" de tipo "++show t++" desde la entrada :("

runInstrN (ReturnN expn _) = Just <$> runExpN expn

runInstrN (ExprN expn) = do
    runExpN expn
    return Nothing




----------------------------------------------------------
-- runExpN ------------------------------------------
----------------------------------------------------------


runExpN :: ExpN -> RunMonad Val

runExpN (IdN s _) = do thrd' <$> lookInSymTable s

runExpN (TrueN _) = return (BooleanVal True)

runExpN (FalseN _) = return (BooleanVal False)
    
runExpN (ParN expn _) = runExpN expn

runExpN (ComparN exp0 s exp1 _) = do
    valExp0 <- runExpN exp0
    valExp1 <- runExpN exp1
    case (valExp0,valExp1) of 
        (NumberVal ex0,NumberVal ex1) ->
            case s of
                "<" -> return $ BooleanVal (ex0 < ex1)
                ">" -> return $ BooleanVal (ex0 > ex1)
                "<=" -> return $ BooleanVal (ex0 <= ex1)
                ">=" -> return $ BooleanVal (ex0 >= ex1)
                "==" -> return $ BooleanVal (ex0 == ex1)
                _ -> return $ BooleanVal (ex0 /= ex1)
        (BooleanVal ex0,BooleanVal ex1) ->
            case s of
                "==" -> return $ BooleanVal (ex0 == ex1)
                _ -> return $ BooleanVal (ex0 /= ex1)

runExpN (NotN expn _) = do
    BooleanVal exp0 <- runExpN expn
    return $ BooleanVal (not exp0)

runExpN (LogicN exp0 s exp1 _) = do
    BooleanVal ex0 <- runExpN exp0
    BooleanVal ex1 <- runExpN exp1
    case s of
        "and" -> return $ BooleanVal (ex0 && ex1)
        "or" -> return $ BooleanVal (ex0 || ex1)

runExpN (FuncN "home" explist _) = do
    listEx <- mapM runExpN (listLEN explist)
    setPosition (0,0) 
    return VoidVal

runExpN (FuncN "openeye" explist _) = do
    listEx <- mapM runExpN (listLEN explist)
    onCursor
    return VoidVal

runExpN (FuncN "closeeye" explist _) = do
    listEx <- mapM runExpN (listLEN explist)
    offCursor
    return VoidVal

runExpN (FuncN "forward" explist _) = do
    listEx <- mapM runExpN (listLEN explist)
    forward $ fromNumberVal $ head listEx
    return VoidVal

runExpN (FuncN "backward" explist _) = do
    listEx <- mapM runExpN (listLEN explist)
    backward $ fromNumberVal $ head listEx
    return VoidVal

runExpN (FuncN "rotatel" explist _) = do
    listEx <- mapM runExpN (listLEN explist)
    rotatel $ fromNumberVal $ head listEx
    return VoidVal

runExpN (FuncN "rotater" explist _) = do
    listEx <- mapM runExpN (listLEN explist)
    rotater $ fromNumberVal $ head listEx
    return VoidVal

runExpN (FuncN "setposition" explist _) = do
    listEx <- mapM runExpN (listLEN explist)
    setPosition $ (fromNumberVal $ listEx !! 0,fromNumberVal $ listEx !! 1)
    return VoidVal

runExpN (FuncN s explist _) = do
    oldSymTable <- getSymTable'
    listEx <- mapM runExpN (listLEN explist)
    createSymTable
    (_,namesVar,listInst,retType) <- findFunDec s
    let variables = foldr (\(name,value) acc-> (name,value,True,getType value):acc) [] (zip namesVar listEx)
    mapM_ addToSymTable variables
    posiblesVal <- mapM runInstrN listInst
    setSymTable oldSymTable
    case msum posiblesVal of
        Just val -> return val
        _ -> case retType of
            Void -> return VoidVal
            _ -> error $ "Runtime Error: Funcion " ++ s ++ " termino sin alcanzar un return :("
    where
        getType (BooleanVal _) = Boolean
        getType (NumberVal _) = Number
        getType _ = Void


runExpN (MinusN exp0 _) = do
    NumberVal ex <- runExpN exp0
    return $ NumberVal (-ex)

runExpN (AritN exp0 s exp1 _) = do
    NumberVal ex0 <- runExpN exp0
    NumberVal ex1 <- runExpN exp1
    case s of 
        "+"   -> return $ NumberVal (ex0+ex1)
        "-"   -> return $ NumberVal (ex0-ex1)
        "*"   -> return $ NumberVal (ex0*ex1)
        "div" -> if (ex1==0) then error "Division entre cero" 
                 else return $ NumberVal (fromIntegral (div (floor ex0) (floor ex1)) :: Double)
        "/"   -> if (ex1==0) then error "Division entre cero" 
                 else return $ NumberVal (ex0 / ex1)
        "mod" -> if (ex1==0) then error "Division entre cero" 
                 else return $ NumberVal (fromIntegral (mod (floor ex0) (floor ex1)) :: Double)
        "%"   -> if (ex1==0) then error "Division entre cero" 
                 else return $ NumberVal (mod' ex0 ex1)

runExpN (NumberLiteralN exp0 _) = do 
    let num = read exp0 :: Double
    return $ NumberVal num

fromNumberVal val = case val of
    NumberVal x -> x
    _ -> 42.0 --Nunca pasara xD (Esperemos xD)