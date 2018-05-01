-- Retina - Proyecto de Traductores
-- Analizador de Semantica
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

module ContextChecker where
import AST
import OurType
import OurContextMonad
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Control.Applicative
import Data.Either
import Data.Maybe

data Returned = Yes | Idk | No deriving Eq

(|+|) :: Returned -> Returned -> Returned
(|*|) :: Returned -> Returned -> Returned
Yes |+| _ = Yes
_ |+| Yes = Yes
Idk |+| _ = Idk
_ |+| Idk = Idk
No |+| No = No

No |*| _ = No
_ |*| No = No
Idk |*| _ = Idk
_ |*| Idk = Idk
Yes |*| Yes = Yes


----------------------------------------------------------
-- checkConstrN ------------------------------------------
----------------------------------------------------------
checkConstrN :: ConstrN -> OurContextMonad ()
checkConstrN (PN ldfN instrListN) = do
    addFunctionSign "home" [] Void
    addFunctionSign "openeye" [] Void
    addFunctionSign "closeeye" [] Void
    addFunctionSign "forward" [("number",Number)] Void
    addFunctionSign "backward" [("number",Number)] Void  
    addFunctionSign "rotatel" [("number",Number)] Void
    addFunctionSign "rotater" [("number",Number)] Void
    addFunctionSign "setposition" [("x",Number),("y",Number)] Void
    checkConstrN ldfN
    checkInstrListN instrListN
    return ()

checkConstrN (LDFN l) = do
    mapM_ checkFuncDefN l

-- el encargado de abrir y cerrar el scope (with) 
-- debe ser el que haga newScope y removeLastScope y lastScopeToLog
checkConstrN (LDN l) = do
    mapM_ proccessTypeAndList l 
    where 
        proccessTypeAndList :: (TypeN, [VarN]) -> OurContextMonad ()
        proccessTypeAndList (tN, varNList) = do
            let t = typeNConvert tN
            mapM_ (adder t) varNList -- lanza error sin pos y en el with catcharlo y lanzarlo bien
        adder :: OurType -> VarN -> OurContextMonad ()
        adder t varN = do 
            let (s,me) = case varN of
                    (VarN st) -> (st, Nothing)
                    (VarValN st e) -> (st, Just e)
            case me of
                (Just e) -> do
                    te <- checkExpN e
                    when (te /= t) $ throwError $ OurErrorNoPos ("Tipo de la variable '"++s++"'('"++show t++"') y tipo de su valor a asignar('"++show te++"') no coinciden en su declaracion.")
                Nothing -> return ()
            x <- lookInLastScope s
            case x of
                Nothing -> addToSymTable (s, t)
                (Just _) -> throwError $ OurErrorNoPos ("Variable '"++s++"' definida dos veces en el mismo bloque with-do.")


----------------------------------------------------------
-- checkFuncDefN -----------------------------------------
----------------------------------------------------------
checkFuncDefN :: FuncDefN -> OurContextMonad ()
checkFuncDefN funcDefN = do
    let (funId, paramList, instrListN, lineNum, retType) = case funcDefN of
            DFN s p i (ln,_) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln, Void)
            RDFN s p ret i (ln,c) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln, typeNConvert ret)

    repeated <- lookFunction funId
    when (repeated) $ throwError $ OurError lineNum $ "Funcion '"++funId++"' redefinida."
    addFunctionSign funId paramList retType
    setReturnType $ Just retType

    newScope
    mapM_ (adder funId lineNum) paramList
    lastScopeToLog $ '_':funId

    returned <- checkInstrListN instrListN
    when (retType /= Void) $ case returned of
        Yes -> return ()
        Idk -> warningToLog $ "linea "++show lineNum++": Funcion '"++funId++"' puede no alcanzar un return."
        No -> throwError $ OurError lineNum $ "Funcion '"++funId++"' no tiene instruccion de return."
    removeLastScope
    setReturnType Nothing
    where 
        adder funId lineNum (s, t) = do
            x <- lookInLastScope s
            case x of
                Nothing -> addToSymTable (s, t)
                (Just _) -> throwError $ OurError lineNum ("Variable '"++s++"' definida dos veces en parametros de funcion '"++funId++"'.")


----------------------------------------------------------
-- checkInstrListN ---------------------------------------
----------------------------------------------------------
checkInstrListN :: InstrListN -> OurContextMonad Returned
checkInstrListN (LIN instrList) = do
    (foldl (|+|) No) <$> (mapM checkInstrN instrList)

----------------------------------------------------------
-- checkInstrN -------------------------------------------
----------------------------------------------------------
checkInstrN :: InstrN -> OurContextMonad Returned
checkInstrN (WithDoN ldn lin (lineNum,_)) = do
    newScope
    checkConstrN ldn `catchError` (reThrow lineNum)
    lastScopeToLog $ "bloque with-do (linea "++show lineNum++")"
    res <- checkInstrListN lin
    removeLastScope
    return res
    where
        reThrow :: Int -> OurError -> OurContextMonad ()
        reThrow lineNum (OurErrorNoPos s) = throwError $ OurError lineNum s
        reThrow lineNum e = throwError e

checkInstrN (RepeatN expn lin (lineNum,_)) = do
    et <- checkExpN expn
    when (et /= Number) $ throwError $ OurError lineNum $ "Instruccion repeat esperaba expresion de tipo 'number' (Tipo encontrado: '"++show et++"')." 
    checkInstrListN lin

checkInstrN (AssignN s expn (lineNum,_)) = do
    found <- lookInSymTable s
    when (found == Nothing) $ throwError $ OurError lineNum $ "'"++s++"' no esta declarada en este alcance."
    et <- checkExpN expn 
    when (et /= fromJust found) $ throwError $ OurError lineNum ("Se esperaba expresion de tipo '"++show (fromJust found)++"' en lado derecho de asignacion (Tipo encontrado: '"++show et++"').")
    return No

checkInstrN (ForN s expn1 expn2 lin (lineNum,_)) = do
    e1t <- checkExpN expn1
    e2t <- checkExpN expn2
    when (e1t /= Number || e2t /= Number) $ throwError $ OurError lineNum $"Las expresiones de un for deben ser de tipo number (Tipos encontrados ('"++show e1t++"','"++show e2t++"'))."
    newScope
    addToSymTable (s, Number)
    lastScopeToLog $ "for (linea "++show lineNum++")"
    res <- checkInstrListN lin
    removeLastScope
    return res

checkInstrN (ForByN s expn1 expn2 expn3 lin (lineNum,_)) = do
    e1t <- checkExpN expn1
    e2t <- checkExpN expn2
    e3t <- checkExpN expn3
    when (e1t /= Number || e2t /= Number || e3t /= Number) $ throwError $ OurError lineNum $ "Las expresiones de un for con salto deben ser de tipo 'number' (Tipos encontrados ('"++show e1t++"','"++show e2t++"','"++show e3t++"'))."
    newScope
    addToSymTable (s, Number)
    lastScopeToLog $ "for-by (linea "++show lineNum++")"
    res <- checkInstrListN lin
    removeLastScope
    return res

checkInstrN (IfThenN expn lin (lineNum,_)) = do
    et <- checkExpN expn
    when (et /= Boolean) $ throwError $ OurError lineNum $ "Se esperaba tipo 'boolean' en condicion de if (Tipo encontrado: '"++show et++"')."
    ((|*|) Idk) <$> checkInstrListN lin

checkInstrN (IfThenElseN expn lin1 lin2 (lineNum,_)) = do
    et <- checkExpN expn
    when (et /= Boolean) $ throwError $ OurError lineNum $ "Se esperaba tipo 'boolean' en condicion de if-else (Tipo encontrado: '"++show et++"')."
    ret1 <- checkInstrListN lin1
    ret2 <- checkInstrListN lin2
    case (ret1, ret2) of
        (Yes, Yes) -> return Yes
        (No, No) -> return No
        (_, _) -> return Idk

checkInstrN (WhileN expn lin (lineNum,_)) = do
    et <- checkExpN expn
    when (et /= Boolean) $ throwError $ OurError lineNum $ "Se esperaba tipo 'boolean' en condicion de while (Tipo encontrado: '"++show et++"')."
    ((|*|) Idk) <$> checkInstrListN lin

checkInstrN (WriteN wordList (lineNum,_)) = do
    checkWordListN wordList `catchError`  (\ourerror-> case ourerror of
                                                            (OurErrorNoPos s) -> throwError $ OurError lineNum s
                                                            _ -> throwError $ ourerror)

    return No

checkInstrN (WritelnN wordList (lineNum,_)) = do
    checkWordListN wordList `catchError` (\ourerror-> case ourerror of
                                                            (OurErrorNoPos s) -> throwError $ OurError lineNum s
                                                            _ -> throwError $ ourerror)
    return No 

checkInstrN (ReadN s (lineNum, _)) = do
    found <- lookInSymTable s
    when (found == Nothing) $ throwError $ OurError lineNum $ "'"++s++"' variable del read no esta declarada en este alcance." 
    return No

checkInstrN (ReturnN expN (lineNum, _)) = do
    rt <- getReturnType 
    when (rt == Nothing) $ throwError $ OurError lineNum $ "No puede haber instrucciones de return fuera de una funcion."
    when (rt == Just Void) $ throwError $ OurError lineNum $ "No puede haber instrucciones de return en una funcion de tipo 'void'."
    et <- checkExpN expN
    when (fromJust rt /= et) $ throwError $ OurError lineNum $ "La funcion esperaba un tipo de retorno '"++show (fromJust rt)++"' (Tipo encontrado '"++show et++"')."
    return Yes

checkInstrN (ExprN expN) = do
    et <- checkExpN expN
    when (et/=Void) $ warningToLog $ "linea "++show (fst (getPos expN))++": Expresion con valor sin efecto."
    return No


----------------------------------------------------------
-- checkExpN ---------------------------------------------
----------------------------------------------------------
checkExpN :: ExpN -> OurContextMonad OurType
checkExpN (IdN s (lineNum,_)) = do
    bo <- lookInSymTable s
    when (bo==Nothing) $ throwError $ OurError lineNum $ "'"++s++"' no esta declarada en este alcance."
    return (fromJust bo)

checkExpN (TrueN _) = do
    return Boolean

checkExpN (FalseN _) = do
    return Boolean

checkExpN (ParN exp (lineNum,_)) = do
    t <- checkExpN exp
    when (t==Void) $ throwError $ OurError lineNum $ ("La expresion entre parentesis no puede ser de tipo 'void'.")
    return t

checkExpN (ComparN exp s exp1 (lineNum,_)) = do
    t <- checkExpN exp
    t1 <- checkExpN exp1
    when (t1==Void && t==Void) $ throwError $ OurError lineNum $ "Operandos invalidos de tipo 'void' y 'void' para el operador '"++s++"'." 
    when (t1/=t) $ throwError $ OurError lineNum $ "Operandos del operador '"++s++"' no concuerdan en tipo: '"++show t++"' y '"++show t1++"'." 
    when ((s/="==" && s/="/=") && (t==Boolean || t1==Boolean))  $ throwError $ OurError lineNum $ "Operandos del operador "++s++" no acepta parametros de tipo 'boolean'."  
    return Boolean

checkExpN (NotN exp (lineNum,_)) = do
    t <- checkExpN exp
    when (t/=Boolean) $ throwError $ OurError lineNum $ "El operador 'not' espera un 'boolean' y recibe un '"++show t++"'." 
    return t

checkExpN (LogicN exp s exp1 (lineNum,_)) = do
    t <- checkExpN exp
    t1 <- checkExpN exp1
    when (t/=Boolean || t1/=Boolean) $ throwError $ OurError lineNum $ "El operador "++s++" espera un (boolean, boolean) y recibio ("++show t++","++show t1++")."  
    return t

checkExpN (FuncN s expList (lineNum,_)) = do
    defined <- lookFunction s
    found <- lookInSymTable s
    when (not defined && (found == Nothing)) $ throwError $ OurError lineNum $ "'"++s++"' no esta definida en este alcance."
    when (not defined && (found /= Nothing)) $ throwError $ OurError lineNum $ "Variable '"++s++"' no puede ser usada como funcion."
    when (found /= Nothing) $ throwError $ OurError lineNum $ "Definicion de variable '"++s++"' de tipo "++(show $ fromJust found)++" en este alcance oculta la definicion de la funcion '"++s++"'."
    newList <- mapM checkExpN (listLEN expList)
    let anyVoid = any (==Void) newList
    when (anyVoid) $ throwError $ OurError lineNum $ "Uso invalido de expresiones void en los parametros de '"++s++"'."
    (goodArgs,len) <- checkFunction s newList 
    let len2 = length newList
    when (not goodArgs && len==len2) $ throwError $ OurError lineNum $ "Tipos de los parametros de la funcion "++s++" no concuerdan." 
    when (not goodArgs && len<len2) $ throwError $ OurError lineNum $ "Muchos argumentos para la funcion '"++s++"' (Esperados: "++show len++", Recibidos: "++show len2++")."
    when (not goodArgs && len>len2) $ throwError $ OurError lineNum $ "Muy pocos argumentos para la funcion '"++s++"' (Esperados: "++show len++", Recibidos: "++show len2++")."
    getFunctionReturnType s

checkExpN (MinusN exp (lineNum,_)) = do
    t <- checkExpN exp
    when (t/=Number) $ throwError $ OurError lineNum $ "El operador '-' espera un 'number' y recibe un '"++(show t)++"'." 
    return t

checkExpN (AritN exp s exp1 (lineNum,_)) = do
    t <- checkExpN exp
    t1 <- checkExpN exp1
    when (t/=Number || t1/=Number) $ throwError $ OurError lineNum $ "El operador '"++s++"' espera ('number', 'number') y recibe ('"++show t++"','"++show t1++"')."
    return t

checkExpN (NumberLiteralN s _) = do
    return Number

----------------------------------------------------------
-- checkWordListN ----------------------------------------
----------------------------------------------------------
checkWordListN :: [WordN] -> OurContextMonad ()
checkWordListN (wordList) = do
    mapM_ checkWord wordList
    where checkWord (PWEN exp) = do
            t <- checkExpN exp
            case t of 
                Void -> throwError $ OurErrorNoPos ("La expresion a mostrar en pantalla no puede ser de tipo 'void'.")
                _ -> return ()    
          checkWord _ = return ()
