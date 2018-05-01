-- Retina - Proyecto de Traductores
-- AST
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

module AST where
import Control.Monad


data ConstrN = --Construcciones
    PN ConstrN InstrListN                           | --Constructor del programa
    LDFN {listLDFN :: [FuncDefN]}                   | --Lista de declaracion de funciones
    LDN {listLDN :: [(TypeN, [VarN])]}                --Lista de declaracion de variables
    deriving Show
    
data TypeN = 
    BooleanN                                        |
    NumberN
    deriving Show

data ExpN = --Expresiones
    IdN { getString::String, getPos::(Int, Int) }                                   | --Identificador
    TrueN { getPos::(Int, Int) }                                                    | --True
    FalseN { getPos::(Int, Int) }                                                   | --False
    ParN  { getExp::ExpN, getPos::(Int, Int) }                                      | --Parentesis
    ComparN { getExp::ExpN, getString::String, getExp1::ExpN, getPos::(Int, Int) }  | --Operacion de comparacion
    NotN { getExp::ExpN, getPos::(Int, Int) }                                       | --Operacion de negacion
    LogicN  { getExp::ExpN, getString::String, getExp1::ExpN, getPos::(Int, Int) }  | --Operacion logica
    FuncN  { getString::String, getExpList::ExpListN, getPos::(Int, Int) }          | --Llamado de funcion
    MinusN { getExp::ExpN, getPos::(Int, Int) }                                     | --Operacion unaria de negacion
    AritN { getExp::ExpN, getString::String, getExp1::ExpN, getPos::(Int, Int) }    | --Operacion aritmetica
    NumberLiteralN  { getString::String, getPos::(Int, Int) }                         --Literal numerico  
    deriving Show

data FuncDefN = --Definicion de funcion
    DFN String ParamListN InstrListN (Int,Int)      | --Definicion de funcion sin retorno
    RDFN String ParamListN TypeN InstrListN (Int,Int) --Definicion de funcion con retorno
    deriving Show

data ParamListN = --Lista de parametros
    LPN {listLPN :: [(TypeN, String)]}              | --Lista de parametros 
    LPNVN {listLPNVN :: [(TypeN, String)]}            --Lista de parametros no vacia  
    deriving Show

data ExpListN = --Lista de expresiones
    LEN {listLEN :: [ExpN]}                         | --Expresiones que pueden ser vacias
    LENVN {listLENVN :: [ExpN]}                       --Expresiones no vacias  
    deriving Show

data VarListN = --Lista de variables
    LVarN {listLVN :: [VarN]}
    deriving Show

data VarN = --Variables
    VarN String                                     | 
    VarValN String ExpN
    deriving Show

data InstrListN = --Lista de instrucciones
    LIN {listLIN :: [InstrN]}
    deriving Show

data WordListN = --Lista de imprimibles
    LPWN {listLPWN :: [WordN]}
    deriving Show

data WordN =
    PWEN ExpN                                       |
    PWSN String
    deriving Show


data InstrN = --Instrucciones
    WithDoN ConstrN InstrListN (Int,Int)                        |
    RepeatN ExpN InstrListN (Int,Int)                           |
    AssignN String ExpN (Int,Int)                               |
    ForN String ExpN ExpN InstrListN (Int,Int)                  |
    ForByN String ExpN ExpN ExpN InstrListN  (Int,Int)          |
    IfThenN ExpN InstrListN (Int,Int)                           |
    IfThenElseN ExpN InstrListN InstrListN (Int,Int)            |
    WhileN ExpN InstrListN (Int,Int)                            |
    WriteN {listWriteN :: [WordN], posWriteN :: (Int, Int)}     |
    WritelnN {listWritelnN :: [WordN], posWriteLnN::(Int, Int)} |
    ReadN String (Int, Int)                                     |
    ReturnN ExpN (Int,Int)                                      |
    ExprN ExpN
    deriving Show

ident = "|  "

putStrWithIdent n s = (replicateM_ n $ putStr ident) >> putStr s

putStrLnWithIdent n s = (replicateM_ n $ putStr ident) >> putStrLn s

printId n s = putStrLnWithIdent n $ "Identificador: " ++ s

printExpN :: Int -> ExpN -> IO()
printExpN n (IdN s _) = do
    printId n s

printExpN n (TrueN _) = do
    putStrLnWithIdent n "Literal booleano: true"
     
printExpN n (FalseN _) = do
    putStrLnWithIdent n "Literal booleano: false"

printExpN n (ParN exp _) = do
    putStrLnWithIdent n "Expresion entre parentesis:"
    printExpN (n+1) exp
     
printExpN n (ComparN exp s exp1 _) = do
    putStrLnWithIdent n "Operacion de comparacion:"
    putStrLnWithIdent (n+1) $ "Comparador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExpN (n+2) exp1
    
printExpN n (NotN exp _) = do
    putStrLnWithIdent n "Negacion booleana:"
    printExpN (n+2) exp

printExpN n (LogicN exp s exp1 _) = do
    putStrLnWithIdent n "Operacion binaria logica:"
    putStrLnWithIdent (n+1) $ "Operador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExpN (n+2) exp1
    
printExpN n (FuncN s exp _) = do 
    putStrLnWithIdent n "Llamada de funcion:"
    printId (n+1) s
    printExpListN (n+1) exp

printExpN n (MinusN exp _) = do
    putStrLnWithIdent n "Menos unario:"
    printExpN (n+1) exp
    
printExpN n (AritN exp s exp1 _) = do
    putStrLnWithIdent n "Operacion binaria aritmetica:"
    putStrLnWithIdent (n+1) $ "Operador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExpN (n+2) exp1
    
printExpN n (NumberLiteralN s _) = do
    putStrLnWithIdent n $ "Literal numerico: " ++ s


printTypeN :: Int -> TypeN -> IO()
printTypeN n (BooleanN) = do
    putStrLnWithIdent n "Tipo de dato: boolean"
     
printTypeN n (NumberN) = do
    putStrLnWithIdent n "Tipo de dato: number"


printConstrN :: Int -> ConstrN -> IO()
printConstrN n (PN ldf lblock) = do
    putStrLnWithIdent n "Constructor de Programa:"
    printConstrN (n+1) ldf
    printInstrListN (n+1) lblock
    
printConstrN n (LDFN dfl) = do
    putStrLnWithIdent n "Lista de definiciones de funciones:"
    mapM_ (printFuncDefN (n+1)) dfl

printConstrN n (LDN ld) = do
    putStrLnWithIdent n "Declaraciones de datos:"
    mapM_ printDatos ld
    where 
        printDatos (exp, ldst) = do
            printTypeN (n+1) exp
            mapM_ (printVarN (n+2)) ldst


printFuncDefN :: Int -> FuncDefN -> IO()
printFuncDefN n (DFN id lp lir _) = do
    putStrLnWithIdent n "Definicion de funcion:"
    printId (n+1) id
    printParamListN (n+1) lp
    putStrLnWithIdent (n+1) "Cuerpo de la funcion:"
    printInstrListN (n+2) lir    
    
printFuncDefN n (RDFN id lp ret lir _) = do
    putStrLnWithIdent n "Definicion de funcion:"
    printId (n+1) id
    printParamListN (n+1) lp
    putStrLnWithIdent (n+1) "Tipo de retorno:"
    printTypeN (n+2) ret
    putStrLnWithIdent (n+1) "Cuerpo de la funcion:"
    printInstrListN (n+2) lir
    

printParamListN :: Int -> ParamListN -> IO()
printParamListN n (LPN pl) = do 
    putStrLnWithIdent n "Lista de parametros de la funcion: "
    mapM_ (printParam) pl
    where 
        printParam (exp, s) = do
            printTypeN (n+1) exp
            printId (n+1) s


printExpListN :: Int -> ExpListN -> IO()
printExpListN n (LEN lv) = do
    putStrLnWithIdent n "Lista de expresiones:"
    mapM_ (printExpN (n+1)) lv


printVarN :: Int -> VarN -> IO()
printVarN n (VarN s) = do
    putStrLnWithIdent n "Declaracion con identificador:"
    printId (n+1) s

printVarN n (VarValN s exp) = do
    putStrLnWithIdent n "Declaracion con identificador y valor:"
    printId (n+1) s
    putStrLnWithIdent (n+1) "Valor:"
    printExpN (n+2) exp
    
printInstrListN :: Int -> InstrListN -> IO()
printInstrListN n (LIN li) = do 
    putStrLnWithIdent n "Lista de instrucciones:"
    mapM_ (printInstrN (n+1)) li
    
printInstrN :: Int -> InstrN -> IO()
printInstrN n (WithDoN exp exp1 _) = do
    putStrLnWithIdent n "Bloque with-do:"
    printConstrN (n+1) exp
    printInstrListN (n+1) exp1
    
printInstrN n (RepeatN exp exp1 _) = do
    putStrLnWithIdent n "Instruccion repeat:"
    putStrLnWithIdent (n+1) "Cantidad de repeticiones:"
    printExpN (n+2) exp
    printInstrListN (n+1) exp1
    
printInstrN n (AssignN s exp1 _) = do
    putStrLnWithIdent n "Instruccion de asignacion:"
    printId (n+1) s
    putStrLnWithIdent (n+1) "Valor:"
    printExpN (n+2) exp1
    
printInstrN n (ForN s exp exp1 exp2 _) = do
    putStrLnWithIdent n "Instruccion for:"
    putStrLnWithIdent (n+1) "Variable iteradora:"
    printId (n+2) s
    putStrLnWithIdent (n+1) "Desde:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Hasta:"
    printExpN (n+2) exp1
    printInstrListN (n+1) exp2

printInstrN n (ForByN s exp exp1 exp2 exp3 _) = do
    putStrLnWithIdent n "Instruccion for-by:"
    putStrLnWithIdent (n+1) "Variable iteradora:"
    printId (n+2) s
    putStrLnWithIdent (n+1) "Desde:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Hasta:"
    printExpN (n+2) exp1
    putStrLnWithIdent (n+1) "Paso:"
    printExpN (n+2) exp2
    printInstrListN (n+1) exp3

printInstrN n (IfThenN exp exp1 _) = do
    putStrLnWithIdent n "Instruccion if-then:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExpN (n+2) exp
    printInstrListN (n+1) exp1
    
printInstrN n (IfThenElseN exp exp1 exp2 _) = do
    putStrLnWithIdent n "Instruccion if-then-else:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Bloque 1 (then):"
    printInstrListN (n+2) exp1
    putStrLnWithIdent (n+1) "Bloque 2 (else):"
    printInstrListN (n+2) exp2
    
printInstrN n (WhileN exp exp1 _) = do
    putStrLnWithIdent n "Instruccion while-do:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExpN (n+2) exp
    printInstrListN (n+1) exp1

printInstrN n (WriteN l _) = do
    putStrLnWithIdent n "Instruccion de salida:"
    putStrLnWithIdent (n+1) "Lista de expresiones:"
    mapM_ (printWordN (n+2)) l
    
printInstrN n (WritelnN l _) = do
    putStrLnWithIdent n "Instruccion de salida con salto:"
    putStrLnWithIdent (n+1) "Lista de expresiones:"
    mapM_ (printWordN (n+2)) l

printInstrN n (ReadN id _) = do
    putStrLnWithIdent n "Instruccion de entrada:"
    printId (n+1) id

printInstrN n (ReturnN exp _) = do
    putStrLnWithIdent n "Instruccion de return:"
    printExpN (n+1) exp

printInstrN n (ExprN exp) = do
    putStrLnWithIdent n "Instruccion de expresion:"
    printExpN (n+1) exp


printWordListN :: Int -> WordListN -> IO()
printWordListN n (LPWN l) = do
    putStrLnWithIdent n "Expresiones:"
    mapM_ (printWordN (n+1)) l
    

printWordN :: Int -> WordN -> IO()
printWordN n (PWSN s) = do
    putStrLnWithIdent n $ "Cadena de caracteres: " ++ s
    
printWordN n (PWEN exp) = do
    printExpN n exp
    

