module AST where
import SymTable(Id, SymId)
import Control.Monad

data STARTN = STARTN [IMPORTN] [OUTSIDEN] deriving Show

data IMPORTN = IMPORTN [Id] deriving Show

data OUTSIDEN =
        OUTFUNCTIONINIC FUNCTIONINICN -- |
--        OUTDECLARATION DECLARATIONN |
--        OUTDEFINE DEFINESTRUCTN
          deriving Show

data FUNCTIONINICN =
        FUNCTIONINICN Id [Parameter] RETURNTYPEN [INSTRUCTIONN] deriving Show

data RETURNTYPEN = OKvoid | OKnotvoid OKTYPE deriving Show

data Parameter = Parameter OKTYPE Id deriving Show

-- data DECLARATIONN = DECLARATIONN OKTYPE [DECLARATIONTYPEN] deriving Show

data OKTYPE = POINTER BASICOKTYPE | NOPOINTER BASICOKTYPE deriving Show

data BASICOKTYPE = OKboolean | OKint | OKfloat | OKchar | OKstring | StructId String deriving Show

data DECLARATIONTYPEN = DECTYPEN1 ID2N EXPRESSIONN |
            DECTYPEN2 ID2N
            -- ID2N DECLARATIONTYPEN
            deriving Show

-- Can be empty
--data ID2N = ID2N deriving Show
data ID2N = IDNORMALN String |
      IDARRAYN String EXPRESSIONN deriving Show

data INSTRUCTIONN = GOINGN [PRINTN]                                                    |
          GOINGSLOWLYN [PRINTN]                                                        |
          GOINGMENTALN [PRINTN]                                                        |
          REDMYMINDN [Id]                                                              |
          AMNESIACN String                                                             |
          IFN EXPRESSIONN [INSTRUCTIONN] IFELSEN                                       |
          CANTSTOPN EXPRESSIONN [INSTRUCTIONN]                                         |
          ONEMORETIMEN OKTYPE String EXPRESSIONN EXPRESSIONN EXPRESSIONN [INSTRUCTIONN] |
          GETBACKN EXPRESSIONN                                                         |
          BREAKTHRUN                                                                   |
          EXITMUSICN                                                                   |
        --DECLARATIONNINST DECLARATIONN                                                |
          EXPRESSIONNINST EXPRESSIONN
          deriving Show

data IFELSEN = IFELSEVOID                              |
         IFASKN EXPRESSIONN [INSTRUCTIONN] IFELSEN     |
         OTHERSIDEN [INSTRUCTIONN]
         deriving Show

-- Probablemente un detallito aca
data PRINTN =   PRINTSTRINGS String PRINTN             |
        PRINTSTRING String
        deriving Show

{-
data DEFINESTRUCTN = BANDN String LDECLARATIONSN       |
           UNIONN String LDECLARATIONSN                |
           DEFINESTRUCTN
           deriving Show

data LDECLARATIONSN = REC1 LDECLARATIONSN DECLARATIONN |
            REC2 DECLARATIONN deriving Show
-}

data EXPRESSIONN = IDEXPRESSION String                 |
           NUMBEREXPN String                           |
           STRINGEXPN String                           |
           OKN                                         |
           NOTOKN                                      |
           PARENTESISN EXPRESSIONN                     |
           COMPARN EXPRESSIONN String EXPRESSIONN      |
           NOTN EXPRESSIONN                     |
           LOGICN EXPRESSIONN String EXPRESSIONN       |
           MINUSN String EXPRESSIONN                   |
           ARITN EXPRESSIONN String EXPRESSIONN        |
           ARRAYINSTN ARRAYPOSN                        |
           EXPSTRUCTN EXPRESSIONSTRUCTN                |
           FUNCCALLN FUNCTIONCALLN                     |
           NEWLIFEN EXPRESSIONN                        |
           POINTERN String                             |
           ASSIGNN String EXPRESSIONN
           deriving Show

--data IDEXPRESSION = IDEXPRESSIONN String deriving Show

data ARRAYPOSN = ARRAYPOSN String String deriving Show

data EXPRESSIONSTRUCTN = EXPRESSIONSTRUCTN String String deriving Show

data FUNCTIONCALLN = FUNCTIONCALLN String [Id] deriving Show

ident = "|  "

putStrWithIdent n s = (replicateM_ n $ putStr ident) >> putStr s

putStrLnWithIdent n s = (replicateM_ n $ putStr ident) >> putStrLn s

printId n s = putStrLnWithIdent n $ "ID: " ++ s

printExpN :: Int -> EXPRESSIONN -> IO()
printExpN n (IDEXPRESSION s) = do
    printId n s

printExpN n (NUMBEREXPN s) = do
    putStrLnWithIdent n $ "Literal numerico: " ++ s

printExpN n (OKN) = do
    putStrLnWithIdent n "Literal booleano: true"
     
printExpN n (NOTOKN) = do
    putStrLnWithIdent n "Literal booleano: false"

printExpN n (PARENTESISN exp) = do
    putStrLnWithIdent n "Expresion entre parentesis:"
    printExpN (n+1) exp
     
printExpN n (COMPARN exp s exp1) = do
    putStrLnWithIdent n "Operacion de comparacion:"
    putStrLnWithIdent (n+1) $ "Comparador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExpN (n+2) exp1
    
printExpN n (NOTN exp) = do
    putStrLnWithIdent n "Negacion booleana:"
    printExpN (n+2) exp

printExpN n (LOGICN exp s exp1) = do
    putStrLnWithIdent n "Operacion binaria logica:"
    putStrLnWithIdent (n+1) $ "Operador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExpN (n+2) exp1

printExpN n (MINUSN s exp) = do
    putStrLnWithIdent n "Menos unario:"
    printExpN (n+1) exp

printExpN n (ARITN exp s exp1) = do
    putStrLnWithIdent n "Operacion binaria aritmetica:"
    putStrLnWithIdent (n+1) $ "Operador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExpN (n+2) exp1

--printExpN n (FUNCCALLN s exp) = do 
 --   putStrLnWithIdent n "Llamada de funcion:"
 --   printId (n+1) s
 --   printExpListN (n+1) exp
    

printSTARTN :: Int -> STARTN -> IO()
printSTARTN n (STARTN imports outsides) = do
    putStrLnWithIdent n "Constructor de Programa:"
    mapM_ (printImportN (n+1)) imports
    mapM_ (printFuncListN (n+1)) outsides

printImportN :: Int -> IMPORTN -> IO()
printImportN n (IMPORTN ids) = do
	putStrWithIdent n "Por aqui voy bien"

printFuncListN :: Int -> OUTSIDEN -> IO()
printFuncListN n (OUTFUNCTIONINIC funciones) = do 
	putStrWithIdent n "Por aqui tambien"