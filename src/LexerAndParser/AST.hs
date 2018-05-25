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
    putStrLnWithIdent n $ "Literal number: " ++ s

printExpN n (OKN) = do
    putStrLnWithIdent n "Literal boolean: ok"
     
printExpN n (NOTOKN) = do
    putStrLnWithIdent n "Literal boolean: notok"

printExpN n (PARENTESISN exp) = do
    putStrLnWithIdent n "Parenthesis expression:"
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

printExpN n (ARRAYINSTN arraypos) = do
    putStrLnWithIdent n "Operation with Array Position:"
    printArrayPosN (n+2) arraypos

printExpN n (EXPSTRUCTN expstruct) = do
    putStrLnWithIdent n "Operation with Struct Id:"
    printExpStructN (n+2) expstruct

printExpN n (FUNCCALLN func) = do 
   putStrLnWithIdent n "Function Call:"
   printFunctionCallN (n+2) func

printExpN n (NEWLIFEN exp) = do
   putStrLnWithIdent n "New life declaration called:"
   printExpN (n+2) exp

printExpN n (POINTERN pointed) = do
   putStrLnWithIdent n $ "Pointer: " ++ pointed 
    
printArrayPosN :: Int -> ARRAYPOSN -> IO()
printArrayPosN n (ARRAYPOSN arrayid posnumber) = do
    printId n arrayid
    printId n posnumber -- Agregar que imprima numeros

printExpStructN :: Int -> EXPRESSIONSTRUCTN -> IO()
printExpStructN n (EXPRESSIONSTRUCTN structid instructid) = do
    printId n structid
    printId n instructid

printFunctionCallN :: Int -> FUNCTIONCALLN -> IO()
printFunctionCallN n (FUNCTIONCALLN funcid listid) = do
    printId n funcid
    putStrLnWithIdent n "With the next IDs: "
    mapM_ (printId (n+1)) listid 

printSTARTN :: Int -> STARTN -> IO()
printSTARTN n (STARTN imports outsides) = do
    putStrLnWithIdent n "Constructor de Programa: "
    mapM_ (printImportN (n+1)) imports
    mapM_ (printFuncListN (n+1)) outsides

printImportN :: Int -> IMPORTN -> IO()
printImportN n (IMPORTN ids) = do
  putStrLnWithIdent n "Imports list: "
  mapM_ (printId (n+1)) ids

printFuncListN :: Int -> OUTSIDEN -> IO()
printFuncListN n (OUTFUNCTIONINIC funciones) = do 
  putStrLnWithIdent n "Function declared: "
  printFuncInic (n+1) funciones

printFuncInic :: Int -> FUNCTIONINICN -> IO()
printFuncInic n (FUNCTIONINICN funcid parameters rtype instructions) = do
  putStrLnWithIdent n "Function ID: "
  printId (n+1) funcid
  putStrLnWithIdent n "Function parameters: "
  mapM_ (printParameter (n+1)) parameters
  putStrLnWithIdent n "Return type: "
  printReturnType (n+1) rtype
  putStrLnWithIdent n "Instructions list: "
  mapM_ (printInstruction (n+1)) instructions

printParameter :: Int -> Parameter -> IO()
printParameter n (Parameter oktyp okid) = do
  putStrLnWithIdent n "Parameter type: "
  printOKType (n+1) oktyp
  putStrLnWithIdent n "Parameter id: "
  printId (n+1) okid

printOKType :: Int -> OKTYPE -> IO()
printOKType n (POINTER basics) = do
  putStrWithIdent n "Pointer "
  printBasicOKType n basics

printOKType n (NOPOINTER basics) = do
  printBasicOKType n basics

printBasicOKType :: Int -> BASICOKTYPE -> IO()
printBasicOKType n (OKboolean) = do
  putStrLnWithIdent n "Boolean "

printBasicOKType n (OKint) = do
  putStrLnWithIdent n "Int"

printBasicOKType n (OKfloat) = do
  putStrLnWithIdent n "Float"

printBasicOKType n (OKchar) = do
  putStrLnWithIdent n "Char"

printBasicOKType n (OKstring) = do
  putStrLnWithIdent n "String"

printBasicOKType n (StructId struct) = do
  putStrLnWithIdent n $ "Struct of type: " ++ struct

printReturnType :: Int -> RETURNTYPEN -> IO()
printReturnType n (OKvoid) = do
  putStrLnWithIdent n "Void "

printReturnType n (OKnotvoid rtype) = do
  printOKType n rtype

printInstruction :: Int -> INSTRUCTIONN -> IO()
printInstruction n (EXITMUSICN) = do
  putStrLnWithIdent n "Me voy par coño "

printInstruction n (BREAKTHRUN) = do
  putStrLnWithIdent n "Me rompi par coño "

printInstruction n (GETBACKN exps) = do
  putStrLnWithIdent n "Get back instruction: "
  printExpN n exps 

printInstruction n (EXPRESSIONNINST exps) = do
  putStrLnWithIdent n "Expression Instruction: "
  printExpN n exps 