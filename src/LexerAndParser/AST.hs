module AST where
import SymTable(Id, SymId)
import Control.Monad

data STARTN = STARTN [IMPORTN] [OUTSIDEN] deriving Show

data IMPORTN = IMPORTN [Id] deriving Show -- TODO SymId

data OUTSIDEN =
        OUTFUNCTIONINIC FUNCTIONINICN | -- |
        OUTASSIGN [EXPRESSIONN] -- All the expressions are assigns
--        OUTDEFINE DEFINESTRUCTN
          deriving Show

data FUNCTIONINICN =
        FUNCTIONINICN Id [Parameter] RETURNTYPEN [INSTRUCTIONN] deriving Show -- TODO SymId

data RETURNTYPEN = OKvoid | OKnotvoid OKTYPE deriving Show

data Parameter = Parameter OKTYPE SymId deriving Show -- TODO Should we save this on the AST? It's basically the same as a
                                                      -- declaration

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
          READMYMINDN [SymId]                                                           |
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
--data PRINTN =   PRINTSTRINGS String PRINTN             |
--        PRINTSTRING String
--        deriving Show

data PRINTN =   PRINTSTRING String
        deriving Show

{-
data DEFINESTRUCTN = BANDN String LDECLARATIONSN       |
           UNIONN String LDECLARATIONSN                |
           DEFINESTRUCTN
           deriving Show

data LDECLARATIONSN = REC1 LDECLARATIONSN DECLARATIONN |
            REC2 DECLARATIONN deriving Show
-}

data EXPRESSIONN = IDEXPRESSION SymId                  |
           NUMBEREXPN String                           |
           STRINGEXPN String                           |
           CHAREXP Char                                |
           OKN                                         |
           NOTOKN                                      |
           PARENTESISN EXPRESSIONN                     |
           COMPARN EXPRESSIONN String EXPRESSIONN      |
           NOTN EXPRESSIONN                            |
           LOGICN EXPRESSIONN String EXPRESSIONN       |
           MINUSN String EXPRESSIONN                   |
           ARITN EXPRESSIONN String EXPRESSIONN        |
           ARRAYINSTN ARRAYPOSN                        |
           EXPSTRUCTN EXPRESSIONSTRUCTN                |
           FUNCCALLN FUNCTIONCALLN                     |
           NEWLIFEN EXPRESSIONN                        |
           POINTERN String                             |
           ASSIGNN SymId EXPRESSIONN
           deriving Show

--data IDEXPRESSION = IDEXPRESSIONN String deriving Show

data ARRAYPOSN = ARRAYPOSN String String deriving Show

data EXPRESSIONSTRUCTN = EXPRESSIONSTRUCTN String String deriving Show

data FUNCTIONCALLN = FUNCTIONCALLN String [EXPRESSIONN] deriving Show

ident = "|  "

putStrWithIdent n s = (replicateM_ n $ putStr ident) >> putStr s

putStrLnWithIdent n s = (replicateM_ n $ putStr ident) >> putStrLn s

printIdSymbol n s = putStrLnWithIdent n $ "ID: " ++ (fst s) ++ " Scope: "

printId n s = putStrLnWithIdent n $ "ID: " ++ s

printExpN :: Int -> EXPRESSIONN -> IO()
printExpN n (IDEXPRESSION s) = do
    printIdSymbol n s

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
    putStrLnWithIdent n "Comparison operation: "
    putStrLnWithIdent (n+1) $ "Comparator: " ++ s
    putStrLnWithIdent (n+1) "Left side:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Right side:"
    printExpN (n+2) exp1
    
printExpN n (NOTN exp) = do
    putStrLnWithIdent n "Boolean negation:"
    printExpN (n+2) exp

printExpN n (LOGICN exp s exp1) = do
    putStrLnWithIdent n "Binary logic operation:"
    putStrLnWithIdent (n+1) $ "Operator: " ++ s
    putStrLnWithIdent (n+1) "Left side:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Right side:"
    printExpN (n+2) exp1

printExpN n (MINUSN s exp) = do
    putStrLnWithIdent n "Unary minus:"
    printExpN (n+1) exp

printExpN n (ARITN exp s exp1) = do
    putStrLnWithIdent n "Binary arithmetic operation:"
    putStrLnWithIdent (n+1) $ "Operator: " ++ s
    putStrLnWithIdent (n+1) "Left side:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Right side:"
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

printExpN n (ASSIGNN symid exp) = do
   putStrLnWithIdent n $ "Assignation: "
   putStrLnWithIdent (n+1) $ "Left side: "
   printIdSymbol (n+2) symid
   putStrLnWithIdent (n+1) $ "Right side: "
   printExpN (n+2) exp
    
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
    mapM_ (printExpN (n+1)) listid 

printSTARTN :: Int -> STARTN -> IO()
printSTARTN n (STARTN imports outsides) = do
    putStrLnWithIdent n "Program start: "
    mapM_ (printImportN (n+1)) imports
    mapM_ (printOutsideListN (n+1)) outsides

printImportN :: Int -> IMPORTN -> IO()
printImportN n (IMPORTN ids) = do
  putStrLnWithIdent n "Imports list: "
  mapM_ (printId (n+1)) ids

printOutsideListN :: Int -> OUTSIDEN -> IO()
printOutsideListN n (OUTFUNCTIONINIC funciones) = do 
  putStrLnWithIdent n "Function declared: "
  printFuncInic (n+1) funciones

printOutsideListN n (OUTASSIGN exps) = do 
  putStrLnWithIdent n "Global assigns: "
  mapM_ (printExpN (n+1)) exps
  

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
  printIdSymbol (n+1) okid

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
  putStrLnWithIdent n "Exit Instruction"

printInstruction n (BREAKTHRUN) = do
  putStrLnWithIdent n "Break Instruction"

printInstruction n (GETBACKN exps) = do
  putStrLnWithIdent n "GetBack instruction: "
  printExpN (n+2) exps 

printInstruction n (EXPRESSIONNINST exps) = do
  putStrLnWithIdent n "Expression Instruction: "
  printExpN (n+2) exps 

printInstruction n (AMNESIACN free) = do
  putStrLnWithIdent n "Amnesiac Instruction: "
  printId (n+2) free 

printInstruction n (READMYMINDN simbs) = do
  putStrLnWithIdent n "ReadMyMind Instruction: "
  mapM_ (printIdSymbol (n+2)) simbs

printInstruction n (GOINGN prints) = do
  putStrLnWithIdent n "Go Instruction: "
  mapM_ (printPrints (n+2)) prints

printInstruction n (GOINGSLOWLYN prints) = do
  putStrLnWithIdent n "GoSlowly Instruction: "
  mapM_ (printPrints (n+2)) prints

printInstruction n (GOINGMENTALN prints) = do
  putStrLnWithIdent n "GoMental Instruction: "
  mapM_ (printPrints (n+2)) prints

printInstruction n (IFN exps instrs elses) = do
  putStrLnWithIdent n "If Instruction: "
  putStrLnWithIdent n "Conditional: "
  printExpN (n+2) exps
  putStrLnWithIdent n "Instructions list: "
  mapM_ (printInstruction (n+2)) instrs
  printIfelse n elses

printInstruction n (CANTSTOPN exps instrs) = do
  putStrLnWithIdent n "CantStop Instruction: "
  putStrLnWithIdent n "Conditional: "
  printExpN (n+2) exps
  putStrLnWithIdent n "Instructions list: "
  mapM_ (printInstruction (n+2)) instrs 

printInstruction n (ONEMORETIMEN oktyp s exp1 exp2 exp3 instrs) = do
  putStrLnWithIdent n "ONEMORETIMEN Instruction: "
  putStrLnWithIdent n "Ayudenme no sirvo "


printIfelse :: Int -> IFELSEN -> IO()
printIfelse n (IFELSEVOID) = do
  putStrLnWithIdent n "End of If Instruction: "

printIfelse n (IFASKN exps instrs elses) = do
  putStrLnWithIdent n "IfYouHaveToAsk Instruction: "
  putStrLnWithIdent n "Conditional: "
  printExpN (n+2) exps
  putStrLnWithIdent n "Instructions list: "
  mapM_ (printInstruction (n+2)) instrs
  printIfelse n elses

printIfelse n (OTHERSIDEN instrs) = do 
  putStrLnWithIdent n "Otherside Instruction: "
  putStrLnWithIdent n "Instructions list: "
  mapM_ (printInstruction (n+2)) instrs

printPrints :: Int -> PRINTN -> IO()
printPrints n (PRINTSTRING printable) = do
  putStrLnWithIdent n "Printable: "
  printId (n+2) printable