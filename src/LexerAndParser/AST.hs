module AST where
import Scope
import OKTypes
import Control.Monad

data START = START [IMPORT] [OUTSIDE] deriving Show

data IMPORT = IMPORT [Id] deriving Show -- TODO SymId

data OUTSIDE =
        OUTASSIGN [EXPRESSION] -- All the expressions are assigns
          deriving Show

{-
data FUNCTIONINICN =
        FUNCTIONINICN Id [Parameter] OKType [INSTRUCTION] deriving Show -- TODO SymId
-}
data Parameter = Parameter{param_type :: OKType, param_id :: SymId} deriving Show -- TODO Should we save this on the AST? It's basically the same as a

data INSTRUCTION = GOING [PRINT]                                         |
          GOINGSLOWLY [PRINT]                                             |
          GOINGMENTAL [PRINT]                                             |
          READMYMIND [EXPRESSION]                                               |
          AMNESIAC String                                                  |
          IF EXPRESSION [INSTRUCTION] IFELSE                            |
          CANTSTOP EXPRESSION [INSTRUCTION]                              |
          ONEMORETIME [EXPRESSION] EXPRESSION EXPRESSION [INSTRUCTION] |
          GETBACK EXPRESSION                                              |
          BREAKTHRU                                                        |
          EXITMUSIC                                                        |
          EXPRESSIONINST EXPRESSION
          deriving Show

data IFELSE = IFELSEVOID                              |
         IFASK EXPRESSION [INSTRUCTION] IFELSE     |
         OTHERSIDE [INSTRUCTION]
         deriving Show


data PRINT =   PRINTSTRING EXPRESSION
        deriving Show


data EXPRESSION = IDEXPRESSION {expId::SymId, exp_type::OKType}                                           |
                   NUMBEREXP {expVal::String, exp_type::OKType}                                           |
                   STRINGEXP {expVal::String, exp_type::OKType}                                           |
                   CHAREXP {expChar::Char, exp_type::OKType}                                              |
                   BOOLEANEXP {expBooleanVal::Bool, exp_type::OKType}                                     |
                   COMPAR {expExp1::EXPRESSION, expComp::String, expExp2::EXPRESSION, exp_type::OKType} |
                   NOT {expExp::EXPRESSION, exp_type::OKType}                                            |
                   LOGIC {expExp1::EXPRESSION, expOp::String, expExp2::EXPRESSION, exp_type::OKType}    |
                   MINUS {expExp::EXPRESSION, exp_type::OKType}                                          |
                   ARIT {expExp1::EXPRESSION, expOp::String, expExp2::EXPRESSION, exp_type::OKType}     |
                   ARRAYPOS {expExp::EXPRESSION, expExpIn::EXPRESSION, exp_type::OKType}                |
                   EXPRESSIONSTRUCT {expExp::EXPRESSION, expName::String, exp_type::OKType}              |
                   FUNCTIONCALL {expFuncName::String, expArgs::[EXPRESSION], exp_type::OKType}           |
                   NEWLIFE {expExp::EXPRESSION, exp_type::OKType}                                        |
                   POINTER {expExp::EXPRESSION, exp_type::OKType}                                        |
                   ASSIGN {expLHS::EXPRESSION, expExp::EXPRESSION, exp_type::OKType}
                   deriving Show


-- Print
-- {{{1
ident = "|  "

putStrWithIdent n s = (replicateM_ n $ putStr ident) >> putStr s

putStrLnWithIdent n s = (replicateM_ n $ putStr ident) >> putStrLn s

printIdSymbol n s = putStrLnWithIdent n $ "ID: " ++ (fst s) ++ " Scope: " ++ show (snd s)

printId n s = putStrLnWithIdent n $ "ID: " ++ s

printExpN :: Int -> EXPRESSION -> IO()
printExpN n (IDEXPRESSION s t) = do
    printIdSymbol n s

printExpN n (NUMBEREXP s t) = do
    putStrLnWithIdent n $ "Literal number: " ++ s

printExpN n (STRINGEXP s t) = do
    putStrLnWithIdent n $ "Literal string: " ++ s

printExpN n (CHAREXP c t) = do
    putStrLnWithIdent n $ "Literal char: " ++ [c]

printExpN n (BOOLEANEXP val t) = do
    putStrLnWithIdent n $ "Literal boolean: " ++ show val

printExpN n (COMPAR exp s exp1 t) = do
    putStrLnWithIdent n "Comparison operation: "
    putStrLnWithIdent (n+1) $ "Comparator: " ++ s
    putStrLnWithIdent (n+1) "Left side:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Right side:"
    printExpN (n+2) exp1

printExpN n (NOT exp t) = do
    putStrLnWithIdent n "Boolean negation:"
    printExpN (n+2) exp

printExpN n (LOGIC exp s exp1 t) = do
    putStrLnWithIdent n "Binary logic operation:"
    putStrLnWithIdent (n+1) $ "Operator: " ++ s
    putStrLnWithIdent (n+1) "Left side:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Right side:"
    printExpN (n+2) exp1

printExpN n (MINUS exp t) = do
    putStrLnWithIdent n "Unary minus:"
    printExpN (n+1) exp

printExpN n (ARIT exp s exp1 t) = do
    putStrLnWithIdent n "Binary arithmetic operation:"
    putStrLnWithIdent (n+1) $ "Operator: " ++ s
    putStrLnWithIdent (n+1) "Left side:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Right side:"
    printExpN (n+2) exp1

printExpN n (ARRAYPOS arrayid posnumber t) = do
    putStrLnWithIdent n "Operation with Array Position:"
    printExpN n arrayid
    printExpN n posnumber -- Agregar que imprima numeros

printExpN n (EXPRESSIONSTRUCT structid instructid t) = do
    putStrLnWithIdent n "Operation with Struct Id:"
    printExpN n structid
    printId n instructid

printExpN n (FUNCTIONCALL funcid listid t) = do
    putStrLnWithIdent n "Function Call:"
    printId n funcid
    putStrLnWithIdent n "With the next IDs: "
    mapM_ (printExpN (n+1)) listid

printExpN n (NEWLIFE exp t) = do
   putStrLnWithIdent n "New life declaration called:"
   printExpN (n+2) exp

printExpN n (POINTER pointed t) = do
   putStrLnWithIdent n $ "Pointer: "
   printExpN (n+2) pointed

printExpN n (ASSIGN symid exp t) = do
   putStrLnWithIdent n $ "Assignation: "
   putStrLnWithIdent (n+1) $ "Left side: "
   printExpN (n+2) symid
   putStrLnWithIdent (n+1) $ "Right side: "
   printExpN (n+2) exp

printSTARTN :: Int -> START -> IO()
printSTARTN n (START imports outsides) = do
    putStrLnWithIdent n "Program start: "
    mapM_ (printImportN (n+1)) imports
    mapM_ (printOutsideListN (n+1)) outsides

printImportN :: Int -> IMPORT -> IO()
printImportN n (IMPORT ids) = do
  putStrLnWithIdent n "Imports list: "
  mapM_ (printId (n+1)) ids

printOutsideListN :: Int -> OUTSIDE -> IO()
{-
printOutsideListN n (OUTFUNCTIONINIC funciones) = do
  putStrLnWithIdent n "Function declared: "
  printFuncInic (n+1) funciones
-}

printOutsideListN n (OUTASSIGN exps) = do
  putStrLnWithIdent n "Global assigns: "
  mapM_ (printExpN (n+1)) exps

{-
printFuncInic :: Int -> FUNCTIONINICN -> IO()
printFuncInic n (FUNCTIONINICN funcid parameters rtype instructions) = do
  putStrLnWithIdent n "Function ID: "
  printId (n+1) funcid
  putStrLnWithIdent n "Function parameters: "
  mapM_ (printParameter (n+1)) parameters
  putStrLnWithIdent n "Return type: "
  printOKType (n+1) rtype
  putStrLnWithIdent n "Instructions list: "
  mapM_ (printInstruction (n+1)) instructions
-}

printParameter :: Int -> Parameter -> IO()
printParameter n (Parameter oktyp okid) = do
  putStrLnWithIdent n "Parameter type: "
  printOKType (n+1) oktyp
  putStrLnWithIdent n "Parameter id: "
  printIdSymbol (n+1) okid

printOKType :: Int -> OKType -> IO()
printOKType n (OKPointer inner) = do
  putStrWithIdent n "Pointer "
  printOKType n inner

printOKType n (OKBoolean) = do
  putStrLnWithIdent n "Boolean"

printOKType n (OKInt) = do
  putStrLnWithIdent n "Int"

printOKType n (OKFloat) = do
  putStrLnWithIdent n "Float"

printOKType n (OKChar) = do
  putStrLnWithIdent n "Char"

printOKType n (OKString) = do
  putStrLnWithIdent n "String"

printOKType n (OKFunc args ret) = do
  putStrLnWithIdent n $ "Function: " ++ show args ++ "->" ++ show ret

printOKType n (OKNameType name oktype) = do
  putStrLnWithIdent n $ "Name of type: " ++ name
  printOKType (n+2) oktype

printOKType n (OKVoid) = do
  putStrLnWithIdent n "Void "

printInstruction :: Int -> INSTRUCTION -> IO()
printInstruction n (EXITMUSIC) = do
  putStrLnWithIdent n "Exit Instruction"

printInstruction n (BREAKTHRU) = do
  putStrLnWithIdent n "Break Instruction"

printInstruction n (GETBACK exps) = do
  putStrLnWithIdent n "GetBack instruction: "
  printExpN (n+2) exps

printInstruction n (EXPRESSIONINST exps) = do
  putStrLnWithIdent n "Expression Instruction: "
  printExpN (n+2) exps

printInstruction n (AMNESIAC free) = do
  putStrLnWithIdent n "Amnesiac Instruction: "
  printId (n+2) free

printInstruction n (READMYMIND simbs) = do
  putStrLnWithIdent n "ReadMyMind Instruction: "
  mapM_ (printExpN (n+2)) simbs

printInstruction n (GOING prints) = do
  putStrLnWithIdent n "Go Instruction: "
  mapM_ (printPrints (n+2)) prints

printInstruction n (GOINGSLOWLY prints) = do
  putStrLnWithIdent n "GoSlowly Instruction: "
  mapM_ (printPrints (n+2)) prints

printInstruction n (GOINGMENTAL prints) = do
  putStrLnWithIdent n "GoMental Instruction: "
  mapM_ (printPrints (n+2)) prints

printInstruction n (IF exps instrs elses) = do
  putStrLnWithIdent n "If Instruction: "
  putStrLnWithIdent n "Conditional: "
  printExpN (n+2) exps
  putStrLnWithIdent n "Instructions list: "
  mapM_ (printInstruction (n+2)) instrs
  printIfelse n elses

printInstruction n (CANTSTOP exps instrs) = do
  putStrLnWithIdent n "CantStop Instruction: "
  putStrLnWithIdent n "Conditional: "
  printExpN (n+2) exps
  putStrLnWithIdent n "Instructions list: "
  mapM_ (printInstruction (n+2)) instrs

--printInstruction n (ONEMORETIME declars exp2 exp3 instrs) = do
printInstruction n (ONEMORETIME decls exp2 exp3 instrs) = do
  putStrLnWithIdent n "OneMoreTime Instruction: "
  putStrLnWithIdent n "Initialization: "
  mapM_ (printExpN (n+2)) decls
  putStrLnWithIdent n "Until: "
  printExpN (n+2) exp2
  putStrLnWithIdent n "Pattern: "
  printExpN (n+2) exp3
  putStrLnWithIdent n "Instructions list: "
  mapM_ (printInstruction (n+2)) instrs




printIfelse :: Int -> IFELSE -> IO()
printIfelse n (IFELSEVOID) = do
  putStrLnWithIdent n "End of If Instruction: "

printIfelse n (IFASK exps instrs elses) = do
  putStrLnWithIdent n "IfYouHaveToAsk Instruction: "
  putStrLnWithIdent n "Conditional: "
  printExpN (n+2) exps
  putStrLnWithIdent n "Instructions list: "
  mapM_ (printInstruction (n+2)) instrs
  printIfelse n elses

printIfelse n (OTHERSIDE instrs) = do
  putStrLnWithIdent n "Otherside Instruction: "
  putStrLnWithIdent n "Instructions list: "
  mapM_ (printInstruction (n+2)) instrs

printPrints :: Int -> PRINT -> IO()
printPrints n (PRINTSTRING printable) = do
  putStrLnWithIdent n "Printable: "
  printExpN (n+2) printable

--- 1}}}
