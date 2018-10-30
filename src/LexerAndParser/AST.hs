module AST where
import Scope
import Control.Monad
import OKTypes

-- AST {{{1

--data START = START [IMPORT] [OUTSIDE] deriving Show
newtype START = START [OUTSIDE] deriving Show

--data IMPORT = IMPORT [Id] deriving Show

newtype OUTSIDE = OUTASSIGN [EXPRESSION] -- All the expressions are assigns
          deriving Show

data INSTRUCTION = GOING [EXPRESSION]                                  |
          GOINGSLOWLY [EXPRESSION]                                     |
          GOINGMENTAL [EXPRESSION]                                     |
          READMYMIND [EXPRESSION]                                      |
          AMNESIAC EXPRESSION                                          |
          IF EXPRESSION [INSTRUCTION] IFELSE                           |
          CANTSTOP EXPRESSION [INSTRUCTION]                            |
          ONEMORETIME [EXPRESSION] EXPRESSION EXPRESSION [INSTRUCTION] |
          GETBACK (Maybe EXPRESSION)                                   |
          BREAKTHRU                                                    |
          EXITMUSIC                                                    |
          EXPRESSIONINST EXPRESSION
          deriving (Show, Eq)

data IFELSE = IFELSEVOID                              |
         IFASK EXPRESSION [INSTRUCTION] IFELSE        |
         OTHERSIDE [INSTRUCTION]
         deriving (Show, Eq)


data EXPRESSION = IDEXPRESSION {expId::SymId, exp_type::OKType}                                        |
                  INTEXP {expInt::Int, exp_type::OKType}                                               |
                  FLOATEXP {expFloat::Float, exp_type::OKType}                                         |
                  CHAREXP {expChar::Char, exp_type::OKType}                                            |
                  BOOLEANEXP {expBooleanVal::Bool, exp_type::OKType}                                   |

                  STRINGEXP {expVal::String, exp_type::OKType}                                         |
                  ARRAYEXP {expVals::[EXPRESSION], exp_type::OKType}                                   |
                  TUPLEEXP {expVals::[EXPRESSION], exp_type::OKType}                                   |

                  LISTEXP {expVals::[EXPRESSION], exp_type::OKType}                                    |

                  MINUS {expExp::EXPRESSION, exp_type::OKType}                                         |
                  ARIT {expExp1::EXPRESSION, expOp::String, expExp2::EXPRESSION, exp_type::OKType}     |

                  COMPAR {expExp1::EXPRESSION, expComp::String, expExp2::EXPRESSION, exp_type::OKType} |

                  NOT {expExp::EXPRESSION, exp_type::OKType}                                           |
                  LOGIC {expExp1::EXPRESSION, expOp::String, expExp2::EXPRESSION, exp_type::OKType}    |

                  CONCAT {expExp1::EXPRESSION, expExp2::EXPRESSION, exp_type::OKType}                  |

                  ARRAYACCESS {expExp::EXPRESSION, expExpIn::EXPRESSION, exp_type::OKType}             |
                  TUPLEACCESS {expExp::EXPRESSION, expPos::Int, exp_type::OKType, exp_shift::Int}      |

                  LISTACCESS {expExp::EXPRESSION, expExpIn::EXPRESSION, exp_type::OKType}              |
                  RECORDACCESS {expExp::EXPRESSION, expName::String, exp_type::OKType}                 |
                  FUNCTIONCALL {expFuncName::String, expArgs::[EXPRESSION], exp_type::OKType}          |
                  NEWLIFE {expExp::EXPRESSION, exp_type::OKType}                                       |
                  POINTER {expExp::EXPRESSION, exp_type::OKType}                                       |
                  ASSIGN {expLHS::EXPRESSION, expExp::EXPRESSION, exp_type::OKType}
                   deriving (Show, Eq)


-- Print
-- AST Printing {{{1
ident = "|  "

putStrWithIdent n s = (replicateM_ n $ putStr ident) >> putStr s

putStrLnWithIdent n s = (replicateM_ n $ putStr ident) >> putStrLn s

printIdSymbol n s = putStrLnWithIdent n $ "ID: " ++ (fst s) ++ " Scope: " ++ show (snd s)

printId n s = putStrLnWithIdent n $ "ID: " ++ s

printSTARTN :: Int -> START -> IO()
printSTARTN n (START outsides) = do
    putStrLnWithIdent n "Program start: "
    --mapM_ (printImportN (n+1)) imports
    mapM_ (printOutsideListN (n+1)) outsides

--printImportN :: Int -> IMPORT -> IO()
--printImportN n (IMPORT ids) = do
  --putStrLnWithIdent n "Imports list: "
  --mapM_ (printId (n+1)) ids

printOutsideListN :: Int -> OUTSIDE -> IO()
printOutsideListN n (OUTASSIGN exps) = do
  putStrLnWithIdent n "Global assigns: "
  mapM_ (printExpN (n+1)) exps

printExpN :: Int -> EXPRESSION -> IO()
printExpN n (IDEXPRESSION s t) = do
    printIdSymbol n s
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (FLOATEXP s t) = do
    putStrLnWithIdent n $ "Literal number: " ++ show s
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (INTEXP s t) = do
    putStrLnWithIdent n $ "Literal number: " ++ show s
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (STRINGEXP s t) = do
    putStrLnWithIdent n $ "Literal string: " ++ s
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (CHAREXP c t) = do
    putStrLnWithIdent n $ "Literal char: " ++ [c]
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (BOOLEANEXP val t) = do
    putStrLnWithIdent n $ "Literal boolean: " ++ show val
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (COMPAR exp s exp1 t) = do
    putStrLnWithIdent n "Comparison operation: "
    putStrLnWithIdent (n+1) $ "Comparator: " ++ s
    putStrLnWithIdent (n+1) "Left side:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Right side:"
    printExpN (n+2) exp1
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (NOT exp t) = do
    putStrLnWithIdent n "Boolean negation:"
    printExpN (n+2) exp
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (LOGIC exp s exp1 t) = do
    putStrLnWithIdent n "Binary logic operation:"
    putStrLnWithIdent (n+1) $ "Operator: " ++ s
    putStrLnWithIdent (n+1) "Left side:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Right side:"
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t
    printExpN (n+2) exp1

printExpN n (MINUS exp t) = do
    putStrLnWithIdent n "Unary minus:"
    printExpN (n+2) exp
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (ARIT exp s exp1 t) = do
    putStrLnWithIdent n "Binary arithmetic operation:"
    putStrLnWithIdent (n+1) $ "Operator: " ++ s
    putStrLnWithIdent (n+1) "Left side:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Right side:"
    printExpN (n+2) exp1
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (ARRAYACCESS arrayid posnumber t) = do
    putStrLnWithIdent n "Operation with Array Position:"
    printExpN n arrayid
    printExpN n posnumber -- Agregar que imprima numeros
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (RECORDACCESS structid instructid t) = do
    putStrLnWithIdent n "Operation with Struct Id:"
    printExpN n structid
    printId n instructid
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (LISTACCESS arrayid posnumber t) = do
    putStrLnWithIdent n "Operation with List Element:"
    printExpN n arrayid
    printExpN n posnumber -- Agregar que imprima numeros
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (TUPLEACCESS arrayid posnumber t _) = do
    putStrLnWithIdent n "Operation with Tuple Element:"
    printExpN n arrayid
    putStrLnWithIdent n $ "Tuple position: " ++ show posnumber
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (FUNCTIONCALL funcid listid t) = do
    putStrLnWithIdent n "Function Call:"
    printId n funcid
    putStrLnWithIdent n "With the next IDs: "
    mapM_ (printExpN (n+1)) listid
    putStrWithIdent n "Type:\n"
    printOKType (n+1) t

printExpN n (NEWLIFE exp t) = do
   putStrLnWithIdent n "New life declaration called:"
   printExpN (n+2) exp
   putStrWithIdent n "Type:\n"
   printOKType (n+1) t

printExpN n (POINTER pointed t) = do
   putStrLnWithIdent n $ "Pointer: "
   printExpN (n+2) pointed
   putStrWithIdent n "Type:\n"
   printOKType (n+1) t

printExpN n (ASSIGN symid exp t) = do
   putStrLnWithIdent n $ "Assignation: "
   putStrLnWithIdent (n+1) $ "Left side: "
   printExpN (n+2) symid
   putStrLnWithIdent (n+1) $ "Right side: "
   printExpN (n+2) exp
   putStrWithIdent n "Type:\n"
   printOKType (n+1) t

printExpN n (ARRAYEXP exps oktype) = do
   putStrLnWithIdent n $ "Array literal: "
   printOKType (n+1) oktype
   putStrLnWithIdent (n+1) $ "Contents:"
   mapM_ (printExpN (n+2)) exps

printExpN n (LISTEXP exps oktype) = do
   putStrLnWithIdent n $ "List literal: "
   printOKType (n+1) oktype
   putStrLnWithIdent (n+1) $ "Contents:"
   mapM_ (printExpN (n+2)) exps

printExpN n (TUPLEEXP exps oktype) = do
   putStrLnWithIdent n $ "Tuple literal: "
   printOKType (n+1) oktype
   putStrLnWithIdent (n+1) $ "Contents:"
   mapM_ (printExpN (n+2)) exps

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

printOKType n t = do
  putStrLnWithIdent n (show t)

printInstruction :: Int -> INSTRUCTION -> IO()
printInstruction n (EXITMUSIC) = do
  putStrLnWithIdent n "Exit Instruction"

printInstruction n (BREAKTHRU) = do
  putStrLnWithIdent n "Break Instruction"

printInstruction n (GETBACK (Just exps)) = do
  putStrLnWithIdent n "GetBack instruction: "
  printExpN (n+2) exps
printInstruction n (GETBACK Nothing) = do
  putStrLnWithIdent n "GetBack void instruction: "

printInstruction n (EXPRESSIONINST exps) = do
  putStrLnWithIdent n "Expression Instruction: "
  printExpN (n+2) exps

printInstruction n (AMNESIAC free) = do
  putStrLnWithIdent n "Amnesiac Instruction: "
  printExpN (n+2) free

printInstruction n (READMYMIND simbs) = do
  putStrLnWithIdent n "ReadMyMind Instruction: "
  mapM_ (printExpN (n+2)) simbs

printInstruction n (GOING prints) = do
  putStrLnWithIdent n "Go Instruction: "
  mapM_ (printExpN (n+2)) prints

printInstruction n (GOINGSLOWLY prints) = do
  putStrLnWithIdent n "GoSlowly Instruction: "
  mapM_ (printExpN (n+2)) prints

printInstruction n (GOINGMENTAL prints) = do
  putStrLnWithIdent n "GoMental Instruction: "
  mapM_ (printExpN (n+2)) prints

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


--- 1}}}
