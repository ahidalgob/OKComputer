module AST where
import SymTable(Id, SymId)

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

