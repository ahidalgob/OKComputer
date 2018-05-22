module AST where
import SymTable(Id, SymId)

data STARTN = STARTN [IMPORTN] [OUTSIDEN] deriving Show

data IMPORTN = IMPORTN [Id] deriving Show

data OUTSIDEN =
        OUTFUNCTIONINIC FUNCTIONINICN |
--        OUTDECLARATION DECLARATIONN |
        OUTDEFINE DEFINESTRUCTN deriving Show

data FUNCTIONINICN =
        FUNCTIONINICN ID [PARAMETERN] RETURNTYPEN [INSTRUCTIONN] deriving Show

data RETURNTYPEN = INTOTHEVOIDN | RETURNSOMN TYPEN deriving Show

data PARAMETERN = PARAMETERN TYPEN ID deriving Show

-- data DECLARATIONN = DECLARATIONN TYPEN [DECLARATIONTYPEN] deriving Show

data TYPEN = TYPENOPOINTERN TYPE2N |
			 TYPEPOINTERN TYPE2N deriving Show

data TYPE2N = BOOLEANN | INTN | FLOATN | CHARN | STRINGN | IDSTRUCTN String deriving Show

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
 					REDMYMINDN [ID]                                                              |
 					AMNESIACN String                                                             |
 					IFN EXPRESSIONN [INSTRUCTIONN] IFELSEN                                       |
 					CANTSTOPN EXPRESSIONN [INSTRUCTIONN]                                         |
 					ONEMORETIMEN TYPEN String EXPRESSIONN EXPRESSIONN EXPRESSIONN [INSTRUCTIONN] |
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

-- Nuevo
data DEFINESTRUCTN = BANDN String LDECLARATIONSN       |
					 UNIONN String LDECLARATIONSN                |
					 DEFINESTRUCTN
					 deriving Show

data LDECLARATIONSN = REC1 LDECLARATIONSN DECLARATIONN |
					  REC2 DECLARATIONN deriving Show

data EXPRESSIONN = IDEXPRESSION String                 |
 				   NUMBEREXPN String                           |
 				   STRINGEXPN String                           |
 				   OKN                                         |
 				   NOTOKN                                      |
 				   PARENTESISN EXPRESSIONN                     |
 				   COMPARN EXPRESSIONN String EXPRESSIONN      |
 				   NOTN String EXPRESSIONN                     |
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

data FUNCTIONCALLN = FUNCTIONCALLN String [ID] deriving Show

