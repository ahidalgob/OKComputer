module SymTable where
import LowLevelAlex
import Scope
import OKTypes
import qualified AST
import qualified Data.HashMap.Strict as H


----------------------------------
-------------Symbols--------------
----------------------------------
data Sym = VarSym{ sym_scope :: Scope
                 , sym_Id :: Id
                 , sym_pos :: Pos -- declaration position
                 , sym_type :: OKType
           }
          | FuncSym{ sym_scope :: Scope
                   , sym_Id :: Id
                   , sym_pos :: Pos -- declaration position
                   , sym_type :: OKType
                   , sym_argsId :: [SymId]
                   , sym_AST :: [AST.INSTRUCTION]
                   , sym_defined :: Bool -- or only declared?
                   , sym_label :: String
           }
          | NameTypeSym{ sym_scope :: Scope
                       , sym_Id :: Id
                       , sym_pos :: Pos -- declaration position
                       , sym_type :: OKType
           }
          | ErrorSym { sym_scope :: Scope
                     , sym_Id :: Id
                     , sym_pos :: Pos
                     , sym_type :: OKType
            } deriving Show

isVarSym VarSym{} = True
isVarSym _ = False

isNameTypeSym NameTypeSym{} = True
isNameTypeSym _ = False

isFuncSym FuncSym{} = True
isFuncSym _ = False

----------------------------------
-----------Sym Table--------------
----------------------------------
type SymTable = H.HashMap Id [Sym]
{-data SymTable = SymTable{
  symt_table ::
}-}
emptySymTable = H.empty

symTableInsert :: Sym -> SymTable -> SymTable
symTableInsert s = H.insertWith (++) (sym_Id s) [s]

symTableModify :: Id -> [Sym] -> SymTable -> SymTable
symTableModify = H.insert

symTableLookUp :: Id -> SymTable -> Maybe [Sym]
symTableLookUp = H.lookup


