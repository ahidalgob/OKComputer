module SymTable where
import LowLevelAlex
import Scope
import OKTypes
import AST
import qualified Data.HashMap.Strict as H


----------------------------------
-------------Symbols--------------
----------------------------------
data Sym = Sym{ sym_scope :: Scope,
                sym_Id :: Id,
                sym_pos :: Pos, -- declaration position
                sym_type :: OKType
           }
          | FuncSym{ sym_scope :: Scope,
                sym_Id :: Id,
                sym_pos :: Pos, -- declaration position
                sym_type :: OKType,
                sym_argsId :: [SymId],
                sym_AST :: [INSTRUCTIONN]
           }
          | ErrorSym {
                sym_scope :: Scope,
                sym_Id :: Id,
                sym_pos :: Pos, -- declaration position
                sym_type :: OKType
            } deriving Show


----------------------------------
-----------Sym Table--------------
----------------------------------
type SymTable = H.HashMap Id [Sym]
{-data SymTable = SymTable{
  symt_table ::
}-}
emptySymTable = H.empty

symTableInsert :: Sym -> SymTable -> SymTable
symTableInsert s st = H.insertWith (++) (sym_Id s) [s] st

symTableLookUp :: Id -> SymTable -> Maybe [Sym]
symTableLookUp = H.lookup


