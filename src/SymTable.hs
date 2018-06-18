module SymTable where
import LowLevelAlex
import OKTypes
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S

type Scope = Int
type SymId = (Id, Scope)

type ScopeStack = [Scope]
emptyScopeStack :: ScopeStack
emptyScopeStack = [1,0]

----------------------------------
-----------Scope Set--------------
----------------------------------
type ScopeSet = S.Set Scope

emptyScopeSet :: ScopeSet
emptyScopeSet = (S.insert 1).(S.insert 0) $ S.empty

scopeSetMember :: Scope -> ScopeSet -> Bool
scopeSetMember = S.member

scopeSetDelete :: Scope -> ScopeSet -> ScopeSet
scopeSetDelete = S.delete

scopeSetInsert :: Scope -> ScopeSet -> ScopeSet
scopeSetInsert = S.insert

----------------------------------
-------------Symbols--------------
----------------------------------
data Sym = Sym{
  sym_scope :: Scope,
  sym_Id :: Id,
  sym_pos :: Pos, -- declaration position
  sym_type :: OKType,
  sym_info :: [SymId]
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


