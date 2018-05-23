module SymTable where
import LowLevelAlex
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S

type Scope = Int
type Id = String
type SymId = (Id, Scope)

type ScopeStack = [Scope]
emptyScopeStack = []

type ScopeSet = S.Set Scope

emptyScopeSet = S.empty

scopeSetMember :: Scope -> ScopeSet -> Bool
scopeSetMember = S.member

data Sym = Sym{
  sym_scope :: Scope,
  sym_Id :: Id,
  sym_pos :: Pos -- declaration position
  -- sym_type :: OKType
} deriving Show

type SymTable = H.HashMap Id [Sym]
{-data SymTable = SymTable{
  symt_table ::
}-}
emptySymTable = H.empty

symTableInsert :: Sym -> SymTable -> SymTable
symTableInsert s st = H.insertWith (++) (sym_Id s) [s] st

symTableLoopUp :: Id -> SymTable -> Maybe [Sym]
symTableLoopUp = H.lookup
