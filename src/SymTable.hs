module SymTable where

type Scope = Int
type Id = String
type SymId = (Id, Scope)
type ScopeStack = [Scope]

data Sym = Sym{
  sym_scope :: Scope,
  sym_Id :: SymId
  -- sym_type :: OKType
}
