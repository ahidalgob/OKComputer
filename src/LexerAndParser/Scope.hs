module Scope where
import OKTypes
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
emptyScopeSet = S.insert 1 . S.insert 0 $ S.empty

scopeSetMember :: Scope -> ScopeSet -> Bool
scopeSetMember = S.member

scopeSetDelete :: Scope -> ScopeSet -> ScopeSet
scopeSetDelete = S.delete

scopeSetInsert :: Scope -> ScopeSet -> ScopeSet
scopeSetInsert = S.insert
