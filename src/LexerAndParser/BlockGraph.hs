module BlockGraph where

import TAC

import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import Data.Set(Set)
import Graph

type Variable = (String, Scope)
type BlockId = Int

type IN = Set Variable
type OUT = Set Variable
type INS = Map BlockId IN
type OUTS = Map BlockId OUT

getBlocksWithAliveVariables :: TAC -> (Int, Map BlockId TAC, OUTS)
getBlocksWithAliveVariables tac =
  let (nBlocks, tacOfBlock, blockOfLabel) = buildBlock 0 tac
      graph = buildGraph nBlocks tacOfBlock blockOfLabel
      emptyVariableSet = Map.fromAscList [(i, Set.empty) | i <- [0..(nBlocks-1)]]
      (_, outs) = iterateFindSets graph tacOfBlock (emptyVariableSet, emptyVariableSet)
   --in (graph, Map.assocs tacOfBlock, Map.assocs ins, Map.assocs outs)
   in (nBlocks, tacOfBlock, outs)


-- BuildGraph {{{1
-- build graph from blocks {{{2
buildGraph :: Int -> Map BlockId TAC -> Map String BlockId -> Graph
buildGraph nBlocks block blockOfLabel = graphFromEdges nBlocks $ filter (\(x,y) -> x/=0 && y/=0) $ getEdges (Map.assocs block) blockOfLabel

getEdges :: [(BlockId, TAC)] -> Map String BlockId -> [(BlockId, BlockId)]
getEdges [] _ = []
getEdges ((v,tac):ls) blockOfLabel
  | isIfGoto (last tac) = if null ls then (v, nodeOfLabel (last tac)):restOfEdges else (v,nodeOfLabel (last tac)):(v, v+1):restOfEdges
  | isGoto (last tac) = (v, nodeOfLabel (last tac)):restOfEdges
  | isReturn (last tac) = restOfEdges
  | otherwise = (v, v+1):restOfEdges
  where isIfGoto IfGoto{} = True
        isIfGoto IfRelGoto{} = True
        isIfGoto _ = False
        isGoto Goto{} = True
        isGoto _ = False
        isReturn Return{} = True
        isReturn ReturnVoid = True
        isReturn _ = False
        nodeOfLabel (IfGoto _ label) = nodeOfLabel (Goto label)
        nodeOfLabel (IfRelGoto _ _ _ label) = nodeOfLabel (Goto label)
        nodeOfLabel (Goto label) = fromJust $ Map.lookup label blockOfLabel
        restOfEdges = getEdges ls blockOfLabel


-- buildBlock {{{2
buildBlock :: Int -> TAC -> (Int, Map BlockId TAC, Map String BlockId)
buildBlock n [] = (n, Map.empty, Map.empty)
buildBlock n (ins:tac) =
  let labelI = fromMaybe (length tac) $ findIndex isLabel tac
      jumpI = fromMaybe (length tac) $ findIndex isJump tac
      endI = (jumpI+1) `min` labelI

      (blk, restOfTac) = splitAt endI tac
      (n', block, blockOfLabel) = buildBlock (n+1) restOfTac
      block' = Map.insert n (ins:blk) block
      blockOfLabel' = if isLabel ins
                        then let PutLabel label = ins
                              in Map.insert label n blockOfLabel
                        else blockOfLabel
   in (n', block', blockOfLabel')
  where isLabel PutLabel{} = True
        isLabel _ = False
        isJump Goto{} = True
        isJump IfGoto{} = True
        isJump IfRelGoto{} = True
        isJump Return{} = True
        isJump ReturnVoid{} = True
        isJump Call{} = True
        isJump CallAssign{} = True
        isJump _ = False

-- Alive variables {{{1

inFromOut :: OUT -> TAC -> IN
inFromOut inSet tac = foldr f inSet tac
  where f :: Instruction -> OUT -> OUT
        f (BinOpInstr x y _ z) = insert' z . insert' y . delete' x
        f (UnOpInstr x _ y) = insert' y . delete' x
        f (CopyInstr x y) = insert' y . delete' x
        f (IfGoto x _) = insert' x
        f (IfRelGoto x _ y _) = insert' x . insert' y
        f (Param x) = insert' x
        f (CallAssign x _ _) = delete' x
        f (Return x) = insert' x
        f (ArrayGetPos x _ y) = insert' y . delete' x
        f (ArraySetPos _ x y) = insert' y . insert' x
        f (GetAddress x y) = insert' y . delete' x
        f (GetContents x y) = insert' y . delete' x
        f (Print x) = insert' x
        f _ = id

        insert' :: X -> OUT -> OUT
        insert' (Name symId) = Set.insert symId
        insert' t@Temporal{} = Set.insert (tmpToSymId t)
        insert' _ = id

        delete' :: X -> OUT -> OUT
        delete' (Name symId) = Set.delete symId
        delete' t@Temporal{} = Set.delete (tmpToSymId t)
        delete' _ = id

outFromSuccessors :: INS -> Graph -> BlockId -> OUT
outFromSuccessors ins graph id = foldl Set.union Set.empty [fromJust $ Map.lookup s ins | s <- successors graph id]

iterateFindSets :: Graph -> Map BlockId TAC -> (INS, OUTS) -> (INS,OUTS)
iterateFindSets graph tacOfBlock (ins, outs) = if ins==ins' && outs==outs' then (ins,outs) else iterateFindSets graph tacOfBlock (ins', outs')
  where newIn :: BlockId -> IN
        newIn id = inFromOut (fromJust $ Map.lookup id outs) (fromJust $ Map.lookup id tacOfBlock)
        ins' = Map.fromAscList $ zip [0..(n-1)] $ map newIn [0..(n-1)]
        outs' = Map.fromAscList $ zip [0..(n-1)] $ map (outFromSuccessors ins' graph) [0..(n-1)]

        n = Map.size ins
