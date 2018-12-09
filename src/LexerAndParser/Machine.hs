module Machine where

import TAC

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List
import Graph

type MIPSCode = [MIPSInstruction]
data MIPSInstruction = F deriving Show

--mipsCode :: TAC ->  Map.Map (String, Scope) Int -> MIPSCode
mipsCode tac offset =
  let (nBlocks, block, blockOfLabel) = buildBlock 0 tac
      graph = buildGraph nBlocks block blockOfLabel
   in (graph, Map.assocs block)


-- BuildGraph {{{1
buildGraph :: Int -> Map.Map Int TAC -> Map.Map String Int -> Graph
buildGraph nBlocks block blockOfLabel = graphFromEdges nBlocks $ getEdges (Map.assocs block) blockOfLabel

getEdges :: [(Int, TAC)] -> Map.Map String Int -> [(Int, Int)]
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
buildBlock :: Int -> TAC -> (Int, Map.Map Int TAC, Map.Map String Int)
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

