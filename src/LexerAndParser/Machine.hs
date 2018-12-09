module Machine where

import TAC

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List

type MIPSCode = [MIPSInstruction]
data MIPSInstruction = F deriving Show

--mipsCode :: TAC ->  Map.Map (String, Scope) Int -> MIPSCode
mipsCode tac offset =
  let (nBlocks, block, blockOfLabel) = buildBlock 0 tac
   in Map.assocs block

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
        isJump Return{} = True
        isJump ReturnVoid{} = True
        isJump _ = False

