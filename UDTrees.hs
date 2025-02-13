{-|
Module      : UDTrees
Description : Utilities for UD trees
License     : BSD-2
Maintainer  : arianna.masciolini@gu.se
Stability   : experimental
Portability : POSIX

Utilities for UD trees, including conversions from and to 'UDSentence's
-}

module UDTrees where

import Data.List


import RTree
import UDStandard

-- | Tree represenation of a UD sentence. A 'UDTree' is a 'RTree' whose nodes
-- are 'UDWord's
type UDTree = RTree UDWord

-- ** Conversions

-- | Convert a 'UDSentence' into a 'UDTree'
sentence2tree :: UDSentence -> UDTree
sentence2tree s = s2t rootWord where
  s2t hd = RTree hd [s2t w | w <- ws, udHEAD w == udID hd]
  rootWord =
    case [w | w <- ws, udHEAD w == rootID] of -- unique if check succeeds
      x:_ -> x
      []  -> error $ 
              "sentence2tree: root word expected to have " 
              ++ show rootID ++ " as root, got instead\n" 
              ++ (prt (UDSentence [] ws))
  ws = udWordLines s

-- | Convert a 'UDTree' into a 'UDSentence' 
tree2sentence :: UDTree -> UDSentence
tree2sentence t = UDSentence {
  udCommentLines = [],
  udWordLines = sortOn udID (allNodes t)
  }

-- ** Printing

-- | Print 'UDTree' as nested 'UDWord's
prtUDTree :: UDTree -> String
prtUDTree = prIndented prt

-- | "Linearize" a 'UDTree' into a one-line sentence
prtUDTreeLin :: UDTree -> String
prtUDTreeLin t = unwords [udFORM n | n <- sortOn udID (allNodes t)]

-- ** Other useful functions

-- | Convert a UD subtree into a complete tree that can be converted into 
-- valid CoNNL-U by rescaling IDs to be in the range 1-n and creating a root
-- node labelled as such  
subtree2tree :: UDTree -> UDTree
subtree2tree = sentence2tree . rescaleIds . tree2sentence . createRoot
  where 
    createRoot tree = tree {
      root = (root tree) {
        udDEPREL = rootLabel, 
        udHEAD = rootID, 
        udMISC = 
          UDData "ORIG_LABEL" [udDEPREL (root tree)]:udMISC (root tree)}}
    rescaleIds uds =
      if ids == [1..length ws]
      then uds
      else uds{udWordLines = map fix ws}
      where
       ws = udWordLines uds
       ids = [n | UDIdInt n <- map udID ws]
       fixes = zip (map udID ws) (map UDIdInt [1..length ws])
       fix udw = udw {
         udID = let idw = udID udw in maybe idw id (lookup idw fixes),
         udHEAD = let idw = udHEAD udw in maybe idw id (lookup idw fixes),
         udMISC = UDData "ADJUSTED" ["True"] : udMISC udw}

-- | Return 'True' if a 'UDTree' is projective, i.e. if it does not have
-- any crossing arcs, 'False' otherwise
isProjective :: UDTree -> Bool
isProjective udt = length nodes - 1 == maxId - minId
 where
    nodes = map (id2int . udID) (allNodes udt)
    maxId = maximum nodes
    minId = minimum nodes

-- | Find the smallest subtree covering tokens between the two given positions 
smallestSpanningUDSubtree :: Int -> Int -> UDTree -> Maybe UDTree
smallestSpanningUDSubtree begin end tree = case tree of
  _ | size tree < 1 + end - begin -> Nothing
  _ -> case [t | t <- subtrees tree, covers t] of
    -- t is unique, since each node occurs once
    t:_ -> smallestSpanningUDSubtree begin end t 
    _ -> Just tree -- must cover due to the size condition
 where
   covers t = 
    all (\n -> n `elem` ([id2int (udID w) | w <- allNodes t])) [begin..end]