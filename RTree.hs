{-|
Module      : RTree
Description : Constructor and utilities for abstract rose trees
License     : BSD-2
Maintainer  : arianna.masciolini@gu.se
Stability   : experimental
Portability : POSIX

Constructor and utilities for (recursive) rose trees.
-}

module RTree where
    
-- | Recursive "[rose](https://en.wikipedia.org/wiki/Rose_tree)" tree 
data RTree a = RTree {
  root :: a,              -- ^ root node
  subtrees :: [RTree a]   -- ^ list of subtrees
  } deriving (Eq,Show,Read)

-- | Leaf nodes of a 'RTree'
leaves :: RTree a -> [a]
leaves t = case t of
  RTree a [] -> [a]
  RTree a ts -> concatMap leaves ts

-- | List of all nodes that make up a 'RTree'
allNodes :: RTree a -> [a]
allNodes t = root t : concatMap allNodes (subtrees t)

-- | List of all of a 'RTree''s subtrees (recursively extracted)
allSubtrees :: RTree a -> [RTree a]
allSubtrees t = t : concatMap allSubtrees (subtrees t)

-- | Number of nodes of a 'RTree'
size :: RTree a -> Int
size = length . allNodes

-- | Depth of a 'RTree'
depth :: RTree a -> Int
depth (RTree _ []) = 1  
depth (RTree _ ts) = 1 + maximum (map depth ts)  

-- | Given two 'RTree's, it checks whether the first one is a subtree of the
-- second one 
isSubtree :: Eq a => RTree a -> RTree a -> Bool
isSubtree t u = t == u || any (isSubtree t) (subtrees u)

-- | 'map' for 'RTree'
rtmap :: (a -> b) -> RTree a -> RTree b
rtmap r (RTree h ts) = RTree (r h) (map (rtmap r) ts)

-- | Indented string representation of a 'RTree'
prIndented :: (a -> String) -> RTree a -> String
prIndented prt = unlines . pr 0 where
  pr i t = indent i (prt (root t)) : concatMap (pr (i+4)) (subtrees t)
  indent i s = replicate i ' ' ++ s

-- | Bracketed string representation of a 'RTree'
prBracketed :: (a -> String) -> RTree a -> String
prBracketed pr t = case t of
  RTree a [] -> pr a
  RTree a ts -> "(" ++ pr a ++ " " ++ unwords (map (prBracketed pr) ts) ++ ")"
