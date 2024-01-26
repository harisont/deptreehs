module RTree where
    
-- | [Rose](https://en.wikipedia.org/wiki/Rose_tree) (recursive) tree
data RTree a = RTree {
  root :: a,              -- ^ root node
  subtrees :: [RTree a]   -- ^ list of subtrees
  } deriving (Eq,Show,Read)

-- | Returns the leaf nodes of a 'RTree'
leaves :: RTree a -> [a]
leaves t = case t of
  RTree a [] -> [a]
  RTree a ts -> concatMap leaves ts

-- | Returns a list of all nodes that make up a 'RTree'
allNodes :: RTree a -> [a]
allNodes t = root t : concatMap allNodes (subtrees t)

-- | Returns a list of all of a 'RTree''s subtrees (recursively extracted)
allSubtrees :: RTree a -> [RTree a]
allSubtrees t = t : concatMap allSubtrees (subtrees t)

-- | Returns the number of nodes of a 'RTree'
size :: RTree a -> Int
size = length . allNodes

-- | Returns the depth of a 'RTree'
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

-- | Returns the indented string representation of a 'RTree'
prIndented :: (a -> String) -> RTree a -> String
prIndented prt = unlines . pr 0 where
  pr i t = indent i (prt (root t)) : concatMap (pr (i+4)) (subtrees t)
  indent i s = replicate i ' ' ++ s

-- | Returns the bracketed string representation of a 'RTree'
prBracketed :: (a -> String) -> RTree a -> String
prBracketed pr t = case t of
  RTree a [] -> pr a
  RTree a ts -> "(" ++ pr a ++ " " ++ unwords (map (prBracketed pr) ts) ++ ")"
