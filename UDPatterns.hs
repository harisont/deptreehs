{-|
Module      : UDPatterns
Description : DSL and functions for pattern matching / replacement on UD trees
License     : BSD-2
Maintainer  : arianna.masciolini@gu.se
Stability   : experimental
Portability : POSIX

Embedded DSL and functions for pattern matching and replacement on 'UDTree's.
For additional information about the semantics of UD patterns, see the 
[pattern matching and replacement docs](https://github.com/harisont/deptreehs/blob/main/pattern_matching_and_replacement.md).
-}

{-# LANGUAGE DeriveDataTypeable #-}

module UDPatterns where

import Data.Data
import Data.Maybe (listToMaybe)
import Data.List (intercalate)

import Utils
import UDStandard
import RTree
import UDTrees

-- * Pattern matching

-- $
-- Pattern matching DSL

data UDPattern =
    FORM String
  | LEMMA String
  | UPOS String
  | XPOS String
  | MISC String String
  | FEATS String
  | FEATS_ String 
  | DEPREL String
  | DEPREL_ String
  | AND [UDPattern]
  | OR [UDPattern]
  | NOT UDPattern
  | SEQUENCE [UDPattern]
  | SEQUENCE_ [UDPattern]
  | TREE UDPattern [UDPattern]
  | TREE_ UDPattern [UDPattern]
  | TRUE
  | PROJECTIVE
  | ARG String String
  | DEPTH Int
  | DEPTH_UNDER Int
  | DEPTH_OVER Int
  | LENGTH Int
  | LENGTH_UNDER Int
  | LENGTH_OVER Int
 deriving (Show,Read,Eq,Typeable,Data)

-- | Given a pattern and a tree, return a list of subtrees matching the 
-- pattern
matchingSubtrees :: UDPattern -> UDTree -> [UDTree]
matchingSubtrees p tree@(RTree node subtrees) = case p of
  SEQUENCE ps  -> maybe [] return $ findMatchingUDSequence True ps tree
  SEQUENCE_ ps  -> maybe [] return $ findMatchingUDSequence False ps tree
  _ -> [tree | matchesPattern p tree] 
    ++ concatMap (matchingSubtrees p) subtrees

-- | Return  'True' if a tree (__or any of its subtrees__) 
-- matches a certain pattern, 'False' otherwise
matchesPattern :: UDPattern -> UDTree -> Bool
matchesPattern patt tree@(RTree node subtrees) = case patt of
  FORM s -> matchString s (udFORM node)
  LEMMA s -> matchString s (udLEMMA node)
  UPOS s -> matchString s (udUPOS node)
  XPOS s -> matchString s (udXPOS node)
  MISC name s -> maybe 
    False 
    (matchString s) $ listToMaybe 
      [intercalate "," vals | 
       UDData arg vals <- udMISC node , arg == name]
  FEATS udds -> udFEATS node == prs udds
  FEATS_ udds -> 
    let uddlist = prs udds in
    or [fs == uddlist | fs <- sublists (length uddlist) (udFEATS node)]
  DEPREL s -> matchString s (udDEPREL node)
  DEPREL_ s -> matchString s (takeWhile (/=':') (udDEPREL node))
  AND ps -> and [matchesPattern p tree | p <- ps]
  OR ps -> or [matchesPattern p tree | p <- ps]
  NOT p -> not (matchesPattern p tree)
  SEQUENCE ps -> 
    maybe False (const True) $ findMatchingUDSequence True ps tree
  SEQUENCE_ ps -> 
    maybe False (const True) $ findMatchingUDSequence False ps tree
  TREE p ps -> matchesPattern p tree
    && length ps == length subtrees
    && and [matchesPattern q t | (q,t) <- zip ps subtrees]
  TREE_ p ps ->
    or [matchesPattern (TREE p ps) (RTree node qs) 
        | qs <- sublists (length ps) subtrees]
  TRUE -> True
  PROJECTIVE -> isProjective tree
  ARG pos deprel -> matchesPattern (AND [UPOS pos, DEPREL deprel]) tree
  DEPTH d -> depth tree == d
  DEPTH_UNDER d -> depth tree < d
  DEPTH_OVER d -> depth tree > d
  LENGTH d -> length (allNodes tree) == d
  LENGTH_UNDER d -> length (allNodes tree) < d
  LENGTH_OVER d -> length (allNodes tree) > d
  where
    matchString p s = case p of
      '*':pp -> pp == drop (length s - length pp) s
      _:_ | last p =='*' -> init p == take (length (init p)) s
      _ -> p == s

-- | Helper function for sequence matching
findMatchingUDSequence :: 
     Bool         -- ^ Whether gaps are allowed or not 
  -> [UDPattern]  -- ^ The list of patterns to be matched in sequence
  -> UDTree       -- ^ The tree to perform the matching on
  -> Maybe UDTree -- ^ The smallest matching subtree, if any
findMatchingUDSequence strict ps tree 
  | null ps = return tree
  | length ps > length nodes = Nothing
  | otherwise =   --- makes sense only for node-matching patterns
       case [snodes |
             snodes <- parts (length ps) nodes,
             all (\ (p,n) -> matchesPattern p (RTree n [])) (zip ps snodes)
             ] of
         snodes:_ -> 
          smallestSpanningUDSubtree (begin snodes) (end snodes) tree
         _ -> Nothing
 where
  nodes = udWordLines (tree2sentence tree)
  parts = if strict then segments else sublists
  begin ns = id2int (udID (head ns)) -- exists because ps > 0
  end ns = id2int (udID (last ns))

-- * Pattern replacement

-- $
-- Pattern replacement DSL

data UDReplacement =
    REPLACE_FORM String String
  | REPLACE_LEMMA String String
  | REPLACE_POS String String
  | REPLACE_XPOS String String
  | REPLACE_MISC String String String
  | REPLACE_DEPREL String String
  | REPLACE_DEPREL_ String String
  | REPLACE_FEATS String String
  | REPLACE_FEATS_ String String
  | IF UDPattern UDReplacement
  | UNDER UDPattern UDReplacement
  | OVER UDPattern UDReplacement
  | PRUNE UDPattern Int
  | FILTER_SUBTREES UDPattern UDPattern
  | FLATTEN UDPattern
  | RETARGET UDPattern UDPattern UDPattern  
  | CHANGES [UDReplacement]
  | COMPOSE [UDReplacement]
 deriving (Show,Read,Eq)

-- | Given a replacement pattern and a tree, return the "replaced" tree
-- and a flag indicating whether the tree has been modified or not
replaceWithUDPattern :: UDReplacement -> UDTree -> (UDTree,Bool)
replaceWithUDPattern rep tree@(RTree node subtrs) = case rep of
  REPLACE_FORM old new | matchesPattern (FORM old) tree -> 
    true $ tree{root = node{udFORM = new}}
  REPLACE_LEMMA old new | matchesPattern (LEMMA old) tree -> 
    true $ tree{root = node{udLEMMA = new}}
  REPLACE_POS old new | matchesPattern (UPOS old) tree -> 
    true $ tree{root = node{udUPOS = new}}
  REPLACE_XPOS old new | matchesPattern (XPOS old) tree -> 
    true $ tree{root = node{udXPOS = new}}
  REPLACE_MISC name old new | matchesPattern (MISC name old) tree -> 
    true $ tree {
      root = node {
        udMISC = map 
          (\ud -> if udArg ud == name 
                    then ud {udVals = getSeps ',' new} 
                    else ud) 
          (udMISC node)}}
  REPLACE_DEPREL old new | matchesPattern (DEPREL old) tree -> 
    true $ tree{root = node{udDEPREL = new}}
  REPLACE_DEPREL_ old new | matchesPattern (DEPREL_ old) tree -> 
    true $ tree{root = node{udDEPREL = new}}
  REPLACE_FEATS old new | matchesPattern (FEATS old) tree -> 
    true $ tree{root = node{udFEATS = prs new}}
  REPLACE_FEATS_ old new | matchesPattern (FEATS_ old) tree -> true $
    let news = [(udArg fv, udVals fv) | fv <- prs new] in
    tree {
      root = node {
        udFEATS = [maybe fv (\v -> fv{udVals = v}) (lookup (udArg fv) news) 
                    | fv <- udFEATS node]}}
  IF cond replace | matchesPattern cond tree -> 
    replaceWithUDPattern replace tree
  UNDER cond replace | matchesPattern cond tree -> 
    true $ tree{ subtrees = map (fst . replaceWithUDPattern replace) subtrs } 
  OVER cond replace | any (matchesPattern cond) subtrs -> 
    replaceWithUDPattern replace tree
  PRUNE cond depth | matchesPattern cond tree -> 
    true $ flattenRTree depth tree
  FILTER_SUBTREES cond scond | matchesPattern cond tree ->
    let sts = [st | st <- subtrs, matchesPattern scond st]
    in (RTree node sts, length sts /= length subtrs)
  RETARGET cond patt1 patt2 | matchesPattern cond tree ->
    let
      newhead = [subtr | subtr <- subtrs, matchesPattern patt2 subtr]
      retarget st = case newhead of
        subtr:_ | udID (root st) == udID (root subtr) ->
          [subtr{subtrees = subtrees subtr ++
              [t { root = (root t) { udHEAD = udID (root subtr) } } 
                | t <- subtrs, matchesPattern patt1 t] }]
        _:_ | matchesPattern patt1 st -> []
        _ -> [st]
      sts = concat [retarget st | st <- subtrs]
    in (RTree node sts, length sts /= length subtrs)
  FLATTEN cond | matchesPattern cond tree ->
    let sts = concat
                [subtr { subtrees = [] }:
                  [t { root = (root t) { udHEAD = udID node }} 
                    | t <- subtrees subtr]
                  | subtr <- subtrs]
    in (RTree node sts, length sts /= length subtrs)
  CHANGES reps -> case reps of
    r:rs -> case replaceWithUDPattern r tree of
      (tr,True) -> (tr,True)
      _ -> replaceWithUDPattern (CHANGES rs) tree
    _ -> (tree,False)
  COMPOSE reps -> case reps of
    r:rs -> case replaceWithUDPattern r tree of
      (tr,b) -> 
        let (tr2,bs) = replaceWithUDPattern (COMPOSE rs) tr 
        in (tr2, b || bs)
    _ -> (tree,False)
  _ -> (tree,False)
 where
    true t = (t,True)
    flattenRTree d tr@(RTree node subtrs) = case d of
      0 -> RTree node []
      _ -> RTree node (map (flattenRTree (d-1)) subtrs)

-- | Given a replacement and a tree, return a list of subtrees matching the 
-- pattern and a flag indicating whether the tree has been modified or not
replacementsWithUDPattern :: UDReplacement -> UDTree -> (UDTree,Bool)
replacementsWithUDPattern rep tree = case replaceWithUDPattern rep tree of
  (RTree node subtrs,b) -> 
    let (trs,bs) = unzip (map (replacementsWithUDPattern rep) subtrs)
    in (RTree node trs, or (b:bs))
