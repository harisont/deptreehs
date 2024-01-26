module UDAnalysis where

import RTree
import UDConcepts
import UDOptions
import Data.List
import Data.Char
import qualified Data.Map as M


-- get statistics from a tree collection: a frequency list in descending order
udFrequencies :: Opts -> [UDSentence] -> [([String],Int)]
udFrequencies opts sents =
   if isOpt opts "SUBTREETYPE"
   then [([prUDType ut],n) | (ut,(n,_)) <- udTypeFrequencies sents, length (udArgs ut) > 1]
   else addTotal $ sortOn ((0-) . snd) $ M.assocs $ udFrequencyMap opts sents
  where
    allWords = concatMap udWordLines sents
    total = length allWords
    addRelative (k,n) = (k,(n, fromIntegral n / fromIntegral total)) ----
    addTotal = ([(["TOTAL SENTENCES"],length sents),(["TOTAL WORDS"],total)]++)

udFrequencyMap :: Opts -> [UDSentence] -> M.Map [String] Int
udFrequencyMap opts sents
   | isOpt opts "SUBTREETYPE" = M.fromList [([prUDType ut],n) | (ut,n) <- M.assocs (udTypeFrequencyMap sents)]
   | isOpt opts "LENGTH" = frequencyMap $ map (return . show . length . udWordLines) sents
   | isOpt opts "DEPTH" = frequencyMap $ map (return . show . depth . udSentence2tree) sents
   | otherwise = frequencyMap $ map f $ allWords
  where
    f = \w -> [fun w | (opt,fun) <- optfuns, isOpt opts opt]
    optfuns = [
      ("FORM", udFORM),
      ("LEMMA", udLEMMA),
      ("POS",   udUPOS),
      ("FEATS", prt . udFEATS),
      ("DISTANCE", \w -> show (udid2int (udHEAD w) - udid2int (udID w))),
      ("DEPREL", udDEPREL)
      ]
    allWords = concatMap udWordLines sents

-- cosine similarity of two treebanks
udCosineSimilarity :: Opts -> [UDSentence] -> [UDSentence] -> Double
udCosineSimilarity opts xs ys = cosineSimilarityOfMaps fxs fys
  where
    fxs = udFrequencyMap opts xs
    fys = udFrequencyMap opts ys

-- features of xs not found in ys
notCoveredFeatures :: Opts -> [UDSentence] -> [UDSentence] -> [[String]]
notCoveredFeatures opts xs ys = [k | k <- M.keys fxs, M.notMember k fys]
  where
    fxs = udFrequencyMap opts xs
    fys = udFrequencyMap opts ys

-------------------
-- another look at the UD2GF task: analyse what shapes of UD trees there are in a treebank
-- and which of them can be covered by a given grammar and annotations
-------------------

-- frequency list of UD types in a treebank
udTypeFrequencyMap :: [UDSentence] -> M.Map UDType Int
udTypeFrequencyMap ss =
    frequencyMap nexx
  where
    nexx = [normalizeUDType ty  | (ty,_) <- exx]
    exx = concatMap (typesInUDTree . udSentence2tree) ss

udTypeFrequencies :: [UDSentence] -> [(UDType,(Int,String))]
udTypeFrequencies ss =
    frequencyExampleList nexx
  where
    nexx = [(normalizeUDType ty, ex)  | (ty,ex) <- exx]
    exx = concatMap (typesInUDTree . udSentence2tree) ss

data UDType = UDType {
  udVal  :: (POS,(Label,[UDData])),
  udArgs :: [(POS,(Label,[UDData]))] -- including the val, to keep its position
  }
 deriving (Eq,Ord,Show)

prUDType ut = unwords $ intersperse "->" $ map prOne (udArgs ut ++ [udVal ut])
  where
    prOne (pos,(label,_)) = pos ++ "(" ++ label ++ ")"

typeOfUDTree :: UDTree -> UDType
typeOfUDTree tr@(RTree un uts) =
  UDType tun
         (map snd (sort (ptun:[(udID n, (udUPOS n,(udDEPREL n, udFEATS n))) | RTree n _ <- uts])))
 where
   (position,tun) = (udID un, (udUPOS un,(head_Label, udFEATS un)))
---   (position,tun) = (udID un, (udUPOS un,(udDEPREL un, udFEATS un)))
   ptun = (udID un, (udUPOS un,(head_Label, udFEATS un)))

typesInUDTree :: UDTree -> [(UDType,String)] -- type and example
typesInUDTree tr =
  (typeOfUDTree tr, topStringOfUDTree tr) :
  concatMap typesInUDTree (subtrees tr)

-- ignore argument order - and morphological tags with [] instead of m
normalizeUDType :: UDType -> UDType
normalizeUDType ut = ut {
  udVal  = head [(p,(l,[])) | (p,(l,m)) <- [udVal ut]],
  udArgs = {-sort-} [(p,(l,[])) | (p,(l,m)) <- udArgs ut]
  }

matchUDType :: UDType -> UDType -> Bool
matchUDType sought tried = normalizeUDType sought == normalizeUDType tried ---- TODO: more precision

findUDTypeInTree :: UDType -> UDTree -> [UDTree]
findUDTypeInTree ty tr@(RTree un uts) =
  [tr | matchUDType ty (typeOfUDTree tr) ] ++
  concatMap (findUDTypeInTree ty) uts

topStringOfUDTree :: UDTree -> String
topStringOfUDTree (RTree n ts) = unwords $ map udFORM $ sortOn udID $ n : map root ts

-------------------------------------
-- descending sorted frequency list of anything, e.g. types in UD trees
frequencyList :: Ord a => [a] -> [(a,Int)]
frequencyList xs = sortOn ((0-) . snd) $ M.assocs $ frequencyMap xs

frequencyMap :: Ord a => [a] -> M.Map a Int
frequencyMap xs = M.fromListWith (+) [(x,1) | x <- xs]

frequencyExampleList :: Ord a => [(a,b)] -> [(a,(Int,b))]
frequencyExampleList xs = sortOn ((0-) . fst . snd) $ M.assocs $ M.fromListWith add [(x,(1,e)) | (x,e) <- xs]
  where
    add (n,e) (m,_) = (n+m,e)

cosineSimilarityOfMaps :: Ord a => M.Map a Int -> M.Map a Int -> Double
cosineSimilarityOfMaps fxs fys = fromIntegral (scalarProduct fxs fys) / (size fxs * size fys)
  where
    scalarProduct fxs fys = sum [x*y | (w,x) <- M.assocs fxs, let y = maybe 0 id (M.lookup w fys)]
    size fs = sqrt (fromIntegral (sum [x*x | (_,x) <- M.assocs fs]))

cosineSimilarity :: Ord a => [a] -> [a] -> Double
cosineSimilarity xs ys = cosineSimilarityOfMaps fxs fys
  where
    fxs = M.fromListWith (+) [(x,1) | x <- xs]
    fys = M.fromListWith (+) [(x,1) | x <- ys]
    scalarProduct fxs fys = sum [x*y | (w,x) <- M.assocs fxs, let y = maybe 0 id (M.lookup w fys)]
    size fs = sqrt (fromIntegral (sum [x*x | (_,x) <- M.assocs fs]))