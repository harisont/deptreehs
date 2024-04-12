module UDAnalysis where

import RTree
import UDConcepts
import Data.List
import Data.Char
import qualified Data.Map as M

type StatOpt = String

allStatOpts = 
  ["DISTANCE", "LENGTH", "DEPTH"] ++ (allFieldNames \\ ["ID", "HEAD"])

-- * Frequencies

-- get statistics from a tree collection: a frequency list in descending order
udFrequencies :: [StatOpt]-> [UDSentence] -> [([String],Int)]
udFrequencies opts sents =
  sortOn ((0-) . snd) $ M.assocs $ udFrequencyMap opts sents

udFrequencyMap :: [StatOpt] -> [UDSentence] -> M.Map [String] Int
udFrequencyMap opts sents
   | "LENGTH" `elem` opts = 
    frequencyMap $ map (return . show . length . udWordLines) sents
   | "DEPTH" `elem` opts = 
    frequencyMap $ map (return . show . depth . sentence2tree) sents
   | otherwise = frequencyMap $ map f allWords
  where
    f w = [fun w | 
            (opt, fun) <- optfuns, opt `elem` opts && opt `elem` allStatOpts]
    optfuns = [
      ("FORM", udFORM),
      ("LEMMA", udLEMMA),
      ("UPOS", udUPOS),
      ("XPOS", udXPOS),
      ("FEATS", prt . udFEATS),
      ("DEPREL", udDEPREL),
      ("DEPS", udDEPS),
      ("MISC", prt . udMISC),
      ("DISTANCE", \w -> show (id2int (udHEAD w) - id2int (udID w)))
      ]
    allWords = concatMap udWordLines sents
    frequencyMap :: Ord a => [a] -> M.Map a Int
    frequencyMap xs = M.fromListWith (+) [(x,1) | x <- xs]

-- * Cosine similarity
-- cosine similarity of two treebanks
udCosineSimilarity :: [StatOpt] -> [UDSentence] -> [UDSentence] -> Double
udCosineSimilarity opts xs ys = cosineSimilarityOfMaps fxs fys
  where
    fxs = udFrequencyMap opts xs
    fys = udFrequencyMap opts ys

cosineSimilarityOfMaps :: Ord a => M.Map a Int -> M.Map a Int -> Double
cosineSimilarityOfMaps fxs fys = 
  fromIntegral (scalarProduct fxs fys) / (size fxs * size fys)
  where
    scalarProduct fxs fys = 
      sum [x*y | (w,x) <- M.assocs fxs, let y = maybe 0 id (M.lookup w fys)]
    size fs = sqrt (fromIntegral (sum [x*x | (_,x) <- M.assocs fs]))