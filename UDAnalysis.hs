{-|
Module      : UDAnalysis
Description : Functions for the quantitative analysis of UD treebanks
License     : BSD-2
Maintainer  : arianna.masciolini@gu.se
Stability   : experimental
Portability : POSIX

Functions for the quantitative analysis of UD treebanks: statistics, 
attachment scores and cosine similarities between treebanks.
-}

module UDAnalysis where

import RTree
import UDConcepts
import Data.List
import Data.Char
import qualified Data.Map as M

-- | Name of a unit of analysis (a CoNNL-U `Field`, DISTANCE, LENGTH or DEPTH)
type Opt = String

-- | List of all possible 'Opt's
allOpts :: [Opt]
allOpts = ["DISTANCE", "LENGTH", "DEPTH"] ++ (allFieldNames \\ ["ID", "HEAD"])

-- * Frequencies

-- | Return a frequency map for the given units of analysis.
-- Note that @frequencyMap ["FIELD1", "FIELD2"]@ computes statistic for 
-- combinations of CoNNL-U fields and that LENGTH and DEPTH are sentence-wise
-- metrics that have priority over the token-wise ones, meaning that for 
-- instance @frequencyMap ["FIELD1", "LENGTH"]@ is equivalent to 
-- @frequencyMap ["LENGTH"]@ (statistics for sentence length)
frequencyMap :: [Opt] -> [UDSentence] -> M.Map [Opt] Int
frequencyMap opts sents
   | "LENGTH" `elem` opts = 
    frequencyMap $ map (return . show . length . udWordLines) sents
   | "DEPTH" `elem` opts = 
    frequencyMap $ map (return . show . depth . sentence2tree) sents
   | otherwise = frequencyMap $ map f allWords
  where
    f w = [fun w | 
            (opt, fun) <- optfuns, opt `elem` opts && opt `elem` allOpts]
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
    frequencyMap xs = M.fromListWith (+) [(x,1) | x <- xs]

-- | Shorthand to get frequencies as a list of `Opt`s-count pairs in 
-- descending order
frequencyList :: [Opt] -> [UDSentence] -> [([Opt],Int)]
frequencyList opts sents =
  sortOn ((0-) . snd) $ M.assocs $ frequencyMap opts sents

-- * Attachment scores

-- $
-- Functions for computing Labelled and Unlabelled Attachment scores. 
-- Scores can be computed for a single sentence or for a full corpus.

-- | Scoring criterion
type Criterion = UDWord -> UDWord -> Bool

-- | Labelled Attachment Score
las :: Criterion
las g t = udHEAD g == udHEAD t && udDEPREL g == udDEPREL t

-- | Unlabelled Attachment Score
uas :: Criterion
uas g t = udHEAD g == udHEAD t 

-- | Record type for scores
data UDScore = UDScore {
  udScore :: Double,    -- ^ numerical score (LAS or UAS)
  udMatching :: Int,    -- ^ whether the words are the same (1 or 0 for 
                        -- single-sentence scores, sum for corpus scores
  udTotalLength :: Int, -- ^ number of words
  udSamesLength :: Int, -- ^ number of words with matching (head,label)
  udPerfectMatch :: Int -- ^ whether all words have matching (head,label) 
                        -- (1 or 0 for single-sentence scores, sum for corpus 
                        -- scores)
  } deriving Show

-- | Given a scoring criterion, a gold sentence and a list of possible parses,
-- return the best candidate parse and its score
sentenceScore :: 
  Criterion -> UDSentence -> [UDSentence] -> (UDSentence,UDScore)
sentenceScore agree gold testeds = (tested,score) 
  where
    score = UDScore {
      udScore = fromIntegral (length sames) / fromIntegral (length alls),
      udMatching = areMatching,
      udTotalLength = length alls,
      udSamesLength = length sames,
      udPerfectMatch = if length sames == length alls then 1 else 0
      } 
    alls = udWordLines tested
    tested = maximumBy 
      (\t u -> compare (length (samest t)) (length (samest u))) 
      testeds
    sames = samest tested
    samest tsd = 
      [() | (g,t) <- zip (udWordLines gold) (udWordLines tsd), agree g t]
    areMatching = 
      if map udFORM (udWordLines gold) == map udFORM (udWordLines tested) 
        then 1 
        else 0

-- | Compute the micro or macro corpus-level score. 
-- In the tested corpus, group trees for the same sentence together
corpusScore :: 
  Bool            -- ^ 'True' for micro, 'False' for macro
  -> Criterion    -- ^ scoring criterion
  -> [UDSentence] -- ^ gold corpus
  -> [UDSentence] -- ^ tested corpus (e.g. automatically parsed sentences)
  -> UDScore
corpusScore isMicro agree golds tests = UDScore {
  udScore = if isMicro
            then fromIntegral numbersames / fromIntegral numberalls                -- micro score (per word)
            else sum (map udScore sentenceScores) / fromIntegral (length sentenceScores), -- macro score (per sentence)
  udMatching = sum (map udMatching sentenceScores),
  udTotalLength = numberalls,
  udSamesLength = numbersames,
  udPerfectMatch = sum (map udPerfectMatch sentenceScores)
  }
  where
    sentenceScores =
      filter ((>0) . udMatching) $ map snd $
        map (uncurry (sentenceScore agree)) (zip golds testgroups)
    numberalls  =  sum (map udTotalLength sentenceScores)
    numbersames =  sum (map udSamesLength sentenceScores)
    testgroups  = groupBy (\t u -> sent t == sent u) tests
    sent t = unwords $ map udFORM $ udWordLines t

-- * Cosine similarity

-- | Compute the cosine similarity between two treebanks
cosineSimilarity :: [Opt] -> [UDSentence] -> [UDSentence] -> Double
cosineSimilarity opts xs ys = cosineSimilarityOfMaps fxs fys
  where
    fxs = frequencyMap opts xs
    fys = frequencyMap opts ys
    cosineSimilarityOfMaps fxs fys = 
      fromIntegral (scalarProduct fxs fys) / (size fxs * size fys)
      where
        scalarProduct fxs fys = 
          sum [x * y | 
            (w,x) <- M.assocs fxs, let y = maybe 0 id (M.lookup w fys)]
        size fs = sqrt (fromIntegral (sum [x*x | (_,x) <- M.assocs fs]))

