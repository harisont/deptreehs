module UDEval where

import Data.List

import UDConcepts

-- labelled attachment score

data UDScore = UDScore {
  udScore          :: Double, --- redundant
  udMatching       :: Int, -- if the words are the same. 1 or 0 for a single sentence, sum for a corpus
  udTotalLength    :: Int, -- number of words
  udSamesLength    :: Int, -- number of words with matching (head,label)
  udPerfectMatch   :: Int  -- all words have match (head,label). 1 or 0 for single sentence, sum for corpus
  }
 deriving Show

type ScoringCriterion = UDWord -> UDWord -> Bool

agreeLAS g t = udHEAD g == udHEAD t && udDEPREL g == udDEPREL t
agreeUAS g t = udHEAD g == udHEAD t 

-- return the best candidate and its score
udSentenceScore :: ScoringCriterion -> UDSentence -> [UDSentence] -> (UDSentence,UDScore)
udSentenceScore agree gold testeds = (tested,score) where

  score = UDScore {
    udScore = fromIntegral (length sames) / fromIntegral (length alls),
    udMatching = areMatching,
    udTotalLength = length alls,
    udSamesLength = length sames,
    udPerfectMatch = if (length sames == length alls) then 1 else 0
    } 

  alls = udWordLines tested
  tested = maximumBy (\t u -> compare (length (samest t)) (length (samest u))) testeds
  sames = samest tested
  samest tsd = [() | (g,t) <- zip (udWordLines gold) (udWordLines tsd), agree g t]
  areMatching = if (map udFORM (udWordLines gold) == map udFORM (udWordLines tested)) then 1 else 0


-- test on corpus level: in the tested corpus, group together trees for the same sentence
udCorpusScore :: Bool -> ScoringCriterion -> [UDSentence] -> [UDSentence] -> UDScore
udCorpusScore isMicro agree golds tests = UDScore {
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
        map (uncurry (udSentenceScore agree)) (zip golds testgroups)
    numberalls  =  sum (map udTotalLength sentenceScores)
    numbersames =  sum (map udSamesLength sentenceScores)
    testgroups  = groupBy (\t u -> sent t == sent u) tests
    sent t = unwords $ map udFORM $ udWordLines t
