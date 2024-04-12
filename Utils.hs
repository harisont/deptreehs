module Utils where

import Data.Char
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)), (<|))

strip :: String -> String
strip [] = []
strip (c:cs)
  | isSpace c = strip cs
  | otherwise = reverse $ strip' (reverse $ c:cs)
  where
    strip' [] = []
    strip' (c:cs)
      | isSpace c = strip' cs
      | otherwise = c:cs

getSeps :: Eq a => a -> [a] -> [[a]]
getSeps p xs = filter (not .null) getSeps'
  where
    getSeps' = case break (==p) xs of
      (c,_:xx) -> c : getSeps p xx
      (c,_) -> [c]

-- | Like `getSeps` but allows escaping the separator with backslash
getSepsEsc :: Char -> String -> [String]
getSepsEsc p = filter (not . null) . NonEmpty.toList . getSepsEsc'
  where
    getSepsEsc' [] = pure []
    getSepsEsc' ('\\' : c : s) = mapHead (c:) $ getSepsEsc' s
    getSepsEsc' (c : s) 
      | c == p = [] <| getSepsEsc' s
      | otherwise = mapHead (c:) $ getSepsEsc' s

    mapHead f ~(x :| xs) = f x :| xs

checkInList :: String -> [String] -> String -> [String]
checkInList desc xs x = 
  if x=="_" || S.member x xset
  then []
  else ["invalid " ++ desc ++ ": " ++ x]
 where
   xset = S.fromList xs

-- | String consisting of 10 'x' and '_' characters, used to specify which
-- CoNLL-U fields are to be printed when outputting reduced CoNLL-U like 
-- format. The pattern '"xxxx\_\_xx\_\_"', for instance, is used for producing
-- the simplified CoNNL-U format (ID, FORM, LEMMA, UPOS, HEAD and DEPREL)
-- used in the Computational Syntax course of the MLT programme at the 
-- University of Gothenburg
type PrintPattern = String