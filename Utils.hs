{-|
Module      : Utils
Description : Misc functions that might be in some other library but who knows
License     : BSD-2
Maintainer  : arianna.masciolini@gu.se
Stability   : experimental
Portability : POSIX

Misc functions that might be in some other library but who knows
-}

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
  if x == "_" || S.member x xset
  then []
  else ["invalid " ++ desc ++ ": " ++ x]
 where
   xset = S.fromList xs

sublists :: Int -> [a] -> [[a]]
sublists n xs = case (n,xs) of
  (0,_)  -> [[]]
  (_,[]) -> []
  (_,x:xx) -> [x:ys | ys <- sublists (n-1) xx] ++ sublists n xx

segments :: Int -> [a] -> [[a]]
segments n xs =
  let lxs = length xs in
  if n <= lxs then [take n (drop m xs) | m <- [0..lxs-n]]
  else []