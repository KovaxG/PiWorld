{-
  This is the source code for the web real time stratgy game, PiWorld.
  It was written to be a hobby project to see how a big project looks
  like in Haskell, and also to learn about web development.
  You are free to look at the code and use it as you see fit, as long
  as it is not for profit. If it is please contact the author, KovaxG
  on gitthub.
  Author(s): KovaxG
-}
module Utils where

import Data.Char
import Data.List
import Data.Maybe

type ID = Int

isID :: String -> Bool
isID = all isDigit

safeMax :: Ord a => [a] -> Maybe a
safeMax [] = Nothing
safeMax as = Just $ maximum as

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)

lowerBound :: Ord a => a -> a -> a
lowerBound b a = if a < b then b else a

split :: Int -> [a] -> [[a]]
split n as
  | null as = []
  | otherwise = taken : split n rest
  where
    (taken, rest) = splitAt n as

count :: Eq a => a -> [a] -> Int
count a = length . filter (==a)

maxBy :: Ord b => (a -> b) -> [a] -> a
maxBy f = maximumBy (\a b -> f a `compare` f b)

safeRead :: Read a => String -> Maybe a
safeRead = listToMaybe . fmap fst . reads

replacefw :: Eq a => (a -> Bool) -> (a -> a) -> [a] -> [a]
replacefw f g = fmap (\a -> if f a then g a else a)

replacef :: Eq a => (a -> Bool) -> a -> [a] -> [a]
replacef f b = fmap (\a -> if f a then b else a)

replace :: Eq a => a -> a -> [a] -> [a]
replace aa b = fmap (\a -> if aa == a then b else a)

saturate :: Ord a => a -> a -> a -> a
saturate lower upper value
  | value >= upper = upper
  | value <= lower = lower
  | otherwise      = value
