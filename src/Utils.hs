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

import Control.Monad.State
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

member :: Eq a => a -> [(a,b)] -> Bool
member a = elem a . fmap fst

mapWithState :: Traversable t => t a -> b -> (a -> b -> (c, b)) -> (t c, b)
mapWithState as b f = runState (traverse (state . f) as) b

interpolateString :: [(String, String)] -> String -> String
interpolateString dl =
  concat . fmap (\w -> if firstElemIs '$' w then replace w else w) . splitVariables '$'
  where
    replace (_:w) = (++end)  $ fromMaybe "Nothing" $ lookup w' dl
      where (w', end) = span isAlphaNum w

splitVariables :: Char -> String -> [String]
splitVariables c s
  | cont == "" = [s]
  | otherwise = start : (c : var) : splitVariables c rest
  where
    (var, rest) = span isAlphaNum $ tail cont
    (start, cont) = span (/=c) s

firstElemIs :: (Eq a) => a -> [a] -> Bool
firstElemIs a [] = False
firstElemIs a (x:_) = a == x

sameElements :: (Eq a) => [a] -> [a] -> Bool
sameElements [] [] = True
sameElements [] _ = False
sameElements _ [] = False
sameElements a b = null $ a \\ b

interpolateList :: [([String], [[String]])] -> String -> String
interpolateList dl = unlines . fmap processLine . lines
  where
    processLine :: String -> String
    processLine line
      | elem '#' line = (=<<) ((++"\n") . concat . merge list) $ fromJust $ flip lookup dl vars
      | otherwise = line
      where
        vars = map tail $ filter (firstElemIs '#') list
        list = splitVariables '#' line

    merge :: [String] -> [String] -> [String]
    merge as bs = fst $ foldl rule ([], bs) as
      where
        rule (acc, bs) a
          | firstElemIs '#' a = (acc ++ [head bs], tail bs)
          | otherwise = (acc ++ [a], bs)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith x s = take (length x) s == x
