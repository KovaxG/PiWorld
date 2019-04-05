module Utils where

import Data.Char

type ID = Int

isID :: String -> Bool
isID = all isDigit

safeMax :: Ord a => [a] -> Maybe a
safeMax [] = Nothing
safeMax as = Just $ maximum as

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)

swap :: (a -> b -> c) -> (b -> a -> c)
swap f a b = f b a

lowerBound :: Ord a => a -> a -> a
lowerBound b a = if a < b then b else a

split :: Int -> [a] -> [[a]]
split n as
  | null as = []
  | otherwise = taken : split n rest
  where
    (taken, rest) = splitAt n as