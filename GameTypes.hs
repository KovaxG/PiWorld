module GameTypes where

import Data.Char
import Data.List
import System.Random

import ServerTypes
import Utils

type Location = (Int, Int)
type TickNr = Int
type Name = String

data Person = Person {
  pID :: ID,
  pName :: Name
}

instance Show Person where
  show = pName

data Event = NewVillage Name Location User [Name] deriving (Show)

data Village = Village {
  vID :: ID,
  vUser :: User,
  vCreated :: TickNr,
  vName :: Name,
  vLocation :: Location,
  vVillagers :: [Person]
} deriving (Show)

showVillage v = vName v ++ " " ++ show (vLocation v)

data GameState = GameState {
  gTickNr :: TickNr,
  gVillages :: [Village]
} deriving (Show)

villagesOf :: GameState -> User -> [Village]
villagesOf gameState user = filter ((== user) . vUser)  $ gVillages gameState

getVillage :: GameState -> ID -> Maybe Village
getVillage gameState id = find ((==id) . vID) . gVillages $ gameState

nameGenerator :: IO Name
nameGenerator = do
  nr <- lowerBound 2 . swap mod 5 <$> randomIO :: IO Int
  result <- concat <$> traverse getSz [1 .. nr]
  return $ capitalise result
  where
    capitalise (c:rest) = toUpper c : rest
    getSz _ = do
      index <- swap mod (length sz) <$> randomIO
      return $ sz !! index
    sz = ["ga", "la","gog", "mo", "ra", "la", "pet", "ki", "hu", "ri", "ro", "er"]
