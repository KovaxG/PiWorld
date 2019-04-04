module GameTypes where

import Data.Char
import Data.List
import Data.Map
import Data.Maybe
import System.Random

import ServerTypes
import Utils

type Location = (Int, Int)
type TickNr = Int
type Name = String


data Inventory = Inventory {
  iWood :: Int
} deriving (Show)

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

data Tile = Grass
          | Forest
          deriving (Show)

data GameState = GameState {
  gTickNr :: TickNr,
  gSize :: (Int, Int),
  gTiles :: Data.Map.Map Location Tile,
  gVillages :: [Village]
} deriving (Show)

showMap :: GameState -> String
showMap gameState =
  concat
    $ Data.List.map (++"\n")
    $ Utils.split x
    [putCity (i, j) $ draw $ fromJust $ Data.Map.lookup (i, j) gameMap
      | i <- [1 .. x], j <- [1 .. y]]
  where
    (x, y) = gSize gameState
    gameMap = gTiles gameState
    draw Grass = '.'
    draw Forest = '|'
    putCity loc c =
      if any (==loc) $ Data.List.map vLocation (gVillages gameState)
      then 'O'
      else c


villagesOf :: GameState -> User -> [Village]
villagesOf gameState user =
  Data.List.filter ((== user) . vUser)  $ gVillages gameState

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
