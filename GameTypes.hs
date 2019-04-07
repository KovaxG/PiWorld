module GameTypes where

import Data.Char
import Data.List
import Data.Map
import Data.Maybe
import System.Random

import Utils

type Location = (Int, Int)
type TickNr = Int
type Name = String

type UserName = String
type Password = String

data User = User {
  uID :: ID,
  uName :: UserName,
  uPass ::  Password
}

instance Show User where
  show user = uName user

instance Eq User where
  u1 == u2 = uID u1 == uID u2

data Resource = Wood deriving (Show, Eq, Ord)

type Inventory = Map Resource Int

emptyInventory :: Inventory
emptyInventory = Data.Map.empty

fromInventory :: Inventory -> Resource -> Int
fromInventory inv res = fromMaybe 0 $ Data.Map.lookup res inv

addResource :: Inventory -> Resource -> Int -> Inventory
addResource inv res qty =
  maybe notFound found $ Data.Map.lookup res inv
  where
    notFound = Data.Map.insert res qty inv
    found q = Data.Map.insert res (q + qty) inv

data Job = Civilian | Woodcutter deriving (Show, Eq)

data Person = Person {
  pID :: ID,
  pName :: Name,
  pJob :: Job
}

instance Show Person where
  show = pName

data Event = NewVillage Name Location User [Name]
           | Tick
           deriving (Show)

data Village = Village {
  vID :: ID,
  vUser :: User,
  vCreated :: TickNr,
  vName :: Name,
  vLocation :: Location,
  vInventory :: Inventory,
  vVillagers :: [Person]
} deriving (Show)

showVillage v = vName v ++ " " ++ show (vLocation v)

data Tile = Grass
          | Forest
          deriving (Show, Eq)

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
