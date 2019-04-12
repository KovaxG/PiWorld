module GameTypes where

import Data.Char
import Data.List
import Data.Map
import Data.Maybe
import Data.Set
import System.Random

import Utils

type Location = (Int, Int)
type TickNr = Int
type Name = String

newtype UserName = UserName { getUserName :: String } deriving (Show, Eq)
newtype Password = Password String deriving (Show, Eq)

data User = User {
  uID :: ID,
  uName :: UserName,
  uPass ::  Password
}

instance Show User where
  show user = getUserName $ uName user

instance Eq User where
  u1 == u2 = uID u1 == uID u2

data Resource = Wood
              | Food
              | Stone
              deriving (Show, Eq, Ord)

type Inventory = Map Resource Double

emptyInventory :: Inventory
emptyInventory = Data.Map.empty

fromInventory :: Inventory -> Resource -> Double
fromInventory inv res = fromMaybe 0.0 $ Data.Map.lookup res inv

addResource :: Resource -> Double -> Inventory -> Inventory
addResource res qty inv =
  maybe notFound found $ Data.Map.lookup res inv
  where
    notFound = Data.Map.insert res qty inv
    found q = Data.Map.insert res (q + qty) inv

data Job = Civilian
         | Woodcutter
         | Explorer
         | Hunter
         | StoneGatherer
         deriving (Show, Read, Eq)

newtype HungerMeter = HungerMeter { getHunger :: Double } deriving (Show)
newtype HealthMeter = HealthMeter { getHealth :: Double } deriving (Show)

data Person = Person {
  pID :: ID,
  pName :: Name,
  pJob :: Job,
  pHunger :: HungerMeter,
  pHealth :: HealthMeter
} deriving (Show)

instance Eq Person where
  p1 == p2 = pID p1 == pID p2

data Event = NewVillage VillageName Location User [Name]
           | Tick
           | ChangeJobOfVillager ID Job
           deriving (Show)

newtype VillageName = VillageName { getVillageName :: String } deriving (Show)

data Village = Village {
  vID :: ID,
  vUser :: User,
  vCreated :: TickNr,
  vName :: VillageName,
  vLocation :: Location,
  vInventory :: Inventory,
  vVillagers :: [Person],
  vDiscoveredTerrain :: Set Terrain
} deriving (Show)

showVillage v = getVillageName (vName v) ++ " " ++ show (vLocation v)

data Terrain = Grass
             | Forest
             | RockyHill
             deriving (Show, Eq, Ord)

data GameState = GameState {
  gTickNr :: TickNr,
  gSize :: (Int, Int),
  gTerrain :: Data.Map.Map Location Terrain,
  gVillages :: [Village],
  gPersonID :: Int
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
    gameMap = gTerrain gameState
    draw Grass = ' '
    draw Forest = '|'
    draw RockyHill = '*'
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
