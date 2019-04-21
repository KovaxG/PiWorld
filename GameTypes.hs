{-
  This is the source code for the web real time stratgy game, PiWorld.
  It was written to be a hobby project to see how a big project looks
  like in Haskell, and also to learn about web development.
  You are free to look at the code and use it as you see fit, as long
  as it is not for profit. If it is please contact the author, KovaxG
  on gitthub.
  Author(s): KovaxG
-}
{-# LANGUAGE BangPatterns #-}
module GameTypes where

import Data.Char
import Data.List as List hiding (lookup)
import Data.Map as Map
import Data.Maybe
import Data.Set
import Prelude hiding (lookup)
import System.Random

import Utils

data GameState = GameState {
  gTickNr :: !TickNr,
  gSize :: !(Int, Int),
  gTerrain :: !(Map Location Terrain),
  gVillages :: ![Village],
  gPersonID :: !Int
} deriving (Show)

type TickNr = Int
type Location = (Int, Int)

data Terrain = Grass
             | Forest
             | RockyHill
             deriving (Show, Eq, Ord)

data Village = Village {
  vID :: !ID,
  vUser :: !User,
  vCreated :: !TickNr,
  vName :: !VillageName,
  vLocation :: !Location,
  vInventory :: !Inventory,
  vVillagers :: ![Person],
  vBuildings :: ![Building],
  vDiscoveredTerrain :: !(Set Terrain)
} deriving (Show)

data User = User {
  uID :: ID,
  uName :: UserName,
  uPass ::  Password
}

instance Show User where
  show user = getUserName $ uName user

instance Eq User where
  u1 == u2 = uID u1 == uID u2

newtype UserName = UserName { getUserName :: String } deriving (Show, Eq)
newtype Password = Password String deriving (Show, Eq)

newtype VillageName = VillageName { getVillageName :: String } deriving (Show)

type Inventory = Map Resource Double

data Person = Person {
  pID :: !ID,
  pName :: !Name,
  pJob :: !Job,
  pHunger :: !HungerMeter,
  pHealth :: !HealthMeter,
  pTool :: !Tool
} deriving (Show)

instance Eq Person where
  p1 == p2 = pID p1 == pID p2

type Name = String

data Job = Civilian
         | Woodcutter
         | Explorer
         | Hunter
         | StoneGatherer
         deriving (Show, Read, Eq)

newtype HungerMeter = HungerMeter { getHunger :: Double } deriving (Show, Eq, Ord)

toHungerMeter :: Double -> HungerMeter
toHungerMeter = HungerMeter . saturate 0 100

newtype HealthMeter = HealthMeter { getHealth :: Double } deriving (Show, Eq, Ord)

toHealthMeter :: Double -> HealthMeter
toHealthMeter = HealthMeter . saturate 0 100

data Tool = EmptyHanded
          | ChippedStone
          | StoneTool
          | CopperTool
          | IronTool
          | SteelTool
          deriving (Show)

data BuildingSize = Tiny | Small | Normal | Big deriving (Show)
data Building = Building BuildingSize deriving (Show)

data Event = NewVillage VillageName Location User [Name]
           | Tick
           | ChangeJobOfVillager ID Job
           deriving (Show)

data Resource = Wood
              | Food
              | Stone
              deriving (Show, Eq, Ord)

emptyInventory :: Inventory
emptyInventory = Map.empty

fromInventory :: Inventory -> Resource -> Double
fromInventory inv res = fromMaybe 0.0 $ lookup res inv

updateResource :: Inventory -> Resource -> Double -> Inventory
updateResource inv res qty = Map.insert res qty inv

addResource :: Resource -> Double -> Inventory -> Inventory
addResource res qty inv =
  maybe notFound found $ lookup res inv
  where
    notFound = Map.insert res qty inv
    found q = Map.insert res (q + qty) inv

toolFactor :: Tool -> Double
toolFactor t = case t of
  EmptyHanded -> 1.0
  ChippedStone -> 2.0
  StoneTool -> 3.0
  CopperTool -> 4.0
  IronTool -> 4.5
  SteelTool -> 4.9

showMap :: GameState -> String
showMap gameState =
  concatMap (++"\n")
    $ Utils.split x
    [
      putCity (i, j)
      $ draw
      $ fromJust
      $ lookup (i, j) (gTerrain gameState)
      | i <- [1 .. x], j <- [1 .. y]
    ]
  where
    (x, y) = gSize gameState
    draw t = case t of
      Grass -> ' '
      Forest -> '|'
      RockyHill -> '*'
    putCity loc c =
      if any (==loc) $ fmap vLocation (gVillages gameState)
      then 'O'
      else c

villagesOf :: GameState -> User -> [Village]
villagesOf gameState user = List.filter ((== user) . vUser) $ gVillages gameState

getVillage :: GameState -> ID -> Maybe Village
getVillage gameState id = find ((==id) . vID) . gVillages $ gameState

nameGenerator :: IO Name
nameGenerator = do
  nr <- lowerBound 2 . flip mod 5 <$> randomIO :: IO Int
  result <- concat <$> traverse getSz [1 .. nr]
  return $ capitalise result
  where
    syllables = ["ga", "la","gog", "mo", "ra", "la", "pet", "ki", "hu", "ri", "ro", "er"]
    capitalise (c:rest) = toUpper c : rest
    getSz _ = do
      index <- flip mod (length syllables) <$> randomIO
      return $ syllables !! index
