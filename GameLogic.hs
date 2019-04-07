{-# LANGUAGE BangPatterns #-}

module GameLogic (
  newGameStateVar,
  updateGameState,
) where

import Control.Concurrent.MVar
import Control.Monad.State
import Data.List
import Data.Map hiding (foldl, (\\))
import Data.Maybe
import Data.Set hiding (foldl, (\\))
import Debug.Trace

import GameTypes
import ServerTypes
import Utils

newGameStateVar :: IO (MVar GameState)
newGameStateVar = newMVar newState

newState :: GameState
newState =
  GameState {
    gTickNr = 0,
    gSize = (width, height),
    gTerrain = Data.Map.fromList sections,
    gVillages = [],
    gPersonID = 0
  }
  where
    width = 10
    height = 10
    sections = [((x, y), f x y) | x <- [1 .. width], y <- [1..height]]
    f x y = if mod (x * y) 13 < 4
      then Forest
      else Grass

updateGameState :: MVar GameState -> [Event] -> IO GameState
updateGameState gameStateVar events' = do
  let events = Tick : events'
  gameState <- takeMVar gameStateVar
  let newState = execState (traverse tick events) gameState
  --putStrLn $ show $ vInventory $ head $ gVillages newState
  --putStrLn $ show $ vDiscoveredTerrain $ head $ gVillages newState
  --putStrLn $ show newState
  putMVar gameStateVar newState
  return newState

tick :: Event -> State GameState ()
tick Tick = do
  updateTickNr
  updateVillages
tick (NewVillage name location user names) = do
  addNewVillage name location user names

addNewVillage :: Name -> Location -> User -> [Name] -> State GameState ()
addNewVillage name location user names = do
  curTick <- gets gTickNr
  villages <- gets gVillages
  lastId <- getLastId
  personId <- gets gPersonID
  let newPeople = zipWith (\n i -> Person i n Civilian) names [personId ..]
  let newVillage = Village {
    vID = (lastId + 1),
    vUser = user,
    vCreated = curTick,
    vName = name,
    vLocation = location,
    vInventory = emptyInventory,
    vVillagers = newPeople,
    vDiscoveredTerrain = Data.Set.empty
  }
  modify $ \s -> s { gVillages = newVillage : villages }

  let newPersonId = maximum $ pID <$>  newPeople
  modify $ \s -> s { gPersonID = newPersonId + 1}

updateVillages :: State GameState ()
updateVillages = do
  villages <- gets gVillages
  terrain <- gets gTerrain
  let newVillages = execState (updateVillage terrain) <$> villages
  modify $ \s -> s { gVillages = newVillages }

updateVillage :: Data.Map.Map Location Terrain -> State Village ()
updateVillage terrain = do
  location <- gets vLocation
  villagers <- gets vVillagers
  let jobs = pJob <$> villagers
  let usableLocations = area 1 location
  let resources = catMaybes $ (fromTerrain terrain) <$> usableLocations
  inventory <- gets vInventory
  let newInventory = execState (updateInventory resources jobs) inventory
  modify $ \v -> v { vInventory = newInventory }

  let explorerNr = count Explorer jobs
  !discoveredLocations <- gets vDiscoveredTerrain
  let accessibleTerrain = nub $ catMaybes $ (fromTerrain terrain) <$> usableLocations
  let undiscoveredLocations = accessibleTerrain \\ (Data.Set.toList discoveredLocations)
  let currentDiscoveredLocation = take explorerNr undiscoveredLocations
  let newDiscoveredLocations = foldl (\s a -> Data.Set.insert a s) discoveredLocations currentDiscoveredLocation
  modify $ \v -> v { vDiscoveredTerrain = newDiscoveredLocations }



updateInventory :: [Terrain] -> [Job] -> State Inventory ()
updateInventory ts js = do
  modify $ addResource Wood woodNr
  modify $ addResource Food foodNr
  modify $ addResource Stone stoneNr
  where
    woodNr = if elem Forest ts then wood else 0
    wood = count Woodcutter js
    foodNr = count Hunter js
    stoneNr = if elem RockyHill ts then stone else 0
    stone = count StoneGatherer js

area :: Int -> Location -> [Location]
area n (x, y) = [(i, j) | i <- [x-n.. x+n], j <- [y-n .. y+n]]

updateTickNr :: State GameState ()
updateTickNr = do
  curTick <- gets gTickNr
  modify (\s -> s { gTickNr = curTick + 1 } )

getLastId :: State GameState ID
getLastId = gets $ fromMaybe 0 . safeMax . fmap vID . gVillages

fromTerrain :: Map Location Terrain -> Location -> Maybe Terrain
fromTerrain t loc = Data.Map.lookup loc t
