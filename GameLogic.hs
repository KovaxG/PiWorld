module GameLogic (
  newGameStateVar,
  updateGameState,
) where

import Control.Concurrent.MVar
import Control.Monad.State
import Data.List
import Data.Map
import Data.Maybe
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
    gTiles = fromList tiles,
    gVillages = []
  }
  where
    width = 10
    height = 10
    tiles = [((x, y), f x y) | x <- [1 .. width], y <- [1..height]]
    f x y = if mod (x * y) 13 < 4
      then Forest
      else Grass

updateGameState :: MVar GameState -> [Event] -> IO GameState
updateGameState gameStateVar events' = do
  let events = events' ++ [Tick]
  gameState <- takeMVar gameStateVar
  let newState = execState (traverse tick events) gameState
  --putStrLn $ show $ vInventory $ head $ gVillages newState
  putMVar gameStateVar newState
  return newState

tick :: Event -> State GameState ()
tick Tick = do
  updateVillages
  updateTickNr
tick (NewVillage name location user names) = do
  addNewVillage name location user names

addNewVillage :: Name -> Location -> User -> [Name] -> State GameState ()
addNewVillage name location user names = do
  curTick <- gets gTickNr
  villages <- gets gVillages
  lastId <- getLastId
  let newVillage = Village {
    vID = (lastId + 1),
    vUser = user,
    vCreated = curTick,
    vName = name,
    vLocation = location,
    vInventory = emptyInventory,
    vVillagers = zipWith (\n i -> Person i n Civilian) names [1..]
  }
  modify $ \s -> s { gVillages = newVillage : villages }

updateVillages :: State GameState ()
updateVillages = do
  villages <- gets gVillages
  terrain <- gets gTiles
  let newVillages = execState (updateVillage terrain) <$> villages
  modify $ \s -> s { gVillages = newVillages }

updateVillage :: Data.Map.Map Location Tile -> State Village ()
updateVillage terrain = do
  location <- gets vLocation
  villagers <- gets vVillagers
  let jobs = pJob <$> villagers
  let resources = catMaybes $ (fromTerrain terrain) <$> area 1 location
  inventory <- gets vInventory
  let newInventory = execState (updateInventory resources jobs) inventory
  modify $ \v -> v { vInventory = newInventory }

updateInventory :: [Tile] -> [Job] -> State Inventory ()
updateInventory ts js = do
  modify $ addResource Wood woodNr
  modify $ addResource Food foodNr
  modify $ addResource Stone stoneNr
  where
    woodNr = if elem Forest ts then wood else 0
    wood = length $ Data.List.filter (==Woodcutter) js
    foodNr = length $ Data.List.filter(==Hunter) js
    stoneNr = if elem RockyHill ts then stone else 0
    stone = length $ Data.List.filter (==StoneGatherer) js

area :: Int -> Location -> [Location]
area n (x, y) = [(i, j) | i <- [x-n.. x+n], j <- [y-n .. y+n]]

updateTickNr :: State GameState ()
updateTickNr = do
  curTick <- gets gTickNr
  modify (\s -> s { gTickNr = curTick + 1 } )

getLastId :: State GameState ID
getLastId = gets $ fromMaybe 0 . safeMax . fmap vID . gVillages

fromTerrain :: Map Location Tile -> Location -> Maybe Tile
fromTerrain t loc = Data.Map.lookup loc t
