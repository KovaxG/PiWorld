module GameLogic (
  newGameStateVar,
  gameLoop
) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad.State
import Data.List
import Data.Map hiding (foldl, (\\), filter, null, take)
import Data.Maybe
import Data.Set hiding (foldl, (\\), filter, null, take)
import Debug.Trace
import System.Time.Extra

import GameTypes
import MessageQueue
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
    f x y
      | mod (x * y) 13 < 4 = Forest
      | mod (x * y) 21 < 2 = RockyHill
      | otherwise = Grass

gameLoop :: MessageQueue Event -> MVar GameState -> IO ()
gameLoop queueVar gameStateVar = do
  threadDelay 1000000 -- TODO account for time taken to update tick
  events <- popMessages queueVar
  (dt, gameState) <- duration(updateGameState gameStateVar events)
  putStrLn $ showDuration dt
  --putStrLn $ show event
  --putStrLn $ show $ gTickNr gameState
  gameLoop queueVar gameStateVar

ticksPerDay :: Int
ticksPerDay = 24 * 60 * 60

perDayToPerTick :: Int -> Double
perDayToPerTick a = fromIntegral a / fromIntegral ticksPerDay

updateGameState :: MVar GameState -> [Event] -> IO GameState
updateGameState gameStateVar events' = do
  let events = Tick : events'
  gameState <- takeMVar gameStateVar
  let newState = execState (traverse tick events) gameState
  --putStrLn $ show $ vInventory $ head $ gVillages newState
  --putStrLn $ show $ vDiscoveredTerrain $ head $ gVillages newState
  --putStrLn $ show newState
  --deepseq newState $ putMVar gameStateVar newState -- TODO to force eval
  putMVar gameStateVar newState
  return newState

tick :: Event -> State GameState ()
tick Tick = do
  updateTickNr
  updateAllVillagers
tick (NewVillage name location user names) = do
  addNewVillage name location user names
tick (ChangeJobOfVillager id newJob) = do
  villages <- gets gVillages
  let newVillages = fmap updateVillagerJob villages
  modify $ \s -> s { gVillages = newVillages }
  where
    updateVillagerJob village =
      let newVills = replacefw ((==id) . pID) (\p -> p { pJob = newJob } ) (vVillagers village)
      in village { vVillagers = newVills }

addNewVillage :: VillageName -> Location -> User -> [Name] -> State GameState ()
addNewVillage name location user names = do
  curTick <- gets gTickNr
  villages <- gets gVillages
  lastId <- getLastId
  personId <- gets gPersonID
  let newPeople = zipWith (\n i -> Person i n Civilian (HungerMeter 100.0) (HealthMeter 100.0) EmptyHanded) names [personId ..]
  let newVillage = Village {
    vID = (lastId + 1),
    vUser = user,
    vCreated = curTick,
    vName = name,
    vLocation = location,
    vInventory = addResource Food 16.0 emptyInventory,
    vVillagers = newPeople,
    vBuildings = [Building Normal],
    vDiscoveredTerrain = Data.Set.empty
  }
  modify $ \s -> s { gVillages = newVillage : villages }

  let newPersonId = maximum $ fmap pID newPeople
  modify $ \s -> s { gPersonID = newPersonId + 1}

updateAllVillagers :: State GameState ()
updateAllVillagers = do
  villages <- gets gVillages
  terrain <- gets gTerrain
  let newVillages = execState (updateVillagers terrain) <$> villages
  modify $ \s -> s { gVillages = newVillages }

updateVillagers :: Data.Map.Map Location Terrain -> State Village ()
updateVillagers terrain = do
  gathererLogic terrain
  explorerLogic terrain
  needsLogic

needsLogic :: State Village ()
needsLogic = do
  villagers <- gets vVillagers
  inv <- gets vInventory
  let food = fromInventory inv Food
  let (newFood, newVillagers) = foldl rule (food, []) villagers
  modify $ \v -> v { vVillagers = newVillagers }
  let newInv = updateResource inv Food newFood
  modify $ \v -> v { vInventory = newInv }
  where
    eatingRatePerDay = 3
    eatingRatePerTick = perDayToPerTick eatingRatePerDay

    hungerRecoveryRatePerDay = 300
    hungerRecoveryRatePerTick = perDayToPerTick hungerRecoveryRatePerDay

    hungerRatePerDay = 100
    hungerRatePerTick = perDayToPerTick hungerRatePerDay

    healingRatePerDay = 10
    healingRatePerTick = perDayToPerTick healingRatePerDay

    damageRatePerDay = 20
    damageRatePerTick = perDayToPerTick damageRatePerDay

    rule :: (Double, [Person]) -> Person -> (Double, [Person])
    rule (food, vs) v
      | food >= eatingRatePerTick && pHunger v >= (HungerMeter 100.0) =
        let health = getHealth $ pHealth v
            v' = v { pHealth = toHealthMeter (health + healingRatePerTick) }
        in (food - eatingRatePerTick, v' : vs)
      | food >= eatingRatePerTick =
        let hunger = getHunger $ pHunger v
            v' = v { pHunger = toHungerMeter (hunger + hungerRecoveryRatePerTick) }
        in (food - eatingRatePerTick, v' : vs)
      | pHunger v > (HungerMeter 0.0) =
        let hunger = getHunger $ pHunger v
            v' = v { pHunger =  toHungerMeter (hunger - hungerRatePerTick) }
        in (food, v' : vs)
      | pHealth v > (HealthMeter 0.0) =
        let health = getHealth $ pHealth v
            v' = v { pHealth = toHealthMeter (health - damageRatePerTick) }
        in (food, v' : vs)
      | otherwise = (food, vs)

gathererLogic :: Data.Map.Map Location Terrain -> State Village ()
gathererLogic terrain = do
  villagers <- gets vVillagers
  location <- gets vLocation
  let resources = getTerrain terrain (areaOf 1 location)

  inventory <- gets vInventory
  let newInventory = execState (updateInventory resources villagers) inventory
  modify $ \v -> v { vInventory = newInventory }

explorerLogic :: Data.Map.Map Location Terrain -> State Village ()
explorerLogic terrain = do
  villagers <- gets vVillagers
  let explorerNr = count Explorer $ fmap pJob villagers

  location <- gets vLocation
  let accessibleTerrainTypes = nub $ getTerrain terrain (areaOf 1 location)

  discoveredTerrainTypes <- gets vDiscoveredTerrain
  let undiscoveredLocations = accessibleTerrainTypes \\ (Data.Set.toList discoveredTerrainTypes)
  let currentDiscoveredTerrainType = take explorerNr undiscoveredLocations
  let newDiscoveredLocations = foldl (\s a -> Data.Set.insert a s) discoveredTerrainTypes currentDiscoveredTerrainType
  modify $ \v -> v { vDiscoveredTerrain = newDiscoveredLocations }

updateInventory :: [Terrain] -> [Person] -> State Inventory ()
updateInventory terrains ps = do
  modify $ addResource Wood $ gatherFrom [Forest] Woodcutter
  modify $ addResource Stone $ gatherFrom [RockyHill] StoneGatherer
  modify $ addResource Food $ gatherFrom [Grass, Forest] Hunter
  where
    gatherFrom :: [Terrain] -> Job -> Double
    gatherFrom ts j = gatherRatePerTick * if null $ intersect ts terrains then 0 else gatherRate
      where
        factor = sum $ fmap (toolFactor . pTool) $ filter ((==j) . pJob) ps
        gatherRate = (*) factor $ fromIntegral $ count j $ filter (==j) $ fmap pJob ps

    gatherRatePerDay = 10
    gatherRatePerTick = perDayToPerTick gatherRatePerDay


areaOf :: Int -> Location -> [Location]
areaOf n (x, y) = [(i, j) | i <- [x-n.. x+n], j <- [y-n .. y+n]]

updateTickNr :: State GameState ()
updateTickNr = do
  curTick <- gets gTickNr
  modify (\s -> s { gTickNr = curTick + 1 } )

getLastId :: State GameState ID
getLastId = gets $ fromMaybe 0 . safeMax . fmap vID . gVillages

getTerrain :: Map Location Terrain -> [Location] -> [Terrain]
getTerrain t = catMaybes . fmap (\loc -> Data.Map.lookup loc t)
