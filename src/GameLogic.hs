{-
  This is the source code for the web real time stratgy game, PiWorld.
  It was written to be a hobby project to see how a big project looks
  like in Haskell, and also to learn about web development.
  You are free to look at the code and use it as you see fit, as long
  as it is not for profit. If it is please contact the author, KovaxG
  on gitthub.
  Author(s): KovaxG
-}
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
import System.Random.Shuffle

import GameTypes
import MessageQueue
import ServerTypes
import Utils
import VillageAlgebra

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
  newState <- execStateT (traverse tick events) gameState
  --putStrLn $ show $ vInventory $ head $ gVillages newState
  --putStrLn $ show $ vDiscoveredTerrain $ head $ gVillages newState
  --putStrLn $ show newState
  --deepseq newState $ putMVar gameStateVar newState -- TODO to force eval
  putMVar gameStateVar newState
  return newState

tick :: Event -> StateT GameState IO ()
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

addNewVillage :: VillageName -> Location -> User -> [Name] -> StateT GameState IO ()
addNewVillage name location user names = do
  curTick <- gets gTickNr
  villages <- gets gVillages
  lastId <- getLastId
  personId <- gets gPersonID
  let newPeople = zipWith (\n i -> Person i n Civilian (HungerMeter 100.0) (HealthMeter 100.0) EmptyHanded Ok) names [personId ..]
  let newVillage = Village {
    vID = (lastId + 1),
    vUser = user,
    vCreated = curTick,
    vName = name,
    vLocation = location,
    vInventory = addResource Food 16.0 emptyInventory,
    vVillagers = newPeople,
    vBuildings = [Building Normal],
    vDiscoveredLocations = [location],
    vDiscoveringLocation = Nothing
  }
  modify $ \s -> s { gVillages = newVillage : villages }

  let newPersonId = maximum $ fmap pID newPeople
  modify $ \s -> s { gPersonID = newPersonId + 1}

updateAllVillagers :: StateT GameState IO ()
updateAllVillagers = do
  villages <- gets gVillages
  terrain <- gets gTerrain
  newVillages <- liftIO $ traverse (execStateT (updateVillagers terrain)) villages
  modify $ \s -> s { gVillages = newVillages }

updateVillagers :: Data.Map.Map Location Terrain -> StateT Village IO ()
updateVillagers terrain = do
  gathererLogic terrain
  explorerLogic terrain
  needsLogic

needsLogic :: StateT Village IO ()
needsLogic = do
  villagers <- gets vVillagers
  inv <- gets vInventory
  let food = fromInventory inv Food
  let (villagersWithUpdatedStatuses, newFood) = updateStatuses villagers food
  let newVillagers = fmap updateAccordingToStatus villagersWithUpdatedStatuses

  setVillagers newVillagers
  modifyInventory (\inv -> updateResource inv Food newFood)
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

    updateAccordingToStatus :: Person -> Person
    updateAccordingToStatus p =
      case pStatus p of
        Ok -> p
        Healing -> p { pHealth = toHealthMeter (health + healingRatePerTick) }
        GettingLessHungry -> p { pHunger = toHungerMeter (hunger + hungerRecoveryRatePerTick) }
        GettingHungry -> p { pHunger =  toHungerMeter (hunger - hungerRatePerTick) }
        Starving -> p { pHealth = toHealthMeter (health - damageRatePerTick) }
        Dead -> p
      where
        hunger = getHunger $ pHunger p
        health = getHealth $ pHealth p

    updateStatuses :: [Person] -> Double -> ([Person], Double)
    updateStatuses ps foodStock = mapWithState ps foodStock $ \p food ->
      let (status', food') = updateStatus (pStatus p) food (pHunger p) (pHealth p)
      in ( p { pStatus = status' } , food')

    updateStatus :: Status -> Double -> HungerMeter -> HealthMeter -> (Status, Double)
    updateStatus Ok food _ _
      | food >= eatingRatePerTick = (Ok, food - eatingRatePerTick)
      | otherwise = (GettingHungry, 0.0)
    updateStatus GettingHungry food hunger _
      | food >= eatingRatePerTick = (GettingLessHungry, food - eatingRatePerTick)
      | hunger == (HungerMeter 0.0) = (Starving, 0.0)
      | otherwise = (GettingHungry, 0.0)
    updateStatus GettingLessHungry food hunger health
      | food < eatingRatePerTick = (GettingHungry, 0.0)
      | hunger == (HungerMeter 100.0) && health < (HealthMeter 100.0) = (Healing, food - eatingRatePerTick)
      | hunger == (HungerMeter 100.0) = (Ok, food - eatingRatePerTick)
      | otherwise = (GettingLessHungry, food - eatingRatePerTick)
    updateStatus Starving food hunger health
      | food >= eatingRatePerTick = (GettingLessHungry, food - eatingRatePerTick)
      | health == (HealthMeter 0.0) = (Dead, 0.0)
      | otherwise = (Starving, 0.0)
    updateStatus Healing food _ health
      | food < eatingRatePerTick = (GettingHungry, 0.0)
      | health == (HealthMeter 100.0) = (Ok, food - eatingRatePerTick)
      | otherwise = (Healing, food - eatingRatePerTick)
    updateStatus Dead food _ _ = (Dead, food)

gathererLogic :: Data.Map.Map Location Terrain -> StateT Village IO ()
gathererLogic terrain = do
  villagers <- gets vVillagers
  location <- gets vLocation
  let resources = getTerrain terrain (aroundLocation 1 location)

  inventory <- gets vInventory
  let newInventory = execState (updateInventory resources villagers) inventory
  setInventory newInventory

explorerLogic :: Data.Map.Map Location Terrain -> StateT Village IO ()
explorerLogic terrain = do
  villagers <- gets vVillagers
  let explorerNr = countf (jobIs Explorer) villagers

  if explorerNr > 0
  then do
    exploringLocation <- gets vDiscoveringLocation
    maybe selectNewLocationToExplore (continueExploringLocation explorerNr) exploringLocation
  else return ()
  where
    selectNewLocationToExplore :: StateT Village IO ()
    selectNewLocationToExplore = do
      discoveredLocations <- gets vDiscoveredLocations
      let accessibleLocations = getAccessibleLocations discoveredLocations
      selectedLocation <- liftIO $ safeHead <$> shuffleM accessibleLocations
      maybe noLocationAvailable startNewDiscoveringLocation selectedLocation
      where
        noLocationAvailable = return ()

    continueExploringLocation :: Int -> (Location, Percent) -> StateT Village IO ()
    continueExploringLocation explorerNr (exploringLocation, percentExplored) = do
      let rate = explorationRate * fromIntegral explorerNr
      let percentExplored' = percentExplored + rate
      if percentExplored' >= 100.0
      then do
        clearDiscoveringLocation
        addDiscoveredLocation exploringLocation
      else
        updatePercentDiscoveringLocation percentExplored'

    explorationRate :: Double
    explorationRate = 1 / 60

getAccessibleLocations :: [Location] -> [Location]
getAccessibleLocations locs =
   nub (neighbors =<< locs) \\ locs
   where
     neighbors (x,y) = [(x+1, y), (x-1,y), (x, y+1), (x, y-1)]

updateInventory :: [Terrain] -> [Person] -> State Inventory ()
updateInventory terrains ps = do
  modify $ addResource Wood $ gatherFrom [Forest] Woodcutter
  modify $ addResource Stone $ gatherFrom [RockyHill] StoneGatherer
  modify $ addResource Food $ gatherFrom [Grass, Forest] Hunter
  where
    gatherFrom :: [Terrain] -> Job -> Double
    gatherFrom ts j = gatherRatePerTick * if null $ intersect ts terrains then 0 else gatherRate
      where
        factor = sum $ fmap (toolFactor . pTool) $ filter (jobIs j) ps
        gatherRate = (*) factor $ fromIntegral $ countf (jobIs j) ps

    gatherRatePerDay = 10
    gatherRatePerTick = perDayToPerTick gatherRatePerDay


aroundLocation :: Int -> Location -> [Location]
aroundLocation n (x, y) = [(i, j) | i <- [x-n.. x+n], j <- [y-n .. y+n]]

getLastId :: StateT GameState IO ID
getLastId = gets $ fromMaybe 0 . safeMax . fmap vID . gVillages

jobIs :: Job -> Person -> Bool
jobIs j = (==j) . pJob
