module GameLogic (
  newGameStateVar,
  updateGameState,
) where

import Control.Concurrent.MVar
import Control.Monad.State
import Data.List
import Data.Map
import Data.Maybe

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
  let newState = snd $ runState (traverse tick events) gameState
  putMVar gameStateVar newState
  return newState

tick :: Event -> State GameState ()
tick Tick = updateTickNr
tick (NewVillage name location user names) = do
  addNewVillage name location user names

addNewVillage :: Name -> Location -> User -> [Name] -> State GameState ()
addNewVillage name location user names = do
  curTick <- currentTick
  villages <- gVillages <$> get
  lastId <- getLastId
  let newVillage = Village {
    vID = (lastId + 1),
    vUser = user,
    vCreated = curTick,
    vName = name,
    vLocation = location,
    vInventory = Inventory 0,
    vVillagers = zipWith (\n i -> Person i n) names [1..]
  }
  modify (\s -> s { gVillages = newVillage : villages } )

updateTickNr :: State GameState ()
updateTickNr = do
  curTick <- currentTick
  modify (\s -> s { gTickNr = curTick + 1 } )

currentTick :: State GameState TickNr
currentTick = gTickNr <$> get

getLastId :: State GameState ID
getLastId = fromMaybe 0 . safeMax . Data.List.map vID . gVillages <$> get
