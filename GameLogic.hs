module GameLogic (
  newGameStateVar,
  updateGameState,
) where

import Control.Concurrent.MVar
import Control.Monad.State
import Data.List
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
    gVillages = []
  }

updateGameState :: MVar GameState -> Maybe Event -> IO GameState
updateGameState gameStateVar event = do
  gameState <- takeMVar gameStateVar
  let newState = snd $ runState (tick event) gameState
  putMVar gameStateVar newState
  return newState

tick :: Maybe Event -> State GameState ()
tick Nothing = updateTickNr
tick (Just (NewVillage name location user names)) = do
  updateTickNr
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
getLastId = fromMaybe 0 . safeMax . map vID . gVillages <$> get
