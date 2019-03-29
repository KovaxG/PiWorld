module GameLogic (
  Event (..),
  GameState (..),
  newGameStateVar,
  updateGameState,
) where

import Control.Concurrent.MVar

type Location = (Int, Int)
type TickNr = Int

data Event = NewVillage Location deriving (Show)

data Village = Village {
  vCreated :: TickNr,
  vLocation :: Location
} deriving (Show)

data GameState = GameState {
  gTickNr :: TickNr,
  gVillages :: [Village]
} deriving (Show)

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
  let newState = tick event gameState
  putMVar gameStateVar newState
  return newState

-- TODO use state monad for convenience
tick :: Maybe Event -> GameState -> GameState
tick Nothing g = updateTickNr g
tick (Just (NewVillage location)) gameState =
  let s = updateTickNr gameState
  in s {
    gVillages = gVillages s ++ [Village (gTickNr s) location]
  }

updateTickNr :: GameState -> GameState
updateTickNr gameState =
  gameState { gTickNr = gTickNr gameState + 1 }
