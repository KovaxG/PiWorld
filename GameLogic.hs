module GameLogic (
  newGameStateVar,
  updateGameState,
) where

import Control.Concurrent.MVar

import GameState

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
tick (Just (NewVillage name location)) gameState =
  let s = updateTickNr gameState
  in s {
    gVillages = gVillages s ++ [Village (gTickNr s) name location]
  }

updateTickNr :: GameState -> GameState
updateTickNr gameState =
  gameState { gTickNr = gTickNr gameState + 1 }
