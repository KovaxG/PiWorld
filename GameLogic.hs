module GameLogic (
  Event (..),
  GameState,
  newState,
  tick
) where

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

newState :: GameState
newState =
  GameState {
    gTickNr = 0,
    gVillages = []
  }

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
