module GameState where

import Data.List

import ServerTypes
import Utils

type Location = (Int, Int)
type TickNr = Int
type Name = String

data Event = NewVillage Name Location User deriving (Show)

data Village = Village {
  vID :: ID,
  vUser :: User,
  vCreated :: TickNr,
  vName :: Name,
  vLocation :: Location
} deriving (Show)

showVillage v = vName v ++ " " ++ show (vLocation v)

data GameState = GameState {
  gTickNr :: TickNr,
  gVillages :: [Village]
} deriving (Show)

villagesOf :: GameState -> User -> [Village]
villagesOf gameState user = filter ((== user) . vUser)  $ gVillages gameState

getVillage :: GameState -> ID -> Maybe Village
getVillage gameState id = find ((==id) . vID) . gVillages $ gameState
