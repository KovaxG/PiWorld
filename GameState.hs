module GameState where

type Location = (Int, Int)
type TickNr = Int
type Name = String

data Event = NewVillage Name Location deriving (Show)

data Village = Village {
  vCreated :: TickNr,
  vName :: Name,
  vLocation :: Location
} deriving (Show)

showVillage v = vName v ++ " " ++ show (vLocation v)

data GameState = GameState {
  gTickNr :: TickNr,
  gVillages :: [Village]
} deriving (Show)
