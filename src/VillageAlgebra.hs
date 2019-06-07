{-
  This is the source code for the web real time stratgy game, PiWorld.
  It was written to be a hobby project to see how a big project looks
  like in Haskell, and also to learn about web development.
  You are free to look at the code and use it as you see fit, as long
  as it is not for profit. If it is please contact the author, KovaxG
  on gitthub.
  Author(s): KovaxG
-}
module VillageAlgebra (
  setVillagers,
  modifyVillagers,
  setInventory,
  modifyInventory,
  updateTickNr,
  updatePercentDiscoveringLocation,
  clearDiscoveringLocation,
  addDiscoveredLocation,
  startNewDiscoveringLocation
) where

import Control.Monad.State
import Data.List
import Data.Maybe

import GameTypes
import ServerTypes
import Utils

-- TODO maybe replace all occurences of IO with a generic context
-- so that they can be used in a context with Identity instead

setVillagers :: [Person] -> StateT Village IO ()
setVillagers villagers = modify $ \v -> v { vVillagers = villagers }

modifyVillagers :: (Person -> Person) -> State Village ()
modifyVillagers f = do
  villagers <- gets vVillagers
  modify $ \v -> v { vVillagers = f <$> villagers }

setInventory :: Inventory -> StateT Village IO ()
setInventory inv = modify $ \v -> v { vInventory = inv }

modifyInventory :: (Inventory -> Inventory) -> StateT Village IO ()
modifyInventory f = do
  inventory <- gets vInventory
  modify $ \v -> v { vInventory = f inventory }

updateTickNr :: StateT GameState IO ()
updateTickNr = do
  curTick <- gets gTickNr
  modify $ \s -> s { gTickNr = curTick + 1 }

updatePercentDiscoveringLocation :: Double -> StateT Village IO ()
updatePercentDiscoveringLocation newPercent = do
  discoveringLocation <- gets vDiscoveringLocation
  maybe noLocation updatePercent discoveringLocation
  where
    noLocation = return ()
    updatePercent :: (Location, Percent) -> StateT Village IO ()
    updatePercent (location, _) =
      modify $ \v -> v { vDiscoveringLocation = Just (location, newPercent) }

clearDiscoveringLocation :: StateT Village IO ()
clearDiscoveringLocation = modify $ \v -> v { vDiscoveringLocation = Nothing }

addDiscoveredLocation :: Location -> StateT Village IO ()
addDiscoveredLocation loc = modify $ \v -> v { vDiscoveredLocations = loc : vDiscoveredLocations v }

startNewDiscoveringLocation :: Location -> StateT Village IO ()
startNewDiscoveringLocation loc = modify $ \v -> v { vDiscoveringLocation = Just (loc, 0) }
