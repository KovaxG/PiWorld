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
  startNewDiscoveringLocation,
  getInventoryCapacity
) where

import Control.Monad.State
import Data.List
import Data.Maybe

import Constants
import GameTypes
import ServerTypes
import Utils

setVillagers :: (Monad m) => [Person] -> StateT Village m ()
setVillagers villagers = modify $ \v -> v { vVillagers = villagers }

modifyVillagers :: (Person -> Person) -> State Village ()
modifyVillagers f = do
  villagers <- gets vVillagers
  modify $ \v -> v { vVillagers = f <$> villagers }

setInventory :: (Monad m) => Inventory -> StateT Village m ()
setInventory inv = modify $ \v -> v { vInventory = inv }

modifyInventory :: (Monad m) => (Inventory -> Inventory) -> StateT Village m ()
modifyInventory f = do
  inventory <- gets vInventory
  modify $ \v -> v { vInventory = f inventory }

updateTickNr :: (Monad m) => StateT GameState m ()
updateTickNr = do
  curTick <- gets gTickNr
  modify $ \s -> s { gTickNr = curTick + 1 }

updatePercentDiscoveringLocation :: (Monad m) => Double -> StateT Village m ()
updatePercentDiscoveringLocation newPercent = do
  discoveringLocation <- gets vDiscoveringLocation
  maybe noLocation updatePercent discoveringLocation
  where
    noLocation = return ()
    updatePercent :: (Monad m) => (Location, Percent) ->  StateT Village m ()
    updatePercent (location, _) =
      modify $ \v -> v { vDiscoveringLocation = Just (location, newPercent) }

clearDiscoveringLocation :: (Monad m) => StateT Village m ()
clearDiscoveringLocation = modify $ \v -> v { vDiscoveringLocation = Nothing }

addDiscoveredLocation :: (Monad m) => Location -> StateT Village m ()
addDiscoveredLocation loc = modify $ \v -> v { vDiscoveredLocations = loc : vDiscoveredLocations v }

startNewDiscoveringLocation :: (Monad m) => Location -> StateT Village m ()
startNewDiscoveringLocation loc = modify $ \v -> v { vDiscoveringLocation = Just (loc, 0) }

getInventoryCapacity :: (Monad m) => StateT Village m Int
getInventoryCapacity = do
  buildings <- gets vBuildings
  return $ sum $ fmap (capacity . buildingSize) buildings
