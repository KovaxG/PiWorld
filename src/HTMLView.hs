{-
  This is the source code for the web real time stratgy game, PiWorld.
  It was written to be a hobby project to see how a big project looks
  like in Haskell, and also to learn about web development.
  You are free to look at the code and use it as you see fit, as long
  as it is not for profit. If it is please contact the author, KovaxG
  on gitthub.
  Author(s): KovaxG
-}
module HTMLView where

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as BS

import GameTypes
import ServerTypes
import Utils

type HTML = String

toPath :: String -> String
toPath s = "src/views/" ++ s ++ ".html"

toHTML :: Response -> IO HTML
toHTML Unrecognised = return ""

toHTML VillageNotFound = return "No such village found."

toHTML (DefaultVillageView villageName userName location) = do
  contents <- readFile $ toPath "DefaultVillageView"
  return $ interpolateString vars contents
  where
    vars = [
      ("villageName", getVillageName villageName),
      ("userName", getUserName userName),
      ("location", show location)]

toHTML (OwnedVillageView villageName location  villagers buildings inventory) = do
  contents <- readFile $ toPath "OwnedVillageView"
  return $ interpolateList lists $ interpolateString strings contents
  where
    strings = [
      ("villageName", getVillageName villageName),
      ("location", show location),
      ("population", show $ length villagers),
      ("inventory", show inventory)]

    lists = [
      (["buildings"], buildingList),
      (["vName", "vJob", "vHunger", "vHealth", "vStatus", "vId"], personList)]

    buildingList = fmap (pure . show) buildings
    personList =
      fmap (\v ->
        fmap ($v) [
          pName,
          show . pJob,
          show . getHunger . pHunger,
          show . getHealth . pHealth,
          show . pStatus,
          show . pID
          ]
      ) villagers

toHTML LogoutPage = readFile $ toPath "LogoutView"

toHTML (Overview userName villages) = do
  contents <- readFile $ toPath "OverviewView"
  return $ interpolateList lists $ interpolateString vars contents
  where
    vars = [("userName", getUserName userName)]
    lists = [(["vName", "vId"], villageList)]
    villageList =  fmap (\v -> fmap ($v) [getVillageName . fst, show . snd]) villages

toHTML MainPage = readFile $ toPath "MainPageView"

toHTML (LoginSuccess userName) = do
  contents <- readFile $ toPath "LoginSuccessView"
  return $ interpolateString vars contents
  where
    vars = [("userName", getUserName userName)]

toHTML FailedLogin = return "User does not exist"

toHTML (AlreadyLoggedIn userName) = do
  contents <- readFile $ toPath "AlreadyLoggedInView"
  return $ interpolateString vars contents
  where
    vars = [("userName", getUserName userName)]

toHTML LoginScreen = readFile $ toPath "LoginScreenView"

toHTML (WorldMapScreen villages) = do
  contents <- readFile $ toPath "WorldMapView"
  return $ interpolateList lists contents
  where
    lists = [(["vName", "vLocation", "vId"], villageList)]
    villageList =
      fmap (\(name, location, id) -> [getVillageName name, show location, show id]) villages

toHTML IllegalAction = readFile $ toPath "IllegalActionView"

toHTML (PersonJobView name id job availableJobs) = do
  contents <- readFile $ toPath "PersonJobView"
  return $ interpolateList lists $ interpolateString vars contents
  where
    vars = [("villagerName", name), ("currentJob", show job)]
    lists = [(["vId", "vJob"], jobList)]
    jobList = fmap (\j -> [show id, show j]) availableJobs

toHTML JobChanged = readFile $ toPath "JobChangedView"

toHTML (Image path) = unpack <$> BS.readFile ("res/" ++ path)
