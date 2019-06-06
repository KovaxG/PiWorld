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

toHTML :: Double -> Response -> IO HTML
toHTML processTime Unrecognised = return ("It took " ++ show processTime ++ "ms to not recognize this command.")

toHTML processTime VillageNotFound = return $ "No such village found. This was done in " ++ show processTime ++ " ms."

toHTML processTime (DefaultVillageView villageName userName location) = do
  contents <- readFile $ toPath "DefaultVillageView"
  return $ interpolateString vars contents
  where
    vars = [
      ("villageName", getVillageName villageName),
      ("userName", getUserName userName),
      ("location", show location),
      ("processTime", show processTime)]

toHTML processTime (OwnedVillageView villageName location  villagers buildings inventory) = do
  contents <- readFile $ toPath "OwnedVillageView"
  return $ interpolateList lists $ interpolateString strings contents
  where
    strings = [
      ("villageName", getVillageName villageName),
      ("location", show location),
      ("population", show $ length villagers),
      ("inventory", show inventory),
      ("processTime", show processTime)]

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

toHTML processTime LogoutPage = readFile $ toPath "LogoutView"

toHTML processTime (Overview userName villages) = do
  contents <- readFile $ toPath "OverviewView"
  return $ interpolateList lists $ interpolateString vars contents
  where
    vars = [
      ("userName", getUserName userName),
      ("processTime", show processTime)]
    lists = [(["vName", "vId"], villageList)]
    villageList =  fmap (\v -> fmap ($v) [getVillageName . fst, show . snd]) villages

toHTML processTime MainPage = readFile $ toPath "MainPageView"

toHTML processTime (LoginSuccess userName) = do
  contents <- readFile $ toPath "LoginSuccessView"
  return $ interpolateString vars contents
  where
    vars = [("userName", getUserName userName)]

toHTML processTime FailedLogin = return "User does not exist"

toHTML processTime (AlreadyLoggedIn userName) = do
  contents <- readFile $ toPath "AlreadyLoggedInView"
  return $ interpolateString vars contents
  where
    vars = [("userName", getUserName userName)]

toHTML processTime LoginScreen = readFile $ toPath "LoginScreenView"

toHTML processTime (WorldMapScreen (maxWidth, maxHeight) locations villages) = do
  contents <- readFile $ toPath "WorldMapView"
  return $ interpolateString vars $ interpolateList lists contents
  where
    vars = [("processTime", show processTime)]
    lists = [
      (["vName", "vLocation", "vId"], villageList),
      (["tileLocation", "tileLocation", "possibleBreak"], tileLocationList)
      ]
    villageList =
      fmap (\(name, location, id) -> [getVillageName name, show location, show id]) villages
    tileLocationList = fmap (\l -> [showLocation l, showLocation l, breakIfMax l]) locations

    showLocation :: Location -> String
    showLocation (x,y) = "Tile(" ++ show x ++ "," ++ show y ++ ")"

    breakIfMax :: Location -> String
    breakIfMax (_, c)
      | c == maxWidth = "<br>"
      | otherwise = ""

toHTML processTime IllegalAction = readFile $ toPath "IllegalActionView"

toHTML processTime (PersonJobView name id job availableJobs) = do
  contents <- readFile $ toPath "PersonJobView"
  return $ interpolateList lists $ interpolateString vars contents
  where
    vars = [("villagerName", name), ("currentJob", show job)]
    lists = [(["vId", "vJob"], jobList)]
    jobList = fmap (\j -> [show id, show j]) availableJobs

toHTML processTime JobChanged = readFile $ toPath "JobChangedView"

toHTML _ (Image path) = unpack <$> BS.readFile ("res/" ++ path)
