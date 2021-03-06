{-
  This is the source code for the web real time stratgy game, PiWorld.
  It was written to be a hobby project to see how a big project looks
  like in Haskell, and also to learn about web development.
  You are free to look at the code and use it as you see fit, as long
  as it is not for profit. If it is please contact the author, KovaxG
  on gitthub.
  Author(s): KovaxG
-}
module Server (
  runServer
) where

import Control.Concurrent.MVar
import Data.ByteString.Char8 (pack, unpack)
import Data.List as List
import Data.Map as Map hiding (member, (\\))
import Data.Maybe
import Data.Set (toList)
import Debug.Trace
import Network.Simple.TCP
import System.CPUTime
import Text.Parsec

import Constants
import GameTypes
import HTMLView
import LoginDB
import MessageQueue
import RequestParser
import ServerTypes
import UserDB
import Utils

runServer :: MVar GameState -> MessageQueue Event -> LoginDB -> UserDB -> IO ()
runServer gameStateVar messageQueue loginDB userDB =
  serve host port $ \(socket, addr) -> do
    startTime <- getCPUTime
    request <- maybe "No Message" unpack <$> recv socket receiveBufferSize
    let validRequestEither = parseRequest <$> parseHTTPRequest request

    maybeUser <- getUserFromIp loginDB (ipOf addr)
    response <- either unrecognisedRequest
                       (requestHandler (ipOf addr) maybeUser)
                       validRequestEither

    putStrLn $ "<-- " ++ show validRequestEither
    putStrLn $ "--> " ++ show response

    endTime <- getCPUTime
    let processTime = fromIntegral (endTime - startTime) / 1000000000
    putStrLn $ "Time took: " ++ show processTime ++ " ms"

    html <- toHTML processTime response
    send socket (pack html)
    putStrLn ""
    where
      requestHandler :: IP -> Maybe User -> Request -> IO Response
      requestHandler ip maybeUser request = case request of
        (GameRequest req) -> handleRequest loginDB ip userDB gameStateVar messageQueue maybeUser req
        (Resource rawPath) -> do
          gameState <- readMVar gameStateVar
          let path = processImagePath maybeUser gameState rawPath
          return $ Image path
        (NotSupported _) -> return Unrecognised

      unrecognisedRequest :: String -> IO Response
      unrecognisedRequest _ = return Unrecognised

      ipOf :: SockAddr -> String
      ipOf = takeWhile (!=':') . show


processImagePath :: Maybe User -> GameState -> String -> String
processImagePath maybeUser gameState s =
  let worldMap = gTerrain gameState
      visibleLocations = maybe (omniscientView worldMap) userView maybeUser
  in parse locationRule "Tile Resource" s
      |> fmap (myLookup worldMap visibleLocations)
      |> either (const s) id
  where
    myLookup :: Map Location Terrain -> [Location] -> Location -> String
    myLookup worldMap visibleLocs loc
      | elem loc visibleLocs =
        Map.lookup loc worldMap
        |> maybe hiddenMapTile tileToPath
      | otherwise = hiddenMapTile

    omniscientView :: Map Location Terrain -> [Location]
    omniscientView = keys

    userView :: User -> [Location]
    userView user =
      villagesOf gameState user
      |> (=<<) vDiscoveredLocations
      |> nub

    tileToPath :: Terrain -> String
    tileToPath t = show t ++ ".png"

    locationRule = do
      string "Tile("
      first <- read <$> many digit
      char ','
      second <- read <$> many digit
      char ')'
      return (first, second)


handleRequest :: LoginDB
              -> IP
              -> UserDB
              -> MVar GameState
              -> MessageQueue Event
              -> Maybe User
              -> GameRequest
              -> IO Response

handleRequest loginDB ip userDB gameStateVar messageQueue (Just user) (ConstructBuilding villageID buildingSize) = do
  gameState <- readMVar gameStateVar
  maybe notFound villageFound $ getVillage gameState villageID
  where
    notFound = return VillageNotFound
    villageFound village = do
      pushMessage messageQueue $ BuildBuilding villageID buildingSize
      handleRequest loginDB ip userDB gameStateVar messageQueue (Just user) (ViewVillage $ vID village)


handleRequest _ _ _ gameStateVar _ (Just user) (ViewNewBuildingMenu id) = do
  gameState <- readMVar gameStateVar
  return $ maybe notFound villageFound $ getVillage gameState id
  where
    notFound = VillageNotFound
    villageFound village = NewBuildingMenuView (vID village) (vName village) (buildingSizes)

handleRequest _ _ _ gameStateVar _ Nothing (ViewVillage id) = do
  gameState <- readMVar gameStateVar
  return $ maybe notFound villageFound $ getVillage gameState id
  where
    notFound = VillageNotFound
    villageFound village =
      DefaultVillageView (vName village) (uName $ vUser village) (vLocation village)

handleRequest _ _ _ gameStateVar _ (Just user) (ViewVillage id) = do
  gameState <- readMVar gameStateVar
  return $ maybe notFound villageFound $ getVillage gameState id
  where
    notFound = VillageNotFound
    villageFound village =
      if vUser village == user
      then OwnedVillageView (vName village)
                            (vLocation village)
                            (vVillagers village)
                            (vBuildings village)
                            (vInventory village)
                            (maybe 0 snd $ vDiscoveringLocation village)
                            (sum $ fmap (capacity . buildingSize) $ vBuildings village)
                            (vID village)
                            (vBuildingUnderConstruction village)
      else DefaultVillageView (vName village) (uName $ vUser village) (vLocation village)

handleRequest loginDB ip _ _ _ _ Logout = do
  unbindIp loginDB ip
  return LogoutPage

handleRequest _ _ _ gameStateVar _ (Just user) MainMenu = do
  gameState <- readMVar gameStateVar
  return $ Overview (uName user) (nameAndId <$> villagesOf gameState user)
  where
    nameAndId v = (vName v, vID v)

handleRequest _ _ _ _ _ Nothing MainMenu = return MainPage

handleRequest loginDB ip userDB _ _ Nothing (Login user pass) = do
  maybe userDoesntExist handleUser =<< getUser userDB user pass
  where
    userDoesntExist = return FailedLogin
    handleUser user = do
      bindIpWithUser loginDB ip user
      return $ LoginSuccess (uName user)

handleRequest _ _ _ _ _ (Just user) (Login _ _) = return $ AlreadyLoggedIn (uName user)

handleRequest _ _ _ _ _ (Just user) LoginPage = return $ AlreadyLoggedIn (uName user)

handleRequest _ _ _ _ _ Nothing LoginPage = return LoginScreen

handleRequest _ _ _ gameStateVar _ _ WorldMap = do
  gameState <- readMVar gameStateVar
  let villages = gVillages gameState
  let locations = Map.keys $ gTerrain gameState
  let maxWidth = maximum $ fmap fst locations
  let maxHeight = maximum $ fmap snd locations
  return $ WorldMapScreen (maxWidth, maxHeight) locations (toData <$> villages)
  where
    toData v = (vName v, vLocation v, vID v)

handleRequest _ _ _ gameStateVar _ (Just user) (JobMenu id) = do
  gameState <- readMVar gameStateVar
  let allPeople = vVillagers =<< gVillages gameState
  return $ maybe notFound (found (gTerrain gameState) (gVillages gameState)) $ find ((==id) . pID) allPeople
  where
    notFound = IllegalAction

    found :: Map Location Terrain -> [Village] -> Person -> Response
    found worldMap villages p =
      let village = fromJust $ find (elem p . vVillagers) villages
          villageJobs = catMaybes $ fmap jobOf (getTerrain worldMap $ vDiscoveredLocations village)
          availableJobs = [Civilian, Hunter, Explorer, Builder] ++ villageJobs
      in PersonJobView (pName p) (pID p) (pJob p) availableJobs

    jobOf :: Terrain -> Maybe Job
    jobOf Forest = Just Woodcutter
    jobOf RockyHill = Just StoneGatherer
    jobOf _ = Nothing

handleRequest loginDB ip userDB gameStateVar messageQueue (Just user) (JobChange id newJob) = do
  villages <- gVillages <$> readMVar gameStateVar
  let allPeople = vVillagers =<< villages
  maybe notFound (found villages) $ find ((==id) . pID) allPeople
  where
    nameAndId v = (vName v, vID v)
    notFound = return IllegalAction
    found villages p = do
      pushMessage messageQueue $ ChangeJobOfVillager id newJob
      let villageId = vID $ fromJust $ find (elem  p . vVillagers) villages
      handleRequest loginDB ip userDB gameStateVar messageQueue (Just user) (ViewVillage villageId)

handleRequest _ _ _ _ _ _ _ = return IllegalAction
