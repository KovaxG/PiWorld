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
import Data.Bifunctor
import Data.ByteString.Char8 (pack, unpack)
import Data.Either.Extra
import Data.List as List
import Data.Map as Map hiding (member)
import Data.Maybe
import Data.Set (toList)
import Debug.Trace
import Network.Simple.TCP
import Text.Parsec

import GameTypes
import HTMLView
import LoginDB
import MessageQueue
import ServerTypes
import UserDB
import Utils

host = Host "127.0.0.1" --"192.168.0.136"
port = "80"

runServer :: MVar GameState -> MessageQueue Event -> LoginDB -> UserDB -> IO ()
runServer gameStateVar messageQueue loginDB userDB =
  serve host port $ \(socket, addr) -> do
    request <- maybe "No Message" unpack <$> recv socket 1024
    let ip = ipOf addr
    maybeUser <- getUserFromIp loginDB ip
    let validRequestEither = parseGetRequest request >>= parseRequest
    let requestHandler = handleRequest loginDB ip userDB gameStateVar messageQueue maybeUser
    response <- either unrecognisedRequest requestHandler validRequestEither
    putStrLn $ "<-- " ++ show validRequestEither
    putStrLn $ "--> " ++ show response
    putStrLn ""
    send socket $ pack (toHTML response)
    where
      unrecognisedRequest _ = return Unrecognised

handleRequest :: LoginDB -> IP -> UserDB -> MVar GameState -> MessageQueue Event -> Maybe User -> Request -> IO Response
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
  return $ WorldMapScreen (toData <$> gVillages gameState)
  where
    toData v = (vName v, vLocation v, vID v)

handleRequest _ _ _ gameStateVar _ (Just user) (JobMenu id) = do
  gameState <- readMVar gameStateVar
  let allPeople = vVillagers =<< gVillages gameState
  return $ maybe notFound (found $ gVillages gameState) $ find ((==id) . pID) allPeople
  where
    notFound = IllegalAction
    found villages p =
      let village = fromJust $ find (elem p . vVillagers) villages
          villageJobs = catMaybes $ fmap jobOf (Data.Set.toList $ vDiscoveredTerrain village)
          availableJobs = [Civilian, Hunter, Explorer] ++ villageJobs
      in PersonJobView (pName p) (pID p) (pJob p) availableJobs


    jobOf :: Terrain -> Maybe Job
    jobOf Forest = Just Woodcutter
    jobOf RockyHill = Just StoneGatherer
    jobOf _ = Nothing

handleRequest loginDB ip userDB gameStateVar messageQueue (Just user) (JobChange id newJob) = do
  gameState <- readMVar gameStateVar
  let allPeople = vVillagers =<< gVillages gameState
  maybe notFound found $ find ((==id) . pID) allPeople
  where
    nameAndId v = (vName v, vID v)
    notFound = return IllegalAction
    found p = do
      pushMessage messageQueue $ ChangeJobOfVillager id newJob
      return JobChanged

handleRequest _ _ _ _ _ _ _ = return IllegalAction

ipOf :: SockAddr -> String
ipOf = takeWhile (/= ':') . show

parseGetRequest :: String -> Either String GetRequest
parseGetRequest = first show . parse rule "Parsing Request" . removeCR
  where
    removeCR :: String -> String
    removeCR = List.filter (/='\r')

    rule = choice [try getWithVars, try pureGet, try post]

    pureGet = do
      string "GET /"
      content <- many alphaNum
      spaces
      return $ Get content []

    nonEmptyLine = alphaNum >> many (noneOf "\n\r")

    post = do
      string "POST"
      _ <- many (noneOf "\n\r")
      newline
      _ <- sepEndBy nonEmptyLine newline
      newline
      tuples <- sepBy tuple (char '&')
      return $ Post tuples

    getWithVars = do
      string "GET /"
      content <- many alphaNum
      char '?'
      tuples <- sepBy tuple (char '&')
      spaces
      return $ Get content tuples

    tuple = do
      key <- many (noneOf "=& ")
      char '='
      value <- many (noneOf "=& ")
      return (key, value)

parseRequest :: GetRequest -> Either String Request
parseRequest req@(Post vars)
  | member userName vars && member passWord vars =
    let name = UserName $ fromJust $ List.lookup userName vars
        pass = Password $ fromJust $ List.lookup passWord vars
    in Right $ Login name pass
  | otherwise = Left $ show req ++ " not supported."
  where
    userName = "username"
    passWord = "password"

parseRequest req@(Get main vars)
  | main == "" && hasKeys && isID firstKey = Right $ ViewVillage (read firstKey)
  | main == "map" && hasKeys && isID firstKey = Right $ ViewVillage (read firstKey)
  | main == "" = Right MainMenu
  | main == "map" = Right WorldMap
  | main == "login" && length vars < 2 = Right LoginPage
  | main == "login" && member userName vars && member passWord vars =
    let name = UserName $ fromJust $ List.lookup userName vars
        pass = Password $ fromJust $ List.lookup passWord vars
    in Right $ Login name pass
  | main == "logout" = Right Logout
  | main == "person" && hasKeys && isID firstKey && firstValue == "Job" = Right $ JobMenu (read firstKey)
  | main == "person" && hasKeys && isID firstKey && isJob firstValue =
    Right $ JobChange (read firstKey) (fromJust $ safeRead firstValue)
  | otherwise = Left $ show req ++ " not supported."
  where
    isJob s = isJust $ (safeRead s :: Maybe Job)
    hasKeys = length vars > 0
    firstKey = fst $ head vars
    firstValue = fromJust (List.lookup firstKey vars)
    userName = "username"
    passWord = "password"
