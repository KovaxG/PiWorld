module Server (
  runServer
) where

import Control.Concurrent.MVar
import Data.Bifunctor
import Data.ByteString.Char8 (pack, unpack)
import Data.Either.Extra
import Data.Map
import Data.Maybe
import Debug.Trace
import Network.Simple.TCP
import Text.Parsec

import GameTypes
import HTMLView
import LoginDB
import ServerTypes
import UserDB
import Utils

host = Host "127.0.0.1" --"192.168.0.136"
port = "80"

runServer :: MVar GameState -> LoginDB -> UserDB -> IO ()
runServer gameStateVar loginDB userDB =
  serve host port $ \(socket, addr) -> do
    request <- maybe "No Message" unpack <$> recv socket 1024
    gameState <- readMVar gameStateVar
    let ip = ipOf addr
    maybeUser <- getUserFromIp loginDB ip
    let validRequestEither = parseGetRequest request >>= parseRequest
    let requestHandler = handleRequest loginDB ip userDB gameState maybeUser
    response <- either unrecognisedRequest requestHandler validRequestEither
    putStrLn $ show response
    send socket $ pack (toHTML response)
    where
      unrecognisedRequest _ = return Unrecognised

handleRequest :: LoginDB -> IP -> UserDB -> GameState -> Maybe User -> Request -> IO Response
handleRequest _ _ _ gameState Nothing (ViewVillage id) =
  return $ maybe notFound villageFound $ getVillage gameState id
  where
    notFound = VillageNotFound
    villageFound village =
      DefaultVillageView (vName village) (uName $ vUser village) (vLocation village)

handleRequest _ _ _ gameState (Just user) (ViewVillage id) =
  return $ maybe notFound villageFound $ getVillage gameState id
  where
    notFound = VillageNotFound
    villageFound village =
      if vUser village == user
      then OwnedVillageView (vName village) (vLocation village) (vVillagers village) (vInventory village)
      else DefaultVillageView (vName village) (uName $ vUser village) (vLocation village)

handleRequest loginDB ip _ _ _ Logout = do
  unbindIp loginDB ip
  return LogoutPage

handleRequest _ _ _ gameState (Just user) MainMenu =
  return $ Overview (uName user) (nameAndId <$> villagesOf gameState user)
  where
    nameAndId v = (vName v, vID v)

handleRequest _ _ _ _ Nothing MainMenu = return $ MainPage

handleRequest loginDB ip userDB _ Nothing (Login user pass) = do
  maybe userDoesntExist handleUser =<< getUser userDB user pass
  where
    userDoesntExist = return FailedLogin
    handleUser user = do
      bindIpWithUser loginDB ip user
      return $ LoginSuccess (uName user)

handleRequest _ _ _ _ (Just user) (Login _ _) = return $ AlreadyLoggedIn (uName user)

handleRequest _ _ _ _ (Just user) LoginPage = return $ AlreadyLoggedIn (uName user)

handleRequest _ _ _ gameState Nothing LoginPage = return LoginScreen

handleRequest _ _ _ gameState _ WorldMap = return $ WorldMapScreen (toData <$> gVillages gameState)
  where
    toData v = (vName v, vLocation v, vID v)

ipOf :: SockAddr -> String
ipOf = takeWhile (/= ':') . show

parseGetRequest :: String -> Either String GetRequest
parseGetRequest = first show . parse rule "Parsing Request"
  where
    rule = choice [try getWithVars, try pureGet]

    pureGet = do
      string "GET /"
      content <- many alphaNum
      spaces
      return $ Get content empty

    getWithVars = do
      string "GET /"
      content <- many alphaNum
      char '?'
      tuples <- sepBy (variable) (char '&')
      spaces
      return $ Get content $ fromList tuples

    variable = do
      key <- many alphaNum
      char '='
      value <- many alphaNum
      return (key, value)

parseRequest :: GetRequest -> Either String Request
parseRequest req@(Get main vars)
  | main == "" && hasKeys && isID firstKey = Right $ ViewVillage (read firstKey)
  | main == "map" && hasKeys && isID firstKey = Right $ ViewVillage (read firstKey)
  | main == "" = Right MainMenu
  | main == "map" = Right WorldMap
  | main == "login" && length (keys vars) < 2 = Right LoginPage
  | main == "login" && member userName vars && member passWord vars =
    let name = fromJust $ Data.Map.lookup userName vars
        pass = fromJust $ Data.Map.lookup passWord vars
    in Right $ Login name pass
  | main == "logout" = Right Logout
  | otherwise = Left $ show req ++ " not supported."
  where
    hasKeys = length (keys vars) > 0
    firstKey = head $ keys vars
    userName = "username"
    passWord = "password"
