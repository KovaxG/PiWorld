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

import GameState
import LoginDB
import MyHTML
import ServerTypes
import UserDB

data GetRequest = Get String (Map String String) deriving (Show)
type Response = String

data Request = MainMenu
             | WorldMap
             | LoginPage
             | Login UserName Password
             | Logout
             deriving (Show)

host = Host "127.0.0.1"
port = "80"

runServer :: MVar GameState -> LoginDB -> UserDB -> IO ()
runServer gameStateVar loginDB userDB =
  serve host port $ \(socket, addr) -> do
    request <- maybe "No Message" unpack <$> recv socket 1024
    gameState <- readMVar gameStateVar
    let ip = ipOf addr
    maybeUser <- getUserFromIp loginDB ip
    let validRequestEither = parseGetRequest request >>= parseRequest
    response <- either return (handleRequest loginDB ip userDB gameState maybeUser) validRequestEither
    --putStrLn $ show validRequestEither
    --putStrLn response
    send socket $ pack response

handleRequest :: LoginDB -> IP -> UserDB -> GameState -> Maybe User -> Request -> IO HTML
handleRequest loginDB ip _ _ _ Logout = do
  unbindIp loginDB ip
  return response
  where
    response =
      startHtml
      |> html (
        hed ( title "PiWorld Logout" )
        |> body (
          addBreak "Logout Successful"
          |> form "/" (
            addBreak "Click here to return to the main page."
            |> input "submit" "" "Main Page"
          )
        )
      )

handleRequest _ _ _ _ (Just user) MainMenu = return response
  where
    response =
      startHtml
      |> html (
        hed ( title "PiWorld Main Menu" )
        |> body ("Hello, " ++ show user)
        |> addBreak ""
        |> form "/logout" (
          "Click here to log out:"
          |> input "submit" "" "Log Out"
        )
      )

handleRequest _ _ _ _ Nothing MainMenu = return response
  where
    response =
      startHtml
      |> html (
        hed ( title "PiWorld Main Menu" )
        |> body (
          form "/login" (
            "You are not logged in, you can do that here:"
            |> addBreak ""
            |> input "submit" "" "Log In"
          )
        )
      )

handleRequest loginDB ip userDB _ Nothing (Login user pass) = do
  maybe userDoesntExist handleUser =<< getUser userDB user pass
  where
    handleUser user = do
      bindIpWithUser loginDB ip user
      return response

    userDoesntExist = return "User does not exist"

    response =
      startHtml
      |> html (
        hed ( title "PiWorld Login" )
        |> body (
          addBreak ( "Welcome " ++ user )
          |> form "/" (
            addBreak ("Click here to go to main view: ")
            |> input "submit" "" "Main Page"
          )
        )
      )

handleRequest _ _ _ _ (Just user) (Login _ _) = return response
  where
    response =
      startHtml
      |> html (
        hed ( title "PiWorld Login" )
        |> body (
          "You are already logged in as " ++ show user ++ ". Log off to sign in as another user."
        )
      )

handleRequest _ _ _ _ (Just user) LoginPage = return response
  where
    response =
      startHtml
      |> html (
        hed ( title "PiWorld Login" )
        |> body (
          "You are already logged in as " ++ show user ++ ". Log off to sign in as another user."
        )
      )

handleRequest _ _ _ gameState Nothing LoginPage = return response
  where
    response =
      startHtml
      |> html (
        hed ( title "PiWorld Login" )
        |> body (
          form "" (
            addBreak "Login"
            |> "username: "
            |> input "text" "username" ""
            |> addBreak ""
            |> "password:"
            |> input "text" "password" ""
            |> addBreak ""
            |> input "submit" "login" "login"
          )
        )
      )

handleRequest _ _ _ gameState maybeUser WorldMap = return response
  where
    toInput v = showVillage v ++ " " ++ input "submit" "" "view"
    response =
      startHtml
      |> html (
        hed ( title "PiWorld Worldmap" )
        |> body (
          form "" (
            addBreak "Game Map"
            |> ((addBreak . toInput) =<< gVillages gameState)
          )
        )
      )

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
  | Prelude.null main = Right MainMenu
  | main == "map" = Right WorldMap
  | main == "login" && Data.Map.null vars = Right LoginPage
  | main == "login" && member userName vars && member passWord vars =
    let name = fromJust $ Data.Map.lookup userName vars
        pass = fromJust $ Data.Map.lookup passWord vars
    in Right $ Login name pass
  | main == "logout" = Right Logout
  | otherwise = Left $ show req ++ " not supported."
  where
    userName = "username"
    passWord = "password"
