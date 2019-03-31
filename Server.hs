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

data GetRequest = Get String (Map String String) deriving (Show)
type Response = String

data Request = MainMenu
             | WorldMap
             | LoginPage
             | Login UserName Password
             deriving (Show)

host = Host "127.0.0.1"
port = "80"

runServer :: MVar GameState -> LoginDB -> IO ()
runServer gameStateVar loginDBVar =
  serve host port $ \(socket, addr) -> do
    request <- maybe "No Message" unpack <$> recv socket 1024
    --putStrLn request
    gameState <- readMVar gameStateVar
    maybeUser <- getUserFromIp loginDBVar $ ipOf addr
    let (response, user) = either (\a -> (a, maybeUser)) id $ handleInput gameState request maybeUser
    --putStrLn response
    --putStrLn $ show maybeUser
    maybe (return ()) (bindIpWithUser loginDBVar (ipOf addr)) user
    send socket $ pack response
  where
    handleInput gameState s maybeUser =
      parseGetRequest s >>= parseRequest >>= return . handleRequest gameState maybeUser

handleRequest :: GameState -> Maybe User -> Request -> (HTML, Maybe User)
handleRequest _ maybeUser MainMenu = (response, maybeUser)
  where
    response =
      startHtml
      |> html (
        hed ( title "PiWorld Main Menu" )
        |> body (
          "Welcome " ++ show maybeUser
        )
      )

handleRequest _ Nothing (Login user pass) = (response, Just user)
  where
    response =
      startHtml
      |> html (
        hed ( title "PiWorld Login" )
        |> body ( "Welcome " ++ user )
      )

handleRequest _ (Just user) (Login _ _) = (response, Just user)
  where
    response =
      startHtml
      |> html (
        hed ( title "PiWorld Login" )
        |> body (
          "You are already logged in as " ++ user ++ ". Log off to sign in as another user."
        )
      )

handleRequest _ (Just user) LoginPage = (response, Just user)
  where
    response =
      startHtml
      |> html (
        hed ( title "PiWorld Login" )
        |> body (
          "You are already logged in as " ++ user ++ ". Log off to sign in as another user."
        )
      )

handleRequest gameState Nothing LoginPage = (response, Nothing)
  where
    response =
      startHtml
      |> html (
        hed ( title "PiWorld Login" )
        |> body (
          form (
            addBreak "Login"
            |> input "text" "username" ""
            |> addBreak ""
            |> input "text" "password" ""
            |> addBreak ""
            |> input "submit" "login" "login"
          )
        )
      )

handleRequest gameState maybeUser WorldMap = (response, maybeUser)
  where
    toInput v = showVillage v ++ " " ++ input "submit" "" "view"
    response =
      startHtml
      |> html (
        hed ( title "PiWorld Worldmap" )
        |> body (
          form (
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
  | otherwise = Left $ show req ++ " not supported."
  where
    userName = "username"
    passWord = "password"
