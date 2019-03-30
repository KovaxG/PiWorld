module Server (
  runServer
) where

import Control.Concurrent.MVar
import Data.Bifunctor
import Data.ByteString.Char8 (pack, unpack)
import Data.Either.Extra
import Network.Simple.TCP
import Text.Parsec

import GameState
import MyHTML

host = Host "127.0.0.1"
port = "80"

runServer :: MVar GameState -> IO ()
runServer gameStateVar =
  serve host port $ \(socket, _) -> do
    request <- recv socket 1024
    gameState <- readMVar gameStateVar
    let response = maybe "No Message" (fromEither . handleInput gameState . unpack) request
    send socket $ pack response
  where
    handleInput gameState s =
      parseGetRequest s >>= parseRequest >>= return . handleRequest gameState

data GetRequest = Get String deriving (Show)
type Response = String

data Request = WorldMap deriving (Show)

parseGetRequest :: String -> Either String GetRequest
parseGetRequest = first show . parse rule "Parsing Request"
  where
    rule = do
      string "GET /"
      content <- many alphaNum
      spaces
      return $ Get content

parseRequest :: GetRequest -> Either String Request
parseRequest (Get s) = case s of
  "map" -> Right WorldMap
  request -> Left $ request ++ " not supported."

handleRequest :: GameState -> Request -> HTML
handleRequest gameState WorldMap =
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
  where
    toInput v = showVillage v ++ " " ++ input "submit" "" "view"
