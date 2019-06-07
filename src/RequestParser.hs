module RequestParser (
  parseRequest,
  parseHTTPRequest
) where

import Data.Either.Extra
import Data.List
import Data.Maybe
import Text.Parsec

import GameTypes
import ServerTypes
import Utils

parseHTTPRequest :: String -> Either String HTTPRequest
parseHTTPRequest = leftMap show . parse rule "Parsing HTTP Request" . removeCR
  where
    removeCR :: String -> String
    removeCR = filter (!='\r')

    rule = choice [try getWithVars, try pureGet, try post]

    pureGet = do
      string "GET /"
      content <- many (noneOf " ")
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
      content <- many (noneOf "? ")
      char '?'
      tuples <- sepBy tuple (char '&')
      spaces
      return $ Get content tuples

    tuple = do
      key <- many (noneOf "=& ")
      char '='
      value <- many (noneOf "=& ")
      return (key, value)


parseRequest :: HTTPRequest -> Request
parseRequest req@(Post vars)
  | member userName vars && member passWord vars =
    let name = UserName $ unsafeLookup userName vars
        pass = Password $ unsafeLookup passWord vars
    in GameRequest $ Login name pass
  | otherwise = NotSupported (show req ++ " not supported.")
  where
    userName = "username"
    passWord = "password"

parseRequest req@(Get main vars)
  | startsWith "favicon.ico" main = Resource main
  | main == "" && hasKeys && isID firstKey = GameRequest $ ViewVillage (read firstKey)
  | main == "map" && hasKeys && isID firstKey = GameRequest $ ViewVillage (read firstKey)
  | main == "" = GameRequest MainMenu
  | main == "map" = GameRequest WorldMap
  | main == "login" && length vars < 2 = GameRequest LoginPage
  | main == "login" && member userName vars && member passWord vars =
    let name = UserName $ unsafeLookup userName vars
        pass = Password $ unsafeLookup passWord vars
    in GameRequest $ Login name pass
  | main == "logout" = GameRequest Logout
  | main == "person" && hasKeys && isID firstKey && firstValue == "Job" = GameRequest $ JobMenu (read firstKey)
  | main == "person" && hasKeys && isID firstKey && isJob firstValue =
    GameRequest $ JobChange (read firstKey) (fromJust $ safeRead firstValue)
  | startsWith resource main = Resource (main \\ resource)
  | otherwise = NotSupported $ show req ++ " not supported."
  where
    isJob s = isJust $ (safeRead s :: Maybe Job)
    hasKeys = length vars > 0
    firstKey = fst $ head vars
    firstValue = unsafeLookup firstKey vars
    userName = "username"
    passWord = "password"
    resource = "resource/"
