import Control.Concurrent
import Data.Maybe

import GameLogic (newGameStateVar, gameLoop)
import GameTypes (Event(NewVillage), GameState (..), nameGenerator, showMap, User (..), UserName (..), Password (..), VillageName (..))
import LoginDB (newLoginDB)
import MessageQueue (newEmptyQueue, pushMessage, popMessages, MessageQueue)
import Server (runServer)
import UserDB (newUserDB, addUser, getUser)

main :: IO ()
main = do
  queueVar <- newEmptyQueue
  gameStateVar <- newGameStateVar
  loginDB <- newLoginDB
  userDB <- newUserDB

  putStrLn "Starting game loop..."
  _ <- forkIO $ gameLoop queueVar gameStateVar

  putStrLn "Starting server..."
  _ <- forkIO $ runServer gameStateVar queueVar loginDB userDB

  putStrLn "Adding test data..."
  addUser userDB (UserName "Gyuri")  (Password "asdf")
  addUser userDB (UserName "Pepusz") (Password "boef")
  addUser userDB (UserName "Klark")  (Password "pass")
  gyuri <- fromJust <$> getUser userDB (UserName "Gyuri")  (Password "asdf")
  petra <- fromJust <$> getUser userDB (UserName "Pepusz") (Password "boef")
  klark <- fromJust <$> getUser userDB (UserName "Klark")  (Password "pass")

  names1 <- sequence $ replicate 4 nameGenerator
  pushMessage queueVar $ NewVillage (VillageName "Gyurtown") (2,2) gyuri names1
  names2 <- sequence $ replicate 4 nameGenerator
  pushMessage queueVar $ NewVillage (VillageName "Peptown") (3,5) petra names2
  names3 <- sequence $ replicate 4 nameGenerator
  pushMessage queueVar $ NewVillage (VillageName "Garbageton") (1,1) klark names3
  putStrLn "Done."
