{-
  This is the source code for the web real time stratgy game, PiWorld.
  It was written to be a hobby project to see how a big project looks
  like in Haskell, and also to learn about web development.
  You are free to look at the code and use it as you see fit, as long
  as it is not for profit. If it is please contact the author, KovaxG
  on gitthub.
  Author(s): KovaxG
-}
import Control.Concurrent
import Data.Maybe

import GameLogic (newGameStateVar, gameLoop)
import GameTypes (Event(NewVillage), GameState (..), nameGenerator, User (..), UserName (..), Password (..), VillageName (..))
import LoginDB (newLoginDB)
import MessageQueue (newEmptyQueue, pushMessage, popMessages, MessageQueue)
import Server (runServer)
import UserDB (newUserDB, addUser, getUser)

-- TODO this ought to be propagated and displayed in game
version = "v" ++ show bigversion ++ "." ++ show subversion
  where
    bigversion = 0
    subversion = 2

main :: IO ()
main = do
  putStrLn $ "Launching PiWorld " ++ version
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
