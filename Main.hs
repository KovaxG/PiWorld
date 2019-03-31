import Control.Concurrent
import Data.Maybe

import GameLogic (newGameStateVar, updateGameState)
import GameTypes (Event(NewVillage), GameState, nameGenerator)
import LoginDB (newLoginDB)
import MessageQueue (newEmptyQueue, pushMessage, popMessage, MessageQueue)
import Server (runServer)
import UserDB (newUserDB, addUser, getUser)
import ServerTypes (User (..))

main :: IO ()
main = do
  queueVar <- newEmptyQueue
  gameStateVar <- newGameStateVar
  loginDB <- newLoginDB
  userDB <- newUserDB

  putStrLn "Starting game loop..."
  _ <- forkIO $ gameLoop queueVar gameStateVar

  putStrLn "Starting server..."
  _ <- forkIO $ runServer gameStateVar loginDB userDB

  putStrLn "Adding test data..."
  addUser userDB "Gyuri" "asdf"
  user <- fromJust <$> getUser userDB "Gyuri" "asdf"
  names1 <- sequence $ replicate 4 nameGenerator
  pushMessage queueVar $ NewVillage "Gyurtown" (0,0) user names1
  names2 <- sequence $ replicate 4 nameGenerator
  pushMessage queueVar $ NewVillage "Peptown" (3,5) user names2
  names3 <- sequence $ replicate 4 nameGenerator
  pushMessage queueVar $ NewVillage "Garbageton" (3,5) (User 42 "Klark" "pass") names3
  putStrLn "Done."

gameLoop :: MessageQueue Event -> MVar GameState -> IO ()
gameLoop queueVar gameStateVar = do
  threadDelay 1000000
  event <- popMessage queueVar
  gameState <- updateGameState gameStateVar event
  --putStrLn $ show event
  --putStrLn $ show gameState
  gameLoop queueVar gameStateVar
