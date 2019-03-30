import Control.Concurrent

import GameLogic (newGameStateVar, updateGameState)
import GameState (Event(NewVillage), GameState)
import MessageQueue (newEmptyQueue, pushMessage, popMessage, MessageQueue)
import Server (runServer)

main :: IO ()
main = do
  queueVar <- newEmptyQueue
  gameStateVar <- newGameStateVar
  putStrLn "Starting game loop..."
  _ <- forkIO $ gameLoop queueVar gameStateVar
  putStrLn "Starting server..."
  _ <- forkIO $ runServer gameStateVar
  pushMessage queueVar $ NewVillage "Gyurtown" (0,0)
  pushMessage queueVar $ NewVillage "Gyurtown2" (0,0)
  pushMessage queueVar $ NewVillage "Gyurtown3" (0,0)
  pushMessage queueVar $ NewVillage "Gyurtown4" (0,0)

gameLoop :: MessageQueue Event -> MVar GameState -> IO ()
gameLoop queueVar gameStateVar = do
  threadDelay 1000000
  event <- popMessage queueVar
  gameState <- updateGameState gameStateVar event
  --putStrLn $ show event
  --putStrLn $ show gameState
  gameLoop queueVar gameStateVar
