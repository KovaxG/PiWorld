import Control.Concurrent
import Control.Concurrent.MVar

import MessageQueue
import GameLogic

main :: IO ()
main = do
  queueVar <- newEmptyQueue
  _ <- forkIO $ gameLoop queueVar newState
  _ <- getLine
  pushMessage queueVar $ NewVillage (0,0)

gameLoop :: MessageQueue Event -> GameState -> IO ()
gameLoop queueVar gameState = do
  event <- popMessage queueVar
  putStrLn $ show event
  putStrLn $ show gameState
  threadDelay 1000000
  gameLoop queueVar (tick event gameState)
