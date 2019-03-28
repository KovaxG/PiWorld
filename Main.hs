import Control.Concurrent
import Control.Concurrent.MVar

import MessageQueue

data Event = Event deriving (Show)

type Queue = [Event]

data GameState = GameState {
  gTickNr :: Int
} deriving (Show)

tick :: Maybe Event -> GameState -> GameState
tick Nothing g = g { gTickNr = gTickNr g + 1 }
tick (Just _) g = g { gTickNr = 0 }

startState = GameState { gTickNr = 0 }

main :: IO ()
main = do
  queueVar <- newEmptyQueue
  _ <- forkIO $ gameLoop queueVar startState
  _ <- getLine
  pushMessage queueVar Event

gameLoop :: MessageQueue Event -> GameState -> IO ()
gameLoop queueVar gameState = do
  event <- popMessage queueVar
  putStrLn $ show event
  putStrLn $ show gameState
  threadDelay 1000000
  gameLoop queueVar (tick event gameState)
