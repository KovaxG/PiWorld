import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString.Char8 (pack, unpack)
import Network.Simple.TCP

import MessageQueue
import GameLogic

main :: IO ()
main = do
  queueVar <- newEmptyQueue
  gameStateVar <- newGameStateVar
  _ <- forkIO $ gameLoop queueVar gameStateVar
  _ <- forkIO $ runServer gameStateVar
  pushMessage queueVar $ NewVillage (0,0)

runServer :: MVar GameState -> IO ()
runServer gameStateVar =
  serve (Host "127.0.0.1") "80" $ \(socket, _) -> do
    msg <- maybe "" unpack <$> recv socket 1024
    putStrLn msg
    gameState <- readMVar gameStateVar
    send socket $ pack $ show $ gameState

gameLoop :: MessageQueue Event -> MVar GameState -> IO ()
gameLoop queueVar gameStateVar = do
  threadDelay 1000000
  event <- popMessage queueVar
  gameState <- updateGameState gameStateVar event
  --putStrLn $ show event
  putStrLn $ show gameState
  gameLoop queueVar gameStateVar
