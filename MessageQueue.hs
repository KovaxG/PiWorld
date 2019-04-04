module MessageQueue (
  MessageQueue,
  newEmptyQueue,
  popMessages,
  pushMessage
) where

import Control.Concurrent.MVar

type MessageQueue a = MVar [a]

newEmptyQueue :: IO (MessageQueue a)
newEmptyQueue = newMVar []

popMessages :: MessageQueue a -> IO [a]
popMessages msgQueue = do
  queue <- takeMVar msgQueue
  putMVar msgQueue []
  return queue

pushMessage :: MessageQueue a -> a -> IO ()
pushMessage msgQueue msg = do
  queue <- takeMVar msgQueue
  putMVar msgQueue (queue ++ [msg])
