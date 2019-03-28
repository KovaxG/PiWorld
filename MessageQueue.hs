module MessageQueue (
  MessageQueue,
  newEmptyQueue,
  popMessage,
  pushMessage
) where

import Control.Concurrent.MVar

type MessageQueue a = MVar [a]

newEmptyQueue :: IO (MessageQueue a)
newEmptyQueue = newMVar []

popMessage :: MessageQueue a -> IO (Maybe a)
popMessage msgQueue = do
  queue <- takeMVar msgQueue
  let (event, rest) = getHead queue
  putMVar msgQueue rest
  return event

pushMessage :: MessageQueue a -> a -> IO ()
pushMessage msgQueue msg = do
  queue <- takeMVar msgQueue
  putMVar msgQueue (queue ++ [msg])

getHead :: [a] -> (Maybe a, [a])
getHead [] = (Nothing, [])
getHead as = (Just $ head as, tail as)
