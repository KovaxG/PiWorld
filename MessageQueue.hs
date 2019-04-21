{-
  This is the source code for the web real time stratgy game, PiWorld.
  It was written to be a hobby project to see how a big project looks
  like in Haskell, and also to learn about web development.
  You are free to look at the code and use it as you see fit, as long
  as it is not for profit. If it is please contact the author, KovaxG
  on gitthub.
  Author(s): KovaxG
-}
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
