{-
  This is the source code for the web real time stratgy game, PiWorld.
  It was written to be a hobby project to see how a big project looks
  like in Haskell, and also to learn about web development.
  You are free to look at the code and use it as you see fit, as long
  as it is not for profit. If it is please contact the author, KovaxG
  on gitthub.
  Author(s): KovaxG
-}
module LoginDB (
  LoginDB,
  newLoginDB,
  bindIpWithUser,
  getUserFromIp,
  unbindIp,
  showContents
) where

import Data.Map
import Control.Concurrent.MVar

import GameTypes
import ServerTypes

type LoginDB = MVar (Map IP User)

newLoginDB :: IO LoginDB
newLoginDB = newMVar empty

bindIpWithUser :: LoginDB -> IP -> User -> IO ()
bindIpWithUser loginDBVar ip user = do
  loginDB <- takeMVar loginDBVar
  putMVar loginDBVar $ insert ip user loginDB

getUserFromIp :: LoginDB -> IP -> IO (Maybe User)
getUserFromIp loginDBVar ip = do
  loginDB <- readMVar loginDBVar
  return $ Data.Map.lookup ip loginDB

unbindIp :: LoginDB -> IP -> IO ()
unbindIp loginDBVar ip = do
  loginDB <- takeMVar loginDBVar
  putMVar loginDBVar $ delete ip loginDB

showContents :: LoginDB -> IO ()
showContents loginDBVar = do
  loginDB <- readMVar loginDBVar
  putStrLn "Login Database"
  mapM_ (putStrLn . show) $ toList loginDB
