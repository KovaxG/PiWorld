module LoginDB (
  LoginDB,
  newLoginDB,
  bindIpWithUser,
  getUserFromIp,
  unbindIp
) where

import Data.Map
import Control.Concurrent.MVar

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
