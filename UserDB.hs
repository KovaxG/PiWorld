module UserDB (
  UserDB,
  newUserDB,
  addUser,
  deleteUser,
  getUser,
  showContents
) where

import Control.Concurrent.MVar
import Data.List
import Data.Maybe

import GameTypes
import ServerTypes
import Utils

type UserDB = MVar [User]

newUserDB :: IO UserDB
newUserDB = newMVar []

addUser :: UserDB -> UserName -> Password -> IO ()
addUser userDBVar userName password = do
  userDB <- takeMVar userDBVar
  let lastId = getLastId userDB
  putMVar userDBVar $ User (lastId + 1) userName password : userDB
  where
    getLastId = fromMaybe 0 . safeMax . map uID

deleteUser :: UserDB -> UserName -> IO ()
deleteUser userDBVar user = do
  userDB <- takeMVar userDBVar
  putMVar userDBVar $ filterNot ((== user) . uName) userDB

getUser :: UserDB -> UserName -> Password -> IO (Maybe User)
getUser userDBVar user password = do
  userDB <- readMVar userDBVar
  return . find (\u -> uName u == user && uPass u == password) $ userDB

showContents :: UserDB -> IO ()
showContents userDBVar = do
  userDB <- readMVar userDBVar
  putStrLn "User Database"
  mapM_ (putStrLn. show . details) userDB
  where
    details (User id name pass) = (id, name, pass)
