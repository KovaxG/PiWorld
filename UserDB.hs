{-
  This is the source code for the web real time stratgy game, PiWorld.
  It was written to be a hobby project to see how a big project looks
  like in Haskell, and also to learn about web development.
  You are free to look at the code and use it as you see fit, as long
  as it is not for profit. If it is please contact the author, KovaxG
  on gitthub.
  Author(s): KovaxG
-}
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
