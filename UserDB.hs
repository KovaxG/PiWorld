module UserDB (
  UserDB,
  newUserDB,
  addUser,
  deleteUser,
  checkUser
) where

import Control.Concurrent.MVar

import ServerTypes

type UserDB = MVar [User]

newUserDB :: IO UserDB
newUserDB = newMVar []

addUser :: UserDB -> User -> IO ()
addUser userDBVar user = do
  userDB <- takeMVar userDBVar
  putMVar userDBVar $ user : userDB

deleteUser :: UserDB -> UserName -> IO ()
deleteUser userDBVar user = do
  userDB <- takeMVar userDBVar
  putMVar userDBVar $ filterNot (\(User name _) -> name == user) userDB

checkUser :: UserDB -> UserName -> Password -> IO Bool
checkUser userDBVar user password = do
  userDB <- readMVar userDBVar
  return $ not $ null $ filter (\(User name pass) -> name == user && pass == password) userDB

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)
