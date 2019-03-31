module ServerTypes where

type IP = String
type ID = Int

type UserName = String
type Password = String
data User = User {
  uID :: ID,
  uName :: UserName,
  uPass ::  Password
}

instance Show User where
  show user = uName user
