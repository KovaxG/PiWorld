module ServerTypes where

import Utils

type IP = String

type UserName = String
type Password = String
data User = User {
  uID :: ID,
  uName :: UserName,
  uPass ::  Password
}

instance Show User where
  show user = uName user

instance Eq User where
  u1 == u2 = uID u1 == uID u2
