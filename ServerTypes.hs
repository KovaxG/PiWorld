module ServerTypes where

type IP = String

type UserName = String
type Password = String
data User = User UserName Password

instance Show User where
  show (User name _) = name
