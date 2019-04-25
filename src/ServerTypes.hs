{-
  This is the source code for the web real time stratgy game, PiWorld.
  It was written to be a hobby project to see how a big project looks
  like in Haskell, and also to learn about web development.
  You are free to look at the code and use it as you see fit, as long
  as it is not for profit. If it is please contact the author, KovaxG
  on gitthub.
  Author(s): KovaxG
-}
module ServerTypes where

import Data.Map

import GameTypes
import Utils

type IP = String

data GetRequest = Get String [(String, String)]
                | Post [(String, String)]
                deriving (Show)

data Response = DefaultVillageView VillageName UserName Location
              | OwnedVillageView VillageName Location [Person] [Building] Inventory
              | VillageNotFound
              | Unrecognised
              | LogoutPage
              | Overview UserName [(VillageName, ID)]
              | MainPage
              | FailedLogin
              | LoginSuccess UserName
              | AlreadyLoggedIn UserName
              | LoginScreen
              | WorldMapScreen [(VillageName, Location, ID)]
              | IllegalAction
              | PersonJobView Name ID Job [Job]
              | JobChanged
              deriving (Show)

data Request = MainMenu
             | WorldMap
             | LoginPage
             | Login UserName Password
             | Logout
             | ViewVillage ID
             | JobMenu ID
             | JobChange ID Job
             deriving (Show)