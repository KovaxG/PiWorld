module ServerTypes where

import Data.Map

import GameTypes
import Utils

type IP = String

data GetRequest = Get String (Map String String) deriving (Show)

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
