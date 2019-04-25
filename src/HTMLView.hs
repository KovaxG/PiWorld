{-
  This is the source code for the web real time stratgy game, PiWorld.
  It was written to be a hobby project to see how a big project looks
  like in Haskell, and also to learn about web development.
  You are free to look at the code and use it as you see fit, as long
  as it is not for profit. If it is please contact the author, KovaxG
  on gitthub.
  Author(s): KovaxG
-}
module HTMLView where

import GameTypes
import MyHTML
import ServerTypes

htmlWith :: String -> String -> HTML
htmlWith headContent bodyContent =
  startHtml |> html (hed headContent |> body bodyContent)

toHTML :: Response -> HTML
toHTML Unrecognised = ""

toHTML VillageNotFound = "No such village found."

toHTML (DefaultVillageView villageName userName location) =
  htmlWith (title $ getVillageName villageName) (
    addBreak ("Welcome to " ++ getVillageName villageName ++ ".")
    |> addBreak ("This village is managed by " ++ getUserName userName)
    |> addBreak ("This village is at " ++ show location)
    |> getForm "/" (
      addBreak "Click here to return to the main page."
      |> button "Main Page"
    )
  )

toHTML (OwnedVillageView villageName location  villagers buildings inventory) =
  htmlWith (title $ getVillageName villageName) (
    addBreak ("Welcome to your village, " ++ getVillageName villageName ++ ".")
    |> addBreak ("This village is at " ++ show location)
    |> addBreak ("This town has a population of " ++ show (length villagers))
    |> getForm "/person" (
      addBreak . personButton =<< villagers
    )
    |> addBreak "Buildings:"
    |> (addBreak . show =<< buildings)
    |> addBreak ""
    |> addBreak ("Inventory: " ++ show inventory)
    |> getForm "/" (
      addBreak "Click here to return to the main page."
      |> button "Main Page"
    )
  )
  where
    personButton v =
      pName v
      ++ " (" ++ show (pJob v) ++ ") "
      ++ " Hunger:" ++ show (getHunger $ pHunger v)
      ++ " Hitpoints: " ++ show (getHealth $ pHealth v)
      ++ input "submit" (show $ pID v) "Job"

toHTML LogoutPage =
  htmlWith (title "PiWorld Logout") (
    addBreak "Logout Successful"
    |> getForm "/" (
      addBreak "Click here to return to the main page."
      |> button "Main Page"
    )
  )

toHTML (Overview userName villages) =
  htmlWith (title "PiWorld Main Menu") (
    addBreak ("Hello, " ++ getUserName userName)
    |> getForm "" (
      addBreak "Here are your villages"
      |> (addBreak . villageAndButton =<< villages)
    )
    |> addBreak ""
    |> getForm "/map" (
      addBreak "View world map: "
      |> button "World Map"
    )
    |> addBreak ""
    |> getForm "/logout" (
      "Click here to log out:"
      |> button "Log Out"
    )
  )
  where
    villageAndButton (name, id) = getVillageName name ++ input "submit" (show id) "view"

toHTML MainPage =
  htmlWith (title "PiWorld Main Menu") (
    getForm "/login" (
      "You are not logged in, you can do that here:"
      |> addBreak ""
      |> button "Log In"
    )
  )

toHTML (LoginSuccess userName) =
  htmlWith (title "PiWorld Login") (
    addBreak ( "Welcome " ++ getUserName userName )
    |> getForm "/" (
      addBreak ("Click here to go to main view: ")
      |> button "Main Page"
    )
  )

toHTML FailedLogin = "User does not exist"

toHTML (AlreadyLoggedIn userName) =
  htmlWith (title "PiWorld Login") (
    "You are already logged in as " ++ getUserName userName ++ ". Log off to sign in as another user."
  )

toHTML LoginScreen =
  htmlWith (title "PiWorld Login") (
    postForm "" (
      addBreak "Login"
      |> "username: "
      |> input "text" "username" ""
      |> addBreak ""
      |> "password:"
      |> input "text" "password" ""
      |> addBreak ""
      |> button "login"
    )
  )

toHTML (WorldMapScreen villages) =
  htmlWith (title "PiWorld Worldmap") (
    getForm "" (
      addBreak "Game Map"
      |> (addBreak . villageAndButton =<< villages)
    )
  )
  where
    villageAndButton (name, location, id) =
      getVillageName name ++ " " ++ show location ++ " " ++ input "submit" (show id) "view"

toHTML IllegalAction =
  htmlWith (title "Criminal Scum") (
    "Hold right there criminal scum! You violated the law!"
  )

toHTML (PersonJobView name id job availableJobs) =
  htmlWith (title ("Job of " ++ name) ) (
    addBreak ("Name: " ++ name)
    |> addBreak ("Current Job: " ++ show job)
    |> getForm "" (
      addBreak . toInputs =<< availableJobs
    )
  )
  where
    toInputs job = input "submit" (show id) (show job)

toHTML JobChanged =
  htmlWith (title ("Great Success") ) (
    addBreak "Job changed."
    |> getForm "/" (
      addBreak ("Click here to go to main view: ")
      |> button "Main Page"
    )
  )
