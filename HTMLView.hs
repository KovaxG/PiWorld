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
    |> form "/" (
      addBreak "Click here to return to the main page."
      |> button "Main Page"
    )
  )

toHTML (OwnedVillageView villageName location  villagers inventory) =
  htmlWith (title $ getVillageName villageName) (
    addBreak ("Welcome to your village, " ++ getVillageName villageName ++ ".")
    |> addBreak ("This village is at " ++ show location)
    |> addBreak ("This town has a population of " ++ show (length villagers))
    |> form "/person" (
      addBreak . personButton =<< villagers
    )
    |> addBreak ""
    |> addBreak ("Inventory: " ++ show inventory)
    |> form "/" (
      addBreak "Click here to return to the main page."
      |> button "Main Page"
    )
  )
  where
    personButton (name, job, id, hunger, health) =
      name
      ++ " (" ++ show job ++ ") "
      ++ " Hunger:" ++ show hunger
      ++ " Hitpoints: " ++ show health
      ++ input "submit" (show id) "Job"

toHTML LogoutPage =
  htmlWith (title "PiWorld Logout") (
    addBreak "Logout Successful"
    |> form "/" (
      addBreak "Click here to return to the main page."
      |> button "Main Page"
    )
  )

toHTML (Overview userName villages) =
  htmlWith (title "PiWorld Main Menu") (
    addBreak ("Hello, " ++ show userName)
    |> form "" (
      addBreak "Here are your villages"
      |> (addBreak . villageAndButton =<< villages)
    )
    |> addBreak ""
    |> form "/map" (
      addBreak "View world map: "
      |> button "World Map"
    )
    |> addBreak ""
    |> form "/logout" (
      "Click here to log out:"
      |> button "Log Out"
    )
  )
  where
    villageAndButton (name, id) = getVillageName name ++ input "submit" (show id) "view"

toHTML MainPage =
  htmlWith (title "PiWorld Main Menu") (
    form "/login" (
      "You are not logged in, you can do that here:"
      |> addBreak ""
      |> button "Log In"
    )
  )

toHTML (LoginSuccess userName) =
  htmlWith (title "PiWorld Login") (
    addBreak ( "Welcome " ++ show userName )
    |> form "/" (
      addBreak ("Click here to go to main view: ")
      |> button "Main Page"
    )
  )

toHTML FailedLogin = "User does not exist"

toHTML (AlreadyLoggedIn userName) =
  htmlWith (title "PiWorld Login") (
    "You are already logged in as " ++ show userName ++ ". Log off to sign in as another user."
  )

toHTML LoginScreen =
  htmlWith (title "PiWorld Login") (
    form "" (
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
    form "" (
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
    |> form "" (
      addBreak . toInputs =<< availableJobs
    )
  )
  where
    toInputs job = input "submit" (show id) (show job)

toHTML JobChanged =
  htmlWith (title ("Great Success") ) (
    addBreak "Job changed."
    |> form "/" (
      addBreak ("Click here to go to main view: ")
      |> button "Main Page"
    )
  )
