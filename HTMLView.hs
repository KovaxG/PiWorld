module HTMLView where

import MyHTML
import ServerTypes

htmlWith :: String -> String -> HTML
htmlWith headContent bodyContent =
  startHtml |> html (hed headContent |> body bodyContent)

toHTML :: Response -> HTML
toHTML Unrecognised = ""

toHTML VillageNotFound = "No such village found."

toHTML (DefaultVillageView villageName userName location) =
  htmlWith (title villageName) (
    addBreak ("Welcome to " ++ villageName ++ ".")
    |> addBreak ("This village is managed by " ++ userName)
    |> addBreak ("This village is at " ++ show location)
    |> form "/" (
      addBreak "Click here to return to the main page."
      |> button "Main Page"
    )
  )

toHTML (OwnedVillageView villageName location  villagers inventory) =
  htmlWith (title villageName) (
    addBreak ("Welcome to your village, " ++ villageName ++ ".")
    |> addBreak ("This village is at " ++ show location)
    |> addBreak ("This town has a population of " ++ show (length villagers))
    |> (addBreak . show =<< villagers)
    |> addBreak ""
    |> addBreak ("Inventory: " ++ show inventory)
    |> form "/" (
      addBreak "Click here to return to the main page."
      |> button "Main Page"
    )
  )

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
    addBreak ("Hello, " ++ userName)
    |> form "" (
      addBreak "Here are your villages"
      |> (villageAndButton =<< villages)
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
    villageAndButton (name, id) = name ++ " " ++ input "submit" (show id) "view" ++ "\n"

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
    addBreak ( "Welcome " ++ userName )
    |> form "/" (
      addBreak ("Click here to go to main view: ")
      |> button "Main Page"
    )
  )

toHTML FailedLogin = "User does not exist"

toHTML (AlreadyLoggedIn userName) =
  htmlWith (title "PiWorld Login") (
    "You are already logged in as " ++ userName ++ ". Log off to sign in as another user."
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
      |> (villageAndButton =<< villages)
    )
  )
  where
    villageAndButton (name, location, id) =
      addBreak $ name ++ " " ++ show location ++ " " ++ input "submit" (show id) "view"
