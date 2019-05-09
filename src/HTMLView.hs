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
import Utils

toPath :: String -> String
toPath s = "src/views/" ++ s ++ ".html"

htmlWith :: String -> String -> HTML
htmlWith headContent bodyContent =
  startHtml |> html (hed headContent |> body bodyContent)

toHTML :: Response -> IO HTML
toHTML Unrecognised = return ""

toHTML VillageNotFound = return "No such village found."

toHTML (DefaultVillageView villageName userName location) = do
  contents <- readFile $ toPath "DefaultVillageView"
  return $ interpolateString vars contents
  where
    vars = [
      ("villageName", getVillageName villageName),
      ("userName", getUserName userName),
      ("location", show location)]

toHTML (OwnedVillageView villageName location  villagers buildings inventory) = do
  contents <- readFile $ toPath "OwnedVillageView"
  return $ interpolateList lists $ interpolateString strings contents
  where
    strings = [
      ("villageName", getVillageName villageName),
      ("location", show location),
      ("population", show $ length villagers),
      ("inventory", show inventory)]

    lists = [
      (["buildings"], buildingList),
      (["vName", "vJob", "vHunger", "vHealth", "vStatus", "vId"], personList)]

    buildingList = fmap (pure . show) buildings
    personList =
      fmap (\v ->
        fmap ($v) [
          pName,
          show . pJob,
          show . getHunger . pHunger,
          show . getHealth . pHealth,
          show . pStatus,
          show . pID
          ]
      ) villagers

toHTML LogoutPage = readFile $ toPath "LogoutView"

toHTML (Overview userName villages) = do
  contents <- readFile $ toPath "OverviewView"
  return $ interpolateList lists $ interpolateString vars contents
  where
    vars = [("userName", getUserName userName)]
    lists = [(["vName", "vId"], villageList)]
    villageList =  fmap (\v -> fmap ($v) [getVillageName . fst, show . snd]) villages

toHTML MainPage = return $
  htmlWith (title "PiWorld Main Menu") (
    getForm "/login" (
      "You are not logged in, you can do that here:"
      |> addBreak ""
      |> button "Log In"
    )
  )

toHTML (LoginSuccess userName) = return $
  htmlWith (title "PiWorld Login") (
    addBreak ( "Welcome " ++ getUserName userName )
    |> getForm "/" (
      addBreak ("Click here to go to main view: ")
      |> button "Main Page"
    )
  )

toHTML FailedLogin = return "User does not exist"

toHTML (AlreadyLoggedIn userName) = return $
  htmlWith (title "PiWorld Login") (
    "You are already logged in as " ++ getUserName userName ++ ". Log off to sign in as another user."
  )

toHTML LoginScreen = return $
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

toHTML (WorldMapScreen villages) = return $
  htmlWith (title "PiWorld Worldmap") (
    getForm "" (
      addBreak "Game Map"
      |> (addBreak . villageAndButton =<< villages)
    )
  )
  where
    villageAndButton (name, location, id) =
      getVillageName name ++ " " ++ show location ++ " " ++ input "submit" (show id) "view"

toHTML IllegalAction = return $
  htmlWith (title "Criminal Scum") (
    "Hold right there criminal scum! You violated the law!"
  )

toHTML (PersonJobView name id job availableJobs) = return $
  htmlWith (title ("Job of " ++ name) ) (
    addBreak ("Name: " ++ name)
    |> addBreak ("Current Job: " ++ show job)
    |> getForm "" (
      addBreak . toInputs =<< availableJobs
    )
  )
  where
    toInputs job = input "submit" (show id) (show job)

toHTML JobChanged = return $
  htmlWith (title ("Great Success") ) (
    addBreak "Job changed."
    |> getForm "/" (
      addBreak ("Click here to go to main view: ")
      |> button "Main Page"
    )
  )
