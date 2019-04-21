{-
  This is the source code for the web real time stratgy game, PiWorld.
  It was written to be a hobby project to see how a big project looks
  like in Haskell, and also to learn about web development.
  You are free to look at the code and use it as you see fit, as long
  as it is not for profit. If it is please contact the author, KovaxG
  on gitthub.
  Author(s): KovaxG
-}
module MyHTML where

type HTML = String

startHtml = tag "!DOCTYPE html"

form :: String -> HTML -> HTML
form action body =
  tag ("form action=" ++ action)
  |> body
  |> endTag "form"

input :: String -> String -> String -> HTML
input inputType inputName valueName = tag $
  "input type=" ++ quoted inputType ++
  " name=" ++ quoted inputName ++
  " value=" ++ quoted valueName

button :: String -> HTML
button name = input "submit" "button" name

html  = addTags "html"
title = addTags "title"
hed = addTags "head" -- head is an existing function :(
body  = addTags "body"
h i = addTags $ "h" ++ show i
code = addTags "pre" . addTags "code"
hline = tag "hr"
tag s = "<" ++ s ++ ">"
endTag s = tag ("/" ++ s)
addTags tagName s = mconcat [tag tagName, "\n", s, "\n", tag ('/':tagName)]
addBreak = (++ "<br>")
allowedCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ "!~-,_ +"
quoted s = "\"" ++ s ++ "\""
a |> b = a ++ "\n" ++ b
