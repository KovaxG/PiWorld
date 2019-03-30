module MyHTML where

type HTML = String

startHtml = tag "!DOCTYPE html"

input :: String -> String -> String -> HTML
input inputType inputName valueName = tag $
  "input type=" ++ quoted inputType ++
  " name=" ++ quoted inputName ++
  " value=" ++ quoted valueName

html  = addTags "html"
title = addTags "title"
hed = addTags "head" -- head is an existing function :(
body  = addTags "body"
form  = addTags "form"
h i = addTags $ "h" ++ show i
code = addTags "pre" . addTags "code"
hline = tag "hr"
tag s = "<" ++ s ++ ">"
addTags tagName s = mconcat [tag tagName, "\n", s, "\n", tag ('/':tagName)]
addBreak = (++ "<br>")
allowedCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ "!~-,_ +"
quoted s = "\"" ++ s ++ "\""
a |> b = a ++ "\n" ++ b
