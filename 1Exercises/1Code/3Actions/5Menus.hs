{-# LANGUAGE LambdaCase #-} 
module Menus where
import Renaming
import Useful

rootMenu       = titleAndOptions commandMeTitle rootOptions
commandMeTitle = "Command me master"
rootOptions    = [addOption,showOption,changeOption,moveOption,exitOption]
addOption      = "a: Add"            
showOption     = "s: Show"
changeOption   = "d: Change"
moveOption     = "f: Move"
exitOption     = "enter: Exit"

addMenu        = titleAndOptions addTitle      options
addTitle       = "Add to"
options        = [toDoOption,doneOption,missedOption,exitOption]
toDoOption     = "a: To Do"
doneOption     = "s: Done"
missedOption   = "d: Missed"
allOption      = "f: All"
showMenu       = titleAndOptions showTitle showOptions
showTitle      = "StringFrom"
showOptions    = take 3 options`append`[allOption,exitOption]
changeMenu     = titleAndOptions changeTitle   options
changeTitle    = "Change"
moveFromMenu   = titleAndOptions moveFromTitle options
moveFromTitle  = "Move from"
moveToMenu     = titleAndOptions moveToTitle   options
moveToTitle    = "Move to"
