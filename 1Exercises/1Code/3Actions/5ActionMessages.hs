{-# LANGUAGE LambdaCase #-} 
module Actions where
import Renaming
import Useful
import RootMenu

toDoOption    = "a: To Do"
doneOption    = "s: Done"
missedOption  = "d: Missed"
allOption     = "f: All"
options       = [toDoOption,doneOption,missedOption,exitOption]
showOptions   = take 3 options`append`[allOption,exitOption]

showTitle     = "Show"
addTitle      = "Add to"
changeTitle   = "Change"
moveFromTitle = "Move from"
moveToTitle   = "Move to"

addMenu         = titleAndOptions addTitle      options
showMenu        = titleAndOptions showTitle     showOptions
changeMenu      = titleAndOptions changeTitle   options
moveFromMenu    = titleAndOptions moveFromTitle options
moveToMenu      = titleAndOptions moveToTitle   options
