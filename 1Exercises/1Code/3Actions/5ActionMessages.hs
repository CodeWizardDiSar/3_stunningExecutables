{-# LANGUAGE LambdaCase #-} 
module ActionOptions where
import Renaming
import Useful
import MenuOptions

showTitle     = "Show"
addTitle      = "Add to"
changeTitle   = "Change"
moveFromTitle = "Move from"
moveToTitle   = "Move to"

toDoOption    = "a: To Do"
doneOption    = "s: Done"
missedOption  = "d: Missed"
allOption     = "f: All"

options         = [toDoOption,doneOption,missedOption,exitOption]
showOptionList  = take 3 options`append`[allOption,exitOption]
showOptions     = titleAndOptions showTitle     showOptionList
addOptions      = titleAndOptions addTitle      options
changeOptions   = titleAndOptions changeTitle   options
moveFromOptions = titleAndOptions moveFromTitle options
moveToOptions   = titleAndOptions moveToTitle   options
