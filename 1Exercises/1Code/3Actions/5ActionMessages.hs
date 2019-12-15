{-# LANGUAGE LambdaCase #-} 
module ActionMessages where
import Useful
import Messages

showTitle     = "Show"
addTitle      = "Add to"
changeTitle   = "Change"
moveFromTitle = "Move from"
moveToTitle   = "Move to"
toDoOption    = "a: To Do"
doneOption    = "s: Done"
missedOption  = "d: Missed"
allOption     = "f: All"

options          = [toDoOption,doneOption,missedOption,exitOption]
showOptions      = take 3 options ++ [allOption,exitOption]
showMessages     = titleAndOptions showTitle  showOptions    
addMessages      = titleAndOptions addTitle  options
changeMessages   = titleAndOptions changeTitle  options
moveFromMessages = titleAndOptions moveFromTitle options 
moveToMessages   = titleAndOptions moveToTitle options  
