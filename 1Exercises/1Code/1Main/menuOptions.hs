{-# LANGUAGE LambdaCase #-} 
module MenuOptions where
import Data.Function
import Useful
import Renaming

commandMeTitle = "Command me master"
addOption      = "a: Add"            
showOption     = "s: Show"
changeOption   = "d: Change"
moveOption     = "f: Move"
exitOption     = "enter: Exit"

menuOptions = titleAndOptions commandMeTitle [addOption
                                             ,showOption
                                             ,changeOption
                                             ,moveOption
                                             ,exitOption]
welcomingMessage     = tabBefore "Welcome to the exercises manager"
byeMessage           = "bye!"
showConfusionMessage = "Ehhh what?"
