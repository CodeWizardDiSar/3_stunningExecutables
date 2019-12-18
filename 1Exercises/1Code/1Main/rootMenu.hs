{-# LANGUAGE LambdaCase #-} 
module RootMenu where
import Data.Function
import Useful
import Renaming

commandMeTitle = "Command me master"
addOption      = "a: Add"            
showOption     = "s: Show"
changeOption   = "d: Change"
moveOption     = "f: Move"
exitOption     = "enter: Exit"

rootMenu = titleAndOptions commandMeTitle [addOption
                                          ,showOption
                                          ,changeOption
                                          ,moveOption
                                          ,exitOption]
