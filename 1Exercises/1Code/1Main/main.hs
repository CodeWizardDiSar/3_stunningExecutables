{-# LANGUAGE LambdaCase #-} 
module Main where
import Prelude hiding (all,show)
import Data.Function
import Control.Arrow
import Types hiding (show)
import General
import FcsToExs
import ExsToFcs
import Renaming
import RootMenu
import Actions
import Useful
import Show
import Add
import Change
import Move

main               = welcome`andThen`showRootMenu
welcome            = emptyLine`andThen`printString welcomingMessage
welcomingMessage   = tabBefore "Welcome to the exercises manager"
showRootMenu       = showMenuAndRespond rootMenu     rootActions
rootActions        = [add,show,change,moveFrom]
add                = showMenuAndRespond addMenu      addActions     
show               = showMenuAndRespond showMenu     showActions    
change             = showMenuAndRespond changeMenu   changeActions  
moveFrom           = showMenuAndRespond moveFromMenu moveFromActions
moveTo             = showMenuAndRespond moveToMenu   moveToActions  
showMenuAndRespond = \x y-> askFor x`unwrapAnd`respondWithOneOf y
respondWithOneOf   =
  \case [a,s,d,f]-> \case "a"->a;"s"->s;"d"->d;"f"->f;"" ->waveAndExit
                          _  ->showConfusion`andThen`showRootMenu
        [a,s,d]  -> \case "a"->a;"s"->s;"d"->d;"" ->waveAndExit
                          _  ->showConfusion`andThen`showRootMenu
        _        -> printErrorMessage "Not 3 or 4 options"
waveAndExit        = printString "bye!"
showConfusion      = printString "Ehhh what?"

addActions      = rootMenuAfterEach [showToDo,showDone,showMissed]
showActions     = rootMenuAfterEach [showToDo,showDone,showMissed,showAll]
changeActions   = rootMenuAfterEach [showToDo,showDone,showMissed]
moveFromActions = rootMenuAfterEach [showToDo,showDone,showMissed]
moveToActions   = rootMenuAfterEach [showToDo,showDone,showMissed]
rootMenuAfterEach = (`andThen`showRootMenu)&forEach 
