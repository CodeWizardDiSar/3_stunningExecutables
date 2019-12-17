{-# LANGUAGE LambdaCase #-} 
module Main where
import Prelude hiding (all,show)
import Data.Function
import Control.Arrow
import Types
import General
import FcsToExs
import ExsToFcs
import Renaming
import MenuOptions
import ActionOptions
import Useful
import Show
import Add
import Change
import Move

main          = welcome`andThen`showMenu
welcome       = emptyLine`andThen`printString welcomingMessage
showMenu      = showOptionsAndRespond menuOptions     menuActions
menuActions   = [add,show,change,moveFrom]
add           = showOptionsAndRespond addOptions      addActions     
show          = showOptionsAndRespond showOptions     showActions    
change        = showOptionsAndRespond changeOptions   changeActions  
moveFrom      = showOptionsAndRespond moveFromOptions moveFromActions
moveTo        = showOptionsAndRespond moveToOptions   moveToActions  
showOptionsAndRespond = \x y-> askFor x`unwrapAnd`respondWithOneOf y
respondWithOneOf =
  \case [a,s,d,f]-> \case "a"->a;"s"->s;"d"->d;"f"->f;"" ->waveAndExit
                          _  ->showConfusion`andThen`showMenu
        [a,s,d]  -> \case "a"->a;"s"->s;"d"->d;"" ->waveAndExit
                          _  ->showConfusion`andThen`showMenu
        _        -> printErrorMessage "Not 3 or 4 options"
waveAndExit   = printString byeMessage
showConfusion = printString showConfusionMessage

addActions      = showMenuAfterEach [showToDo,showDone,showMissed]
showActions     = showMenuAfterEach [showToDo,showDone,showMissed,showAll]
changeActions   = showMenuAfterEach [showToDo,showDone,showMissed]
moveFromActions = showMenuAfterEach [showToDo,showDone,showMissed]
moveToActions   = showMenuAfterEach [showToDo,showDone,showMissed]
showMenuAfterEach = (`andThen`showMenu)&forEach 
