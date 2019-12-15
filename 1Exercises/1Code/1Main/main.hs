{-# LANGUAGE LambdaCase #-} 
module Main where
import Prelude hiding (all)
import Data.Function
import Control.Arrow
import Types
import General
import FcsToExs
import ExsToFcs
import Renaming
import Messages
import ActionMessages
import Useful
import Show
import Add
import Change
import Move

main          = 2&newLines>>printString welcomingMessage>>printMenu
printMenu     = printGetAndDo4 menuMessages menuActions

menuActions   = [add,showExs,change,moveFrom]
add           = printGetAndDo3 addMessages      addActions     
showExs       = printGetAndDo4 showMessages     showActions    
change        = printGetAndDo3 changeMessages   changeActions  
moveFrom      = printGetAndDo3 moveFromMessages moveFromActions
moveTo        = printGetAndDo3 moveToMessages   moveToActions  

printGetAndDo4 = \x y-> printAndGet x>>=doAsAsked4 y
printGetAndDo3 = \x y-> printAndGet x>>=doAsAsked3 y

doAsAsked4 = \[a,s,d,f]->
  \case "a"->a;"s"->s;"d"->d;"f"->f;
        "" ->waveAndExit
        _  ->showConfusion&thenPrintMenu

doAsAsked3 = \[a,s,d]->
  \case "a"->a;"s"->s;"d"->d
        "" ->waveAndExit
        _  ->showConfusion&thenPrintMenu

waveAndExit   = printString byeMessage
showConfusion = printString showConfusionMessage

showActions     = printMenuAfterEach [showToDo,showDone,showMissed,showAll]
addActions      = printMenuAfterEach [showToDo,showDone,showMissed]
changeActions   = printMenuAfterEach [showToDo,showDone,showMissed]
moveFromActions = printMenuAfterEach [showToDo,showDone,showMissed]
moveToActions   = printMenuAfterEach [showToDo,showDone,showMissed]

printMenuAfterEach = map thenPrintMenu
thenPrintMenu      = (>>printMenu) 
