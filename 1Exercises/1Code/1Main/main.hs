module Main where
import Renaming        (forEach,printString,andThen,unwrapAnd,append,and)
import UsefulFunctions (doSequentially)
import UsefulFunctions (printEmptyLine,tabBefore)
import Prelude         (getLine)
import FileManagement  (getVersion,downdateVersion,dataKeeperPrefix)
import Menus           (rootMenu,addMenu,showMenu,changeMenu)
import Menus           (moveFromMenu,moveToMenu)
import Show            (showList)
import Add             (addList)
import Change          (changeList)
import Move            (moveFromList,moveToList)
import System.Directory (removeFile)

-- Main baby
main = doSequentially
 [printEmptyLine,printWelcomingMessage,presentRootMenu] 
-- Printing strings
[printWelcomingMessage,waveAndExit,showConfusion] = forEach printString
 [tabBefore "Welcome to the exercises manager","bye!","Ehhh what?"]
-- Exciting actions
[presentRootMenu,add,show,change,moveFrom,moveTo] = forEach menuAndChosen 
  [(rootMenu    ,rootList            ),(addMenu   ,addThenRootList   ),
   (showMenu    ,showThenRootList    ),(changeMenu,changeThenRootList),
   (moveFromMenu,moveFromThenRootList),(moveToMenu,moveToThenRootList)]
rootList = [add,show,change,moveFrom,undo]
undo =
 getVersion`unwrapAnd`((dataKeeperPrefix`append`)`and`removeFile)`andThen`
 downdateVersion`andThen`presentRootMenu
-- Print menu and do chosen from action list
menuAndChosen = \(menu,actionList)->
 printString menu`andThen`getLine`unwrapAnd`doChosenFrom actionList
-- Do Chosen From Action List
doChosenFrom = \case
 [a1,a2,a3,a4,a5]-> \case "5"->a5;otherChoise->doChosenFrom [a1,a2,a3,a4] otherChoise
 [a1,a2,a3,a4]   -> \case "4"->a4;otherChoise->doChosenFrom [a1,a2,a3]    otherChoise
 [a1,a2,a3]      -> \case "1"->a1;"2"->a2;"3"->a3;""->waveAndExit;_->confusionAndRoot
-- Action Lists
[addThenRootList,showThenRootList,changeThenRootList,moveFromThenRootList,
 moveToThenRootList,[confusionAndRoot]] =
 forEach rootMenuAfterEachAction
  [addList,showList,changeList,moveFromList,moveToList,[showConfusion]]
-- Present root menu after each action in the action list
rootMenuAfterEachAction = forEach (`andThen`presentRootMenu) 
