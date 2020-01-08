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
 [a,s,d,f,u]-> \case "u"->u;otherChoise->doChosenFrom [a,s,d,f] otherChoise
 [a,s,d,f]  -> \case "f"->f;otherChoise->doChosenFrom [a,s,d]   otherChoise
 [a,s,d]    -> \case "a"->a;"s"->s;"d"->d;""->waveAndExit;_->confusionAndRoot
-- Action Lists
[addThenRootList,showThenRootList,changeThenRootList,moveFromThenRootList,
 moveToThenRootList,[confusionAndRoot]] =
 forEach rootMenuAfterEachAction
  [addList,showList,changeList,moveFromList,moveToList,[showConfusion]]
-- Present root menu after each action in the action list
rootMenuAfterEachAction = forEach (`andThen`presentRootMenu) 
