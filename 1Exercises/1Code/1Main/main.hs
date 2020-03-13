module Main where
import Renaming         (forEach,printString,andThen,unwrapAnd,append,and)
import UsefulFunctions  (doSequentially)
import UsefulFunctions  (printEmptyLine,tabBefore)
import Prelude          (getLine)
import FileManagement   (getCurrentDataKeeper,downdateVersion,dataKeeperPrefix)
import Menus            (rootMenu,addMenu,showMenu,editMenu,deleteMenu)
import Menus            (moveMenu)
import Show             (showList)
import Add              (addList)
import Edit             (editList)
import Delete           (deleteList)
import Move             (moveList)
import System.Directory (removeFile)

-- Main baby
main = doSequentially [printEmptyLine,printWelcomingMessage,root] 

-- Exciting actions
[root,add,show,edit,delete,move] =
 forEach menuAndChosen 
  [(rootMenu,rootList)            ,(addMenu,addThenRootList)  ,
   (showMenu,showThenRootList)    ,(editMenu,editThenRootList),
   (deleteMenu,deleteThenRootList),(moveMenu,moveThenRootList)]

-- Print menu and do chosen from action list
menuAndChosen = \(menu,actionList)->
 printString menu`andThen`getLine`unwrapAnd`
 doChosenFrom actionList

-- Do Chosen From Action List
doChosenFrom = \case
 [a1,a2,a3,a4,a5,a6]-> \case
  "6"        ->a6
  otherChoise->doChosenFrom [a1,a2,a3,a4,a5] otherChoise
 [a1,a2,a3,a4,a5]   -> \case
  "5"        ->a5
  otherChoise->doChosenFrom [a1,a2,a3,a4]    otherChoise
 [a1,a2,a3,a4]      -> \case
  "4"        ->a4
  otherChoise->doChosenFrom [a1,a2,a3]       otherChoise
 [a1,a2,a3]      -> \case
  "1"->a1;"2"->a2;"3"->a3
  "" ->waveAndExit
  _  ->confusionAndRoot

-- Action Lists
[addThenRootList,showThenRootList,editThenRootList,
 deleteThenRootList,moveThenRootList,[confusionAndRoot]] =
 forEach rootMenuAfterEachAction
  [addList,showList,editList,deleteList,
   moveList,[showConfusion]]
rootList = [add,show,edit,delete,move,undo]
rootMenuAfterEachAction = forEach (`andThen`root) 
undo =
 getCurrentDataKeeper`unwrapAnd`
 removeFile`andThen`downdateVersion`andThen`root

-- Printing strings
[printWelcomingMessage,waveAndExit,showConfusion] =
 forEach printString
  [tabBefore "Welcome to the exercises manager","bye!","Ehhh what?"]
