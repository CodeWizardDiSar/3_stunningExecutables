module Main where
import Renaming         (forEach,printString,andThen,unwrapAnd,append,and)
import UsefulFunctions  (doSequentially)
import UsefulFunctions  (printEmptyLine,tabBefore)
import Prelude          (getLine)
import FileManagement   (getCurrentDataKeeper,downdateVersion,dataKeeperPrefix)
import Menus            (rootMenu,addMenu,showMenu,deleteMenu)
import Menus            (moveFromMenu,moveToMenu)
import Show             (showList)
import Add              (addList)
import Delete           (deleteList)
import Move             (moveFromList,moveToList)
import System.Directory (removeFile)

-- Main baby
main = doSequentially
 [printEmptyLine,printWelcomingMessage,presentRootMenu] 

-- Exciting actions
[presentRootMenu,add   ,
 show           ,delete,
 moveFrom       ,moveTo] =
 forEach menuAndChosen 
  [(rootMenu    ,rootList            ),(addMenu   ,addThenRootList   ),
   (showMenu    ,showThenRootList    ),(deleteMenu,deleteThenRootList),
   (moveFromMenu,moveFromThenRootList),(moveToMenu,moveToThenRootList)]

-- Print menu and do chosen from action list
menuAndChosen = \(menu,actionList)->
 printString menu`andThen`
 getLine         `unwrapAnd`
 doChosenFrom actionList

-- Do Chosen From Action List
doChosenFrom = \case
 [a1,a2,a3,a4,a5]-> \case
  "5"        ->a5
  otherChoise->doChosenFrom [a1,a2,a3,a4] otherChoise
 [a1,a2,a3,a4]   -> \case
  "4"        ->a4
  otherChoise->doChosenFrom [a1,a2,a3]    otherChoise
 [a1,a2,a3]      -> \case
  "1"->a1;"2"->a2;"3"->a3
  "" ->waveAndExit
  _  ->confusionAndRoot

-- Action Lists
[addThenRootList   ,showThenRootList    ,
 deleteThenRootList,moveFromThenRootList,
 moveToThenRootList,[confusionAndRoot]  ] =
 forEach rootMenuAfterEachAction
  [addList   ,showList       ,
   deleteList,moveFromList   ,
   moveToList,[showConfusion]]

rootList = [add,show,delete,moveFrom,undo]

-- Present root menu after each action in the action list
rootMenuAfterEachAction = forEach (`andThen`presentRootMenu) 

-- Printing strings
[printWelcomingMessage,
 waveAndExit          ,
 showConfusion        ] =
 forEach printString
  [tabBefore "Welcome to the exercises manager",
   "bye!"                                      ,
   "Ehhh what?"                                ]

-- Undo
undo =
 getCurrentDataKeeper`unwrapAnd`
 removeFile `andThen`
 downdateVersion`andThen`
 presentRootMenu
