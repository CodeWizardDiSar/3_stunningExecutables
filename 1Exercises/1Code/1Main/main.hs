module Main where
import Renaming         (forEach,printString,andThen,unwrapAnd)
import UsefulFunctions  (doSequentially,printEmptyLine,tabBefore)
import Prelude          (getLine,Maybe(..),Int(..),(-),(!!))
import FileManagement   (getCurrentDataKeeper,downdateVersion)
import Menus            (rootMenu,addMenu,showMenu,editMenu,deleteMenu,moveMenu)
import Show             (showList)
import Add              (addList)
import Edit             (editList)
import Delete           (deleteList)
import Move             (moveList)
import System.Directory (removeFile)
import Text.Read        (readMaybe)

-- Main baby
main = doSequentially [printEmptyLine,printWelcomingMessage,root] 

-- Exciting actions
[root,add,show,edit,delete,move] =
 forEach printMenuDoChosen 
  [(rootMenu,rootActions),(addMenu   ,addActions   ),(showMenu,showActions),
   (editMenu,editActions),(deleteMenu,deleteActions),(moveMenu,moveActions)]
rootActions = [add,show,edit,delete,move,undo]
printMenuDoChosen = \(menu,actions)->
 printString menu`andThen`getLine`unwrapAnd`doChosenFrom actions
doChosenFrom = \actions chosen-> case readMaybe chosen :: Maybe Int of
 Just i  -> actions!!(i-1)
 Nothing -> case chosen of ""->waveAndExit;_->confusionAndRoot
[addActions,showActions,editActions,deleteActions,moveActions,[confusionAndRoot]] =
 forEach rootMenuAfterEachAction
  [addList,showList,editList,deleteList,moveList,[showConfusion]]
rootMenuAfterEachAction = forEach (`andThen`root) 
undo = getCurrentDataKeeper`unwrapAnd`removeFile`andThen`downdateVersion`andThen`root

-- Printing strings
[printWelcomingMessage,waveAndExit,showConfusion] =
 forEach printString [tabBefore "Welcome to the exercises manager","bye!","Ehhh what?"]
