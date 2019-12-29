{-# LANGUAGE LambdaCase #-} 
module Main where
import Prelude hiding (print,show,showList)
import Data.Function ((&))
import Renaming (forEach,printString,andThen,unwrapAnd)
import Menus (rootMenu,addMenu,showMenu,changeMenu,moveFromMenu,moveToMenu)
import Useful (doSequentially,printEmptyLine,tabBefore,printStrsWithNLs)
import Show (showList)
import Add (addToList)
import Change (changeList)
import Move (moveFromList,moveToList)

-- Main baby
[main,welcome] = forEach doSequentially
  [[welcome,presentRootMenu],[printEmptyLine,printWelcomingMessage]] 
-- Printing strings
[printWelcomingMessage,waveAndExit,showConfusion] = forEach printString
  [tabBefore "Welcome to the exercises manager","bye!","Ehhh what?"]
-- Exciting actions
[presentRootMenu,add,
 show           ,change,
 moveFrom       ,moveTo] = forEach printMenuAndDoChosen 
    [(rootMenu,[add,show,change,moveFrom]),(addMenu,addActionList)
    ,(showMenu,showActionList)            ,(changeMenu,changeActionList)
    ,(moveFromMenu,moveFromActionList)    ,(moveToMenu,moveToActionList)]
printMenuAndDoChosen = \(menuStrings,actionList)->
  printStrsWithNLs menuStrings`andThen`getLine`unwrapAnd`doChosenFrom actionList
doChosenFrom     = \case
  [a,s,d,f]-> \case
    "f"->f
    otherChoise -> doChosenFrom [a,s,d] otherChoise
  [a,s,d]       -> \case
    "a"->a;"s"->s;"d"->d;"" ->waveAndExit
    _  ->showConfusion`andThen`presentRootMenu

[addActionList,showActionList,changeActionList,moveFromActionList,
 moveToActionList] = forEachList rootMenuAfterEachAction
  [addToList,showList,changeList,moveFromList
  ,moveToList]
forEachList = forEach
rootMenuAfterEachAction = (`andThen`presentRootMenu)&forEach 
