{-# LANGUAGE LambdaCase #-} 
module Main where
import Renaming      (forEach,printString,andThen,unwrapAnd)
import Useful        (doSequentially)
import Useful        (printEmptyLine,tabBefore,printStrsWithNLs)
import Prelude       (getLine)
import Data.Function ((&))
import Menus         (rootMenu,addMenu,showMenu,changeMenu)
import Menus         (moveFromMenu,moveToMenu)
import Show          (showList)
import Add           (addList)
import Change        (changeList)
import Move          (moveFromList,moveToList)
-- Main baby
[main,welcome] =
 forEach doSequentially
  [[welcome,presentRootMenu],[printEmptyLine,printWelcomingMessage]] 
-- Printing strings
[printWelcomingMessage,waveAndExit,showConfusion] =
 forEach printString
  [tabBefore "Welcome to the exercises manager","bye!","Ehhh what?"]
-- Exciting actions
[presentRootMenu,add,show,change,moveFrom,moveTo] =
 forEach printMenuAndDoChosen 
  [(rootMenu,[add,show,change,moveFrom]),(addMenu,addThenMenuList)      ,
   (showMenu,showThenMenuList)          ,(changeMenu,changeThenMenuList),
   (moveFromMenu,moveFromThenMenuList)  ,(moveToMenu,moveToThenMenuList)]
printMenuAndDoChosen = \(menuStrings,actionList)->
 printStrsWithNLs menuStrings`andThen`
 getLine`unwrapAnd`doChosenFrom actionList
doChosenFrom = \case
 [a,s,d,f]-> \case
  "f"->f
  otherChoise -> doChosenFrom [a,s,d] otherChoise
 [a,s,d]       -> \case
  "a"->a;"s"->s;"d"->d;"" ->waveAndExit
  _  ->showConfusion`andThen`presentRootMenu
-- Action Lists
[addThenMenuList,showThenMenuList,changeThenMenuList,
 moveFromThenMenuList,moveToThenMenuList] =
 forEach rootMenuAfterEachAction
  [addList,showList,changeList,moveFromList,moveToList]
rootMenuAfterEachAction = (`andThen`presentRootMenu)&forEach 
