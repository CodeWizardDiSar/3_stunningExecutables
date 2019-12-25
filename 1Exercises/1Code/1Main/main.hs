{-# LANGUAGE LambdaCase #-} 
module Main where
import Prelude hiding (print,show,showList)
import Data.Function
import Renaming
import Menus
import Useful
import Show
import Add
import Change
import Move

main             = doSequentially [welcome,presentRootMenu]
welcome          = doSequentially [printEmptyLine,printWelcomingMessage]
printWelcomingMessage = printString welcomingMessage
welcomingMessage = tabBefore "Welcome to the exercises manager"
presentRootMenu  = printMenuAndDoChosen (rootMenu,rootActionList)
printMenuAndDoChosen = \(menu,actionList)->
  print menu `getChoiceAnd`doChosenFrom actionList
print            = printStrsWithNLs
getChoiceAnd     = \a b->(a`andThen`getLine`unwrapAnd`b)
doChosenFrom     = \case
  [a,s,d,f]-> \case
    "a"->a;"s"->s;"d"->d;"f"->f;"" ->waveAndExit
    _  ->showConfusion`andThen`presentRootMenu
  [a,s,d]  -> \case
    "a"->a;"s"->s;"d"->d;"" ->waveAndExit
    _  ->showConfusion`andThen`presentRootMenu
  _        -> printErrorMessage "Not 3 or 4 options"
[waveAndExit,showConfusion] = forEach printString ["bye!","Ehhh what?"]
rootActionList   = [add,show,change,moveFrom]
[add,show,change,moveFrom,moveTo]= 
  forEach printMenuAndDoChosen menusAndActionLists
menusAndActionLists =
  [(addMenu     ,addActionList     )
  ,(showMenu    ,showActionList    )
  ,(changeMenu  ,changeActionList  )
  ,(moveFromMenu,moveFromActionList)
  ,(moveToMenu  ,moveToActionList  )]

[addActionList,showActionList,changeActionList,moveFromActionList,
 moveToActionList] =
  forEachList rootMenuAfterEachAction
    [addToList,showList,changeList,moveFromList,moveToList]
forEachList = forEach
rootMenuAfterEachAction = (`andThen`presentRootMenu)&forEach 
