module Main where

import Prelude
  ( String, IO, Maybe( Just, Nothing ), Int, (-), (!!), (>>), (>>=), Bool ( True ) , (>), (<=)
  , (&&), length )
import Renaming
  ( forEach )
import UsefulFunctions
  ( doSequentially, printEmptyLine, showConfusion )
import FileManagement
  ( getCurrentDataKeeper, downdateVersion )
import Choices
  ( initialChoicesWT, addChoicesWT, showChoicesWT, editChoicesWT, deleteChoicesWT
  , moveChoicesWT )
import Show
  ( showActions )
import Add
  ( addActions )
import Edit
  ( editActions )
import Delete
  ( deleteActions )
import Move
  ( moveActions )
import System.Directory
  ( removeFile )
import Text.Read
  ( readMaybe )
import UsefulForActions
  ( printAndGetAnswer )
import ToString
  ( print )

main :: IO ()
main = doSequentially [ printEmptyLine, printWelcomingMessage, initialMenu ] 

-- WIMAE = With Initial Menu After Each
menus = [ initialMenu, addMenu, showMenu, editMenu, deleteMenu, moveMenu ] 
[ initialMenu, addMenu, showMenu, editMenu, deleteMenu, moveMenu ] =
  forEach printChoicesAndDoChosenAction
    [ ( initialChoicesWT, initialActions ), ( addChoicesWT, addActionsWIMAE )
    , ( showChoicesWT, showActionsWIMAE ), ( editChoicesWT, editActionsWIMAE )
    , ( deleteChoicesWT, deleteActionsWIMAE ), ( moveChoicesWT, moveActionsWIMAE )
    ] :: [ IO () ]

initialActions :: [ IO () ]
initialActions = [ addMenu, showMenu, editMenu, deleteMenu, moveMenu, undo ]

printChoicesAndDoChosenAction :: ( String, [ IO () ] ) ->  IO ()
printChoicesAndDoChosenAction ( choices, actions ) =
  printAndGetAnswer choices >>= doChosenFrom actions

doChosenFrom :: [ IO () ] -> String -> IO ()
doChosenFrom actions input =
  case readMaybe input :: Maybe Int of
    Just i ->
      case i > 0 && i <= length actions of
        True -> actions !! ( i - 1 )
        _ -> showConfusion >> initialMenu
    _ ->
      case input of
        "" -> waveAndExit
        _ -> showConfusion >> initialMenu

[ addActionsWIMAE, showActionsWIMAE, editActionsWIMAE, deleteActionsWIMAE, moveActionsWIMAE ] =
  forEach ( forEach ( >> initialMenu ) )
    [ addActions, showActions, editActions, deleteActions, moveActions ]
      :: [ [ IO () ] ]

undo :: IO ()
undo = getCurrentDataKeeper >>= removeFile >> downdateVersion >> initialMenu

[ printWelcomingMessage, waveAndExit ] =
  forEach print [ "Welcome to the exercises manager!\n", "bye!" ] :: [ IO () ]
