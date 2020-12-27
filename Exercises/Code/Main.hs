module Main where
import Prelude
  ( String, IO, getLine, Maybe( Just, Nothing ), Int, (-), (!!), (>>), (>>=) )
import Renaming
  ( forEach, printString )
import UsefulFunctions
  ( doSequentially, printEmptyLine )
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

main :: IO ()
main = doSequentially [ printEmptyLine, printWelcomingMessage, initialMenu ] 

-- WIMAE = With Initial Menu After Each
menus :: [ IO () ]
menus = [ initialMenu, addMenu, showMenu, editMenu, deleteMenu, moveMenu ] 
[ initialMenu, addMenu, showMenu, editMenu, deleteMenu, moveMenu ] =
  printChoicesAndDoChosenAction `forEach`
    [ ( initialChoicesWT, initialActions ), ( addChoicesWT, addActionsWIMAE )
    , ( showChoicesWT, showActionsWIMAE ), ( editChoicesWT, editActionsWIMAE )
    , ( deleteChoicesWT, deleteActionsWIMAE ), ( moveChoicesWT, moveActionsWIMAE )
    ]

initialActions :: [ IO () ]
initialActions = [ addMenu, showMenu, editMenu, deleteMenu, moveMenu, undo ]

printChoicesAndDoChosenAction :: ( String, [ IO () ] ) ->  IO ()
printChoicesAndDoChosenAction ( choices, actions ) =
  printAndGetAnswer choices >>= doChosenFrom actions

doChosenFrom :: [ IO () ] -> String -> IO ()
doChosenFrom actions input =
  case readMaybe input :: Maybe Int of
    Just i -> actions !! ( i - 1 )
    Nothing ->
      case input of
        "" -> waveAndExit
        _ -> showConfusion >> initialMenu

actionsWIMAE :: [ [ IO () ] ]
actionsWIMAE = 
  [ addActionsWIMAE, showActionsWIMAE, editActionsWIMAE, deleteActionsWIMAE, moveActionsWIMAE ]
[ addActionsWIMAE, showActionsWIMAE, editActionsWIMAE, deleteActionsWIMAE, moveActionsWIMAE ] =
  ( ( >> initialMenu ) `forEach` ) `forEach`
    [ addActions, showActions, editActions, deleteActions, moveActions ]

undo :: IO ()
undo = getCurrentDataKeeper >>= removeFile >> downdateVersion >> initialMenu

printingStuff :: [ IO () ]
printingStuff = [ printWelcomingMessage, waveAndExit, showConfusion ]
[ printWelcomingMessage, waveAndExit, showConfusion ] =
  printString `forEach` [ "\tWelcome to the exercises manager", "bye!", "Ehhh what?" ]
