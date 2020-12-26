module Main where
import Prelude
  (String, IO, getLine, Maybe(..), Int(..), (-), (!!), (>>), (>>=))
import Renaming
  (forEach, printString)
import UsefulFunctions
  (doSequentially, printEmptyLine)
import FileManagement
  (getCurrentDataKeeper, downdateVersion)
import Choices
  (initialChoicesWT, addChoicesWT, showChoicesWT, editChoicesWT, deleteChoicesWT
  ,moveChoicesWT)
import Show
  (showActions)
import Add
  (addActions)
import Edit
  (editActions)
import Delete
  (deleteActions)
import Move
  (moveActions)
import System.Directory
  (removeFile)
import Text.Read
  (readMaybe)

main :: IO ()
main = doSequentially [printEmptyLine, printWelcomingMessage, initialMenu] 

-- TIM = Then Initial Menu
menus :: [IO ()]
menus = [initialMenu, addMenu, showMenu, editMenu, deleteMenu, moveMenu] 
[initialMenu, addMenu, showMenu, editMenu, deleteMenu, moveMenu] =
  printChoicesAndDoChosenAction `forEach`
    [(initialChoicesWT, initialActions), (addChoicesWT, addActionsTIM)
    ,(showChoicesWT, showActionsTIM), (editChoicesWT, editActionsTIM)
    ,(deleteChoicesWT, deleteActionsTIM), (moveChoicesWT, moveActionsTIM)
    ]

initialActions :: [IO ()]
initialActions = [addMenu, showMenu, editMenu, deleteMenu, moveMenu, undo]

printChoicesAndDoChosenAction :: (String, [IO ()]) ->  IO ()
printChoicesAndDoChosenAction (choices,actions) =
  printString choices >>
  getLine >>= doChosenFrom actions

doChosenFrom :: [IO ()] -> String -> IO ()
doChosenFrom actions input =
  case readMaybe input :: Maybe Int of
    Just i  ->
      actions!!(i-1)
    Nothing ->
      case input of
        ""->
          waveAndExit
        _->
          showConfusion >>
          initialMenu

actionsTIM :: [[IO ()]]
actionsTIM = 
  [addActionsTIM, showActionsTIM, editActionsTIM, deleteActionsTIM, moveActionsTIM] 
[addActionsTIM, showActionsTIM, editActionsTIM, deleteActionsTIM, moveActionsTIM] =
  ((>>initialMenu)`forEach`)`forEach`
    [addActions, showActions, editActions, deleteActions, moveActions]

undo :: IO ()
undo =
  getCurrentDataKeeper >>= removeFile >>
  downdateVersion >>
  initialMenu

printingStuff :: [IO ()]
printingStuff = [printWelcomingMessage, waveAndExit, showConfusion]
[printWelcomingMessage,waveAndExit,showConfusion] =
  printString`forEach`["\tWelcome to the exercises manager", "bye!", "Ehhh what?"]
