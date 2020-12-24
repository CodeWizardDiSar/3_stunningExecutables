module Main where
import Prelude (String, IO, getLine, Maybe(..), Int(..), (-), (!!), (>>), (>>=))
import Renaming (forEach, printString)
import UsefulFunctions (doSequentially, printEmptyLine)
import FileManagement (getCurrentDataKeeper, downdateVersion)
import Choices (initialChoices, addChoices, showChoices, editChoices, deleteChoices
               ,moveChoices)
import Show (showActions)
import Add (addActions)
import Edit (editActions)
import Delete (deleteActions)
import Move (moveActions)
import System.Directory (removeFile)
import Text.Read (readMaybe)

main :: IO ()
main = doSequentially [printEmptyLine, printWelcomingMessage, initialMenu] 

menus :: [IO ()]
menus = [initialMenu, addMenu, showMenu, editMenu, deleteMenu, moveMenu] 
[initialMenu, addMenu, showMenu, editMenu, deleteMenu, moveMenu] =
  forEach printChoicesAndDoChosenAction 
    [(initialChoices, initialActions), (addChoices, addActionsTIM)
    ,(showChoices, showActionsTIM), (editChoices, editActionsTIM)
    ,(deleteChoices, deleteActionsTIM), (moveChoices, moveActionsTIM)
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

-- TIM = Then Initial Menu
actionsTIM :: [[IO ()]]
actionsTIM = 
  [addActionsTIM, showActionsTIM, editActionsTIM, deleteActionsTIM, moveActionsTIM] 
[addActionsTIM, showActionsTIM, editActionsTIM, deleteActionsTIM, moveActionsTIM] =
  forEach (forEach (>>initialMenu))
    [addActions, showActions, editActions, deleteActions, moveActions]

undo :: IO ()
undo =
  getCurrentDataKeeper >>= removeFile >>
  downdateVersion >>
  initialMenu

printingStuff :: [IO ()]
printingStuff = [printWelcomingMessage, waveAndExit, showConfusion]
[printWelcomingMessage,waveAndExit,showConfusion] =
  forEach printString ["\tWelcome to the exercises manager", "bye!", "Ehhh what?"]
