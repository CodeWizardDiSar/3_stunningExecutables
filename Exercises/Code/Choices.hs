module Choices where
import Renaming
  ( forEach, glue, convertIntToString )
import Types
  ( ChoicesWithTitle, Choices, Title, Strings )
import UsefulFunctions
  ( tabBefore, tabsBefore )
import Prelude
  ( zipWith, String, (+), (++) )
import Data.Function
  ( (&) )

-- WT = With Title
allChoicesWT :: [ChoicesWithTitle]
allChoicesWT =
  [ initialChoicesWT, addChoicesWT, showChoicesWT, editChoicesWT, deleteChoicesWT
  , moveChoicesWT ]
[ initialChoicesWT, addChoicesWT, showChoicesWT, editChoicesWT, deleteChoicesWT
 ,moveChoicesWT] = zipWith mergeTitleAndChoices titles allChoices

mergeTitleAndChoices :: Title -> Choices -> ChoicesWithTitle
mergeTitleAndChoices = \title choices-> 
  [ tabBefore title ] ++ ( ( 2 & tabsBefore ) `forEach` choices ) & ( (++ "\n")`forEach` ) &
  glue

titles :: [ Title ]
titles = [ "Command me master", "Add to", "Show", "Edit", "Delete From", "Move From" ]

allChoices :: [ Choices ]
allChoices =
  [ initialChoices, exceptAllChoices, showChoices, exceptAllChoices, exceptAllChoices
  , exceptAllChoices ]

numbered :: Strings -> Choices
numbered = zipWith ( \int string -> convertIntToString int ++ ": " ++ string ) [ 1.. ]

exerciseTypes :: Strings
exerciseTypes = [ "To Do", "Done", "Missed" ]

exceptAllChoices :: Choices
exceptAllChoices = ( exerciseTypes & numbered ) ++ [ exitOption ]

initialChoices :: Choices
initialChoices =
  ( [ "Add", "Show", "Edit", "Delete", "Move", "Undo" ] & numbered) ++ [exitOption]

showChoices :: Choices
showChoices = ( ( exerciseTypes ++ [ "All" ] ) & numbered ) ++ [ exitOption ]

type Choice = String
exitOption :: Choice
exitOption = "enter: Exit"
