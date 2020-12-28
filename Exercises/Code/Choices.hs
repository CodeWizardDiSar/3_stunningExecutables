module Choices where
import Prelude
  ( zipWith, String, Int, (+), (++) )
import Renaming
  ( forEach, glue )
import Types
  ( ChoicesWithTitle, Choices, Title, Strings )
import UsefulFunctions
  ( tabBefore, tabsBefore )
import Data.Function
  ( (&) )
import ToString
  ( toString )

-- WT = With Title
allChoicesWT :: [ ChoicesWithTitle ]
allChoicesWT =
  [ initialChoicesWT, addChoicesWT, showChoicesWT, editChoicesWT, deleteChoicesWT
  , moveChoicesWT ]
[ initialChoicesWT, addChoicesWT, showChoicesWT, editChoicesWT, deleteChoicesWT
 ,moveChoicesWT] = zipWith mergeTitleAndChoices titles allChoices

mergeTitleAndChoices :: Title -> Choices -> ChoicesWithTitle
mergeTitleAndChoices title choices =
  tabBefore title : forEach ( 2 & tabsBefore ) choices & forEach ( ++ "\n" ) & glue

titles :: [ Title ]
titles = [ "Command me master", "Add to", "Show", "Edit", "Delete From", "Move From" ]

allChoices :: [ Choices ]
allChoices =
  [ initialChoices, exceptAllChoices, showChoices, exceptAllChoices, exceptAllChoices
  , exceptAllChoices ]

initialChoices :: Choices
initialChoices =
  putNumbers [ "Add", "Show", "Edit", "Delete", "Move", "Undo" ] ++ [ exitOption ]

exceptAllChoices :: Choices
exceptAllChoices = putNumbers exerciseTypes ++ [ exitOption ]

showChoices :: Choices
showChoices = putNumbers ( exerciseTypes ++ [ "All" ] ) ++ [ exitOption ]

putNumbers :: Strings -> Choices
putNumbers = zipWith ( \int string -> toString int ++ ": " ++ string ) ( [ 1.. ] :: [ Int ] )

exerciseTypes :: Strings
exerciseTypes = [ "To Do", "Done", "Missed" ]

type Choice = String
exitOption :: Choice
exitOption = "enter: Exit"
