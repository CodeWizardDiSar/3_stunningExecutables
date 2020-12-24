module Choices where
import Renaming
  (forEachIn, glue, convertIntToString)
import UsefulFunctions
  (tabBefore, tabsBefore)
import Types
  (Strings)
import Prelude
  (zipWith, take, ($), String, (+), (++))
import Data.Function
  ((&))

type ChoicesWithTitle = String
-- WT = With Title
allChoicesWT :: [ChoicesWithTitle]
allChoicesWT =
  [initialChoicesWT, addChoicesWT, showChoicesWT, editChoicesWT, deleteChoicesWT
  ,moveChoicesWT]
[initialChoicesWT, addChoicesWT, showChoicesWT, editChoicesWT, deleteChoicesWT,
 moveChoicesWT] = 
  zipWith mergeTitleAndChoices titles allChoices

type Title = String
type Choices = [String]
mergeTitleAndChoices :: Title -> Choices -> ChoicesWithTitle
mergeTitleAndChoices = \title choices-> 
  [tabBefore title] ++ ((2 & tabsBefore)`forEachIn`choices) & ( (++ "\n")`forEachIn` ) & glue

titles :: [Title]
titles = ["Command me master", "Add to", "Show", "Edit", "Delete From", "Move From"]

allChoices :: [Choices]
allChoices =
  [initialChoices, exceptAllChoices, showChoices, exceptAllChoices, exceptAllChoices
  ,exceptAllChoices]

numbered :: [String] -> Choices
numbered =
  zipWith (\int string -> convertIntToString int ++ ": " ++ string) [1..]

exerciseTypes :: [String]
exerciseTypes = ["To Do", "Done", "Missed"]

exceptAllChoices :: Choices
exceptAllChoices = (exerciseTypes & numbered) ++ [exitOption]

initialChoices :: Choices
initialChoices = (["Add", "Show", "Edit", "Delete", "Move", "Undo"] & numbered) ++ [exitOption]

showChoices :: Choices
showChoices = (( exerciseTypes ++ ["All"] ) & numbered) ++ [exitOption]

type Choice = String
exitOption :: Choice
exitOption = "enter: Exit"
