module Choices where
import Renaming (forEach, glue, and, convertIntToString)
import UsefulFunctions (tabBefore, tabsBefore)
import Types (Strings)
import Prelude (zipWith, take, ($), String, (+), (++))
import Data.Function ((&))

[initialChoices, addChoices, showChoices, editChoices, deleteChoices, moveChoices] = 
  zipWith mergeTitleAndChoices titles optionsList

type Title = String
type Choices = [String]
type TitleChoicesMerged = String
mergeTitleAndChoices :: Title -> Choices -> TitleChoicesMerged
mergeTitleAndChoices = \t os-> 
  [tabBefore t] ++ (forEach (2 & tabsBefore) os) & forEach (++ "\n") & glue

titles = ["Command me master", "Add to", "Show", "Edit", "Delete From", "Move From"]

optionsList =
  [initialOptions, exceptAllOptions, showOptions, exceptAllOptions, exceptAllOptions
  ,exceptAllOptions]

-- Options
putNumbers = putNumber 1
putNumber = \i -> \case 
  [] -> 
    []
  s:ss ->
    glue [convertIntToString i,": ", s]:putNumber (i+1) ss

exerciseTypes = ["To Do","Done","Missed"]

exceptAllOptions = putNumbers exerciseTypes++[exitOption]

initialOptions = putNumbers ["Add","Show","Edit","Delete", "Move","Undo"]++[exitOption]

showOptions = putNumbers (exerciseTypes++["All"])++[exitOption]

exitOption = "enter: Exit"
