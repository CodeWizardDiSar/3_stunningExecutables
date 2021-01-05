module Edit where

import Prelude
  ( getLine, (++), (>>=), IO, String, (>>) )
import Types
  ( Exercise( ToDo, Done, Missed ), HopefullySome( IndeedItIs ), Exercises, Date, Strings
  , ExData ( subject, number, name ), ToDoExercise( ToDoExercise ), Choice
  , ExerciseType ( ToDoEx, DoneEx, MissedEx ), Choices
  , ExercisesAndChosen ( ExercisesAndChosen, chosen ) )
import Helpers
  ( removeChosen )
import Helpers2
  ( exsAfter )
import GetFromUser
  ( getFromUser )
import UsefulForActions
  ( printAndGetAnswer, exsToFileAndUpdate )
import Renaming 
  ( wrap, (.>), forEach )
import Data.Function
  ( (&) )
import UsefulFunctions
  ( printStrings )
import ShowExercises
  ( getChosen )
import Control.Monad
  ( (>=>) )
import Choices
  ( putNumbers )

editActions :: [ IO () ]
editActions = [ edit ToDoEx, edit DoneEx, edit MissedEx ]

edit :: ExerciseType -> IO ()
edit = exsAfter ( getChosen >=> editChosen ) >=> exsToFileAndUpdate

editChosen :: ExercisesAndChosen -> IO Exercises
editChosen exsAndChosen =
  editEx ( chosen exsAndChosen ) >>= ( : removeChosen exsAndChosen ) .> wrap

editEx :: Exercise -> IO Exercise
editEx = \case
  ToDo toDoExercise -> editToDoEx toDoExercise
  Done exData -> editDoneOrMissedEx Done exData
  Missed exData -> editDoneOrMissedEx Missed exData

editToDoEx :: ToDoExercise -> IO Exercise
editToDoEx ( ToDoExercise exerciseData date ) =
  getChoiceWithDate >>= \case
    "1" -> changeSubject exerciseData >>= newToDoExercise date
    "2" -> changeNumber exerciseData >>= newToDoExercise date
    "3" -> changeName exerciseData >>= newToDoExercise date
    "4" -> getFromUser >>= \newDate -> ToDo ( ToDoExercise exerciseData newDate ) & wrap

newToDoExercise :: Date -> ExData -> IO Exercise
newToDoExercise date newExData = ToDo ( ToDoExercise newExData date ) & wrap

editDoneOrMissedEx :: ( ExData -> Exercise ) -> ExData -> IO Exercise
editDoneOrMissedEx constructor exerciseData =
  getChoice >>= \case
    "1" -> changeSubject exerciseData >>= constructor .> wrap
    "2" -> changeNumber exerciseData >>= constructor .> wrap
    "3" -> changeName exerciseData >>= constructor .> wrap

changeSubject :: ExData -> IO ExData
changeSubject exerciseData =
  getSubject >>= \newSubject -> exerciseData { subject = newSubject } & wrap

changeNumber :: ExData -> IO ExData
changeNumber exerciseData =
  getENum >>= \newNumber -> exerciseData { number = newNumber } & wrap

changeName :: ExData -> IO ExData
changeName exerciseData =
  getEName >>= \newName -> exerciseData { name = IndeedItIs newName } & wrap

[ getChoice, getChoiceWithDate ] =
  forEach ( >> getLine ) [ printExDataChoices, printExDataAndDateChoices ] :: [ IO Choice ]

exDataChoices :: Choices
exDataChoices = [ "Subject", "Number", "Name" ]

putNumbersTabsAndPrint :: Choices -> IO ()
putNumbersTabsAndPrint = putNumbers .> forEach ('\t':) .> printStrings

[ printExDataChoices, printExDataAndDateChoices ] = 
  forEach putNumbersTabsAndPrint [ exDataChoices, exDataChoices ++ [ "Date" ] ] :: [ IO () ]

[ getSubject, getENum, getEName ] =
  forEach printAndGetAnswer [ "New Subject?", "New Number?", "New Name?" ] :: [ IO String ]
