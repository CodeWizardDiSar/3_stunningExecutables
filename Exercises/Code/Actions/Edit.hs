module Edit where
import Prelude
  ( (.), not, filter, (-), (!!), (==), getLine, (++), (>>=), IO, String, Int, (>>) )
import Types
  ( Exercise( ToDo, Done, Missed ), HopefullySome( IndeedItIs ), Exercises, Date
  , ExerciseData ( subjectName, exerciseNumber, exerciseName ) )
import Add
  ( getDate )
import UsefulForActions
  ( combine, printAndGetAnswer, writeExercisesToFile, exercisesToSubjects )
import Renaming 
  ( printString, wrap, andThen, (>>>) )
import ExercisesFromFile
  ( getToDoExercises, getDoneExercises, getMissedExercises )
import Data.Function
  ( (&) )
import FileManagement
  ( updateVersion )
import UsefulFunctions
  ( printStrings )
import ShowExercises
  ( getChosen, subIs )
import Control.Monad
  ( (>=>) )
import Choices
  ( numbered )

editActions :: [ IO () ]
editActions = [ edit "todo", edit "done", edit "missed" ]

edit :: String -> IO ()
edit exerciseType = editExercises exerciseType >>= writeExercisesToFile >> updateVersion

editExercises :: String -> IO Exercises
editExercises = \case
 "todo" ->
   combine [ getToDoExercises >>= getAndEditChosen, getDoneExercises, getMissedExercises ]
 "done" ->
   combine [ getToDoExercises, getDoneExercises >>= getAndEditChosen, getMissedExercises ]
 "missed" ->
   combine [ getToDoExercises, getDoneExercises, getMissedExercises >>= getAndEditChosen ]

getAndEditChosen :: Exercises -> IO Exercises
getAndEditChosen = getChosen >=> editChosen

editChosen :: ( Exercises, Int, Int ) -> IO Exercises
editChosen = \( exs, subNum, exNum ) ->
 let sub = exercisesToSubjects exs !! ( subNum - 1 )
     ex = filter (subIs sub) exs !! ( exNum - 1 )
 in modify ex >>= ( ( : filter ( not . (==ex) ) exs ) >>> wrap )

changeSubjectName :: ExerciseData -> IO ExerciseData
changeSubjectName exerciseData =
  getSubject >>= \newSubjectName -> exerciseData { subjectName = newSubjectName } & wrap

changeExerciseNumber :: ExerciseData -> IO ExerciseData
changeExerciseNumber exerciseData =
  getENum >>= \newExerciseNumber -> exerciseData { exerciseNumber = newExerciseNumber } & wrap

changeExerciseName :: ExerciseData -> IO ExerciseData
changeExerciseName exerciseData =
  getEName >>= \newExerciseName ->
    exerciseData { exerciseName = IndeedItIs newExerciseName } & wrap

getToDoExerciseWithNewData :: Date -> ExerciseData -> IO Exercise
getToDoExerciseWithNewData date newExerciseData = ToDo newExerciseData date & wrap

getExerciseWithNewData :: ( ExerciseData -> Exercise ) -> ExerciseData -> IO Exercise
getExerciseWithNewData exerciseConstructor newExerciseData = 
  exerciseConstructor newExerciseData & wrap

modify :: Exercise -> IO Exercise
modify = \case
  ToDo exerciseData date ->
    chooseAttributeWithDate >>= \case
      "1"-> changeSubjectName exerciseData >>= getToDoExerciseWithNewData date
      "2"-> changeExerciseNumber exerciseData >>= getToDoExerciseWithNewData date
      "3"-> changeExerciseName exerciseData >>= getToDoExerciseWithNewData date
      "4"-> getDate >>= \newDate -> ToDo exerciseData newDate & wrap
  Done exerciseData ->
    chooseAttribute >>= \case
      "1"-> changeSubjectName exerciseData >>= getExerciseWithNewData Done
      "2"-> changeExerciseNumber exerciseData >>= getExerciseWithNewData Done
      "3"-> changeExerciseName exerciseData >>= getExerciseWithNewData Done
  Missed exerciseData ->
    chooseAttribute>>= \case
      "1"-> changeSubjectName exerciseData >>= getExerciseWithNewData Missed
      "2"-> changeExerciseNumber exerciseData >>= getExerciseWithNewData Missed
      "3"-> changeExerciseName exerciseData >>= getExerciseWithNewData Missed

chooseAttribute :: IO String
chooseAttribute = printBasic >> getLine

chooseAttributeWithDate :: IO String
chooseAttributeWithDate = printBasicAndDate >> getLine

exData :: [ String ] 
exData = [ "Subject", "Number", "Name" ]

printBasic :: IO ()
printBasic = exData & numbered & printStrings

printBasicAndDate :: IO ()
printBasicAndDate = exData ++ [ "Date" ] & numbered & printStrings

getSubject :: IO String
getSubject = printAndGetAnswer "New Subject?"

getENum :: IO String
getENum = printAndGetAnswer "New Exercise Number?"

getEName :: IO String
getEName = printAndGetAnswer "New Exercise Name?"
