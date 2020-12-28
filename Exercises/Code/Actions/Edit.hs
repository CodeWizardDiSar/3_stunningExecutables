module Edit where
import Prelude
  ( (.), not, filter, (-), (!!), (==), getLine, (++), (>>=), IO, String, Int, (>>) )
import Types
  ( Exercise( ToDo, Done, Missed ), HopefullySome( IndeedItIs ), Exercises, Date
  , ExerciseData ( subject, number, name ), ToDoExercise( ToDoExercise )
  , DoneExercise( DoneExercise ), MissedExercise( MissedExercise )
  , ExerciseType ( ToDoEx, DoneEx, MissedEx ) )
import Add
  ( dateFromUser )
import UsefulForActions
  ( combine, printAndGetAnswer, writeExercisesToFile, exercisesToSubjects )
import Renaming 
  ( printString, wrap, (>>>) )
import ExercisesFromFile
  ( toDoExercises, doneExercises, missedExercises )
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
  ( putNumbers )

editActions :: [ IO () ]
editActions = [ edit ToDoEx, edit DoneEx, edit MissedEx ]

edit :: ExerciseType -> IO ()
edit exerciseType = editExercises exerciseType >>= writeExercisesToFile >> updateVersion

editExercises :: ExerciseType -> IO Exercises
editExercises = \case
  ToDoEx -> combine [ toDoExercises >>= getAndEditChosen, doneExercises, missedExercises ]
  DoneEx -> combine [ toDoExercises, doneExercises >>= getAndEditChosen, missedExercises ]
  MissedEx -> combine [ toDoExercises, doneExercises, missedExercises >>= getAndEditChosen ]

getAndEditChosen :: Exercises -> IO Exercises
getAndEditChosen = getChosen >=> editChosen

editChosen :: ( Exercises, Int, Int ) -> IO Exercises
editChosen = \( exs, subNum, exNum ) ->
 let sub = exercisesToSubjects exs !! ( subNum - 1 )
     ex = filter (subIs sub) exs !! ( exNum - 1 )
 in modify ex >>= ( ( : filter ( not . (==ex) ) exs ) >>> wrap )

changeSubjectName :: ExerciseData -> IO ExerciseData
changeSubjectName exerciseData =
  getSubject >>= \newSubject -> exerciseData { subject = newSubject } & wrap

changeExerciseNumber :: ExerciseData -> IO ExerciseData
changeExerciseNumber exerciseData =
  getENum >>= \newNumber -> exerciseData { number = newNumber } & wrap

changeExerciseName :: ExerciseData -> IO ExerciseData
changeExerciseName exerciseData =
  getEName >>= \newName -> exerciseData { name = IndeedItIs newName } & wrap

getToDoExerciseWithNewData :: Date -> ExerciseData -> IO Exercise
getToDoExerciseWithNewData date newExerciseData =
  ToDo ( ToDoExercise newExerciseData date ) & wrap

getExerciseWithNewData :: ( ExerciseData -> Exercise ) -> ExerciseData -> IO Exercise
getExerciseWithNewData exerciseConstructor newExerciseData = 
  exerciseConstructor newExerciseData & wrap

modify :: Exercise -> IO Exercise
modify = \case
  ToDo ( ToDoExercise exerciseData date ) ->
    chooseAttributeWithDate >>= \case
      "1"-> changeSubjectName exerciseData >>= getToDoExerciseWithNewData date
      "2"-> changeExerciseNumber exerciseData >>= getToDoExerciseWithNewData date
      "3"-> changeExerciseName exerciseData >>= getToDoExerciseWithNewData date
      "4"-> dateFromUser >>= \newDate -> ToDo ( ToDoExercise exerciseData newDate ) & wrap
  Done ( DoneExercise exerciseData ) ->
    chooseAttribute >>= \case
      "1"-> changeSubjectName exerciseData >>= getExerciseWithNewData ( DoneExercise >>> Done )
      "2"-> changeExerciseNumber exerciseData >>=
              getExerciseWithNewData ( DoneExercise >>> Done )
      "3"-> changeExerciseName exerciseData >>=
              getExerciseWithNewData ( DoneExercise >>> Done )
  Missed ( MissedExercise exerciseData ) ->
    chooseAttribute >>= \case
      "1"-> changeSubjectName exerciseData >>=
              getExerciseWithNewData ( MissedExercise >>> Missed )
      "2"-> changeExerciseNumber exerciseData >>=
              getExerciseWithNewData ( MissedExercise >>> Missed )
      "3"-> changeExerciseName exerciseData >>=
              getExerciseWithNewData ( MissedExercise >>> Missed )

chooseAttribute :: IO String
chooseAttribute = printBasic >> getLine

chooseAttributeWithDate :: IO String
chooseAttributeWithDate = printBasicAndDate >> getLine

exData :: [ String ] 
exData = [ "Subject", "Number", "Name" ]

printBasic :: IO ()
printBasic = exData & putNumbers & printStrings

printBasicAndDate :: IO ()
printBasicAndDate = exData ++ [ "Date" ] & putNumbers & printStrings

getSubject :: IO String
getSubject = printAndGetAnswer "New Subject?"

getENum :: IO String
getENum = printAndGetAnswer "New Exercise Number?"

getEName :: IO String
getEName = printAndGetAnswer "New Exercise Name?"
