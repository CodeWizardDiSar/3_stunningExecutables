module Edit where
import Prelude
  ( (.), not, filter, (-), (!!), (==), getLine, (++), (>>=), IO, String, Int, (>>) )
import Types
  ( Exercise( ToDo, Done, Missed ), HopefullySome( IndeedItIs ), Exercises, Date
  , ExData ( subject, number, name ), ToDoExercise( ToDoExercise )
  , DoneExercise, MissedExercise
  , ExerciseType ( ToDoEx, DoneEx, MissedEx ) 
  , ExercisesAndChosen ( ExercisesAndChosen, chosen ) )
import Helpers
  ( combine, removeChosen )
import GetFromUser
  ( getFromUser )
import UsefulForActions
  ( printAndGetAnswer, writeExercisesToFile )
import ToSubject
  ( toSubjects )
import Renaming 
  ( wrap, (.>) )
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

editChosen :: ExercisesAndChosen -> IO Exercises
editChosen exercisesAndChosen =
  modify ( chosen exercisesAndChosen ) >>= ( : removeChosen exercisesAndChosen ) .> wrap

newToDoExercise :: Date -> ExData -> IO Exercise
newToDoExercise date newExData = ToDo ( ToDoExercise newExData date ) & wrap

modify :: Exercise -> IO Exercise
modify = \case
  ToDo toDoExercise -> modifyToDo toDoExercise
  Done doneExercise -> modifyDone doneExercise
  Missed missedExercise -> modifyMissed missedExercise

modifyToDo :: ToDoExercise -> IO Exercise
modifyToDo ( ToDoExercise exerciseData date ) =
  chooseAttributeWithDate >>= \case
    "1" -> changeSubject exerciseData >>= newToDoExercise date
    "2" -> changeNumber exerciseData >>= newToDoExercise date
    "3" -> changeName exerciseData >>= newToDoExercise date
    "4" -> getFromUser >>= \newDate -> ToDo ( ToDoExercise exerciseData newDate ) & wrap

modifyDone :: DoneExercise -> IO Exercise
modifyDone exerciseData =
  chooseAttribute >>= \case
    "1" -> changeSubject exerciseData >>= Done .> wrap
    "2" -> changeNumber exerciseData >>= Done .> wrap
    "3" -> changeName exerciseData >>= Done .> wrap

modifyMissed :: MissedExercise -> IO Exercise
modifyMissed exerciseData =
  chooseAttribute >>= \case
    "1" -> changeSubject exerciseData >>= Missed .> wrap
    "2" -> changeNumber exerciseData >>= Missed .> wrap
    "3" -> changeName exerciseData >>= Missed .> wrap

changeSubject :: ExData -> IO ExData
changeSubject exerciseData =
  getSubject >>= \newSubject -> exerciseData { subject = newSubject } & wrap

changeNumber :: ExData -> IO ExData
changeNumber exerciseData =
  getENum >>= \newNumber -> exerciseData { number = newNumber } & wrap

changeName :: ExData -> IO ExData
changeName exerciseData =
  getEName >>= \newName -> exerciseData { name = IndeedItIs newName } & wrap

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
