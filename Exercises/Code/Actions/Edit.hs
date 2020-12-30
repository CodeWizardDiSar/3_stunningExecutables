module Edit where
import Prelude
  ( (.), not, filter, (-), (!!), (==), getLine, (++), (>>=), IO, String, Int, (>>) )
import Types
  ( Exercise( ToDo, Done, Missed ), HopefullySome( IndeedItIs ), Exercises, Date
  , ExerciseData ( subject, number, name ), ToDoExercise( ToDoExercise )
  , DoneExercise( DoneExercise ), MissedExercise( MissedExercise )
  , ExerciseType ( ToDoEx, DoneEx, MissedEx ) )
import GetFromUser
  ( getFromUser )
import UsefulForActions
  ( combine, printAndGetAnswer, writeExercisesToFile )
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

editChosen :: ( Exercises, Int, Int ) -> IO Exercises
editChosen = \( exs, subNum, exNum ) ->
 let sub = toSubjects exs !! ( subNum - 1 )
     ex = filter (subIs sub) exs !! ( exNum - 1 )
 in modify ex >>= ( ( : filter ( not . (==ex) ) exs ) .> wrap )

newToDoExercise :: Date -> ExerciseData -> IO Exercise
newToDoExercise date newExerciseData = ToDo ( ToDoExercise newExerciseData date ) & wrap

newExercise :: ( ExerciseData -> Exercise ) -> ExerciseData -> IO Exercise
newExercise exerciseConstructor newExerciseData = exerciseConstructor newExerciseData & wrap

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
modifyDone ( DoneExercise exerciseData ) =
  chooseAttribute >>= \case
    "1" -> changeSubject exerciseData >>= newExercise ( DoneExercise .> Done )
    "2" -> changeNumber exerciseData >>= newExercise ( DoneExercise .> Done )
    "3" -> changeName exerciseData >>= newExercise ( DoneExercise .> Done )

modifyMissed :: MissedExercise -> IO Exercise
modifyMissed ( MissedExercise exerciseData ) =
  chooseAttribute >>= \case
    "1" -> changeSubject exerciseData >>= newExercise ( MissedExercise .> Missed )
    "2" -> changeNumber exerciseData >>= newExercise ( MissedExercise .> Missed )
    "3" -> changeName exerciseData >>= newExercise ( MissedExercise .> Missed )

changeSubject :: ExerciseData -> IO ExerciseData
changeSubject exerciseData =
  getSubject >>= \newSubject -> exerciseData { subject = newSubject } & wrap

changeNumber :: ExerciseData -> IO ExerciseData
changeNumber exerciseData =
  getENum >>= \newNumber -> exerciseData { number = newNumber } & wrap

changeName :: ExerciseData -> IO ExerciseData
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
