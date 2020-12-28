module Delete where
import Prelude   
  ( (.), not, filter, (-), (!!), (==), IO, String, Int, (>>=), (>>) )
import Renaming
  ( wrap )
import ExercisesFromFile
  ( getToDoExercises, getDoneExercises, getMissedExercises )
import Data.Function
  ( (&) )
import FileManagement
  ( updateVersion )
import Types
  ( Strings, Exercise, Exercises, Subject, Subjects )
import UsefulForActions
  ( combine, writeExercisesToFile, exercisesToSubjects )
import ShowExercises
  ( getChosen, subIs )
import Control.Monad
  ( (>=>) )

deleteActions :: [ IO () ]
deleteActions = [ deleteFrom "todo", deleteFrom "done", deleteFrom "missed" ]

deleteFrom :: String -> IO ()
deleteFrom exerciseType =
  deleteAndGetNewExercises exerciseType >>= writeExercisesToFile >> updateVersion

deleteAndGetNewExercises :: String -> IO Exercises
deleteAndGetNewExercises = \case
 "todo" ->
   combine [ getToDoExercises >>= deleteChosen, getDoneExercises, getMissedExercises ]
 "done" ->
   combine [ getToDoExercises, getDoneExercises >>= deleteChosen, getMissedExercises ]
 "missed" ->
   combine [ getToDoExercises, getDoneExercises, getMissedExercises >>= deleteChosen ]

deleteChosen :: Exercises -> IO Exercises
deleteChosen = getChosen >=> delete

delete :: ( Exercises, Int, Int ) -> IO Exercises
delete = \( exercises, subjectNumber, exerciseNumber ) ->
 let sub = exercises & exercisesToSubjects & chosenSubject subjectNumber
     ex = chosenSubjectExercises exercises sub !! ( exerciseNumber - 1 )
 in removeExercise ex exercises & wrap

chosenSubject :: Int -> Subjects -> Subject
chosenSubject subjectNumber subjects = subjects !! ( subjectNumber - 1 )

chosenSubjectExercises :: Exercises -> Subject -> Exercises
chosenSubjectExercises exercises subject = filter ( subIs subject ) exercises

removeExercise :: Exercise -> Exercises -> Exercises
removeExercise exercise = filter ( not . ( == exercise ) )
