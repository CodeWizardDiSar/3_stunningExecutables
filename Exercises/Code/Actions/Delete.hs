module Delete where
import Prelude   
  ( (.), not, filter, (-), (!!), (==), IO, String, Int, (>>=), (>>) )
import Types
  ( Exercise, Exercises, Subject, Subjects, ExerciseType( ToDoEx, DoneEx, MissedEx ) )
import Helpers
  ( combine  )
import Renaming
  ( wrap )
import ExercisesFromFile
  ( toDoExercises, doneExercises, missedExercises )
import Data.Function
  ( (&) )
import FileManagement
  ( updateVersion )
import UsefulForActions
  ( writeExercisesToFile )
import ToSubject
  ( toSubjects )
import ShowExercises
  ( getChosen, subIs )
import Control.Monad
  ( (>=>) )

deleteActions :: [ IO () ]
deleteActions = [ delete ToDoEx, delete DoneEx, delete MissedEx ]

delete :: ExerciseType -> IO ()
delete exerciseType = exercisesAfterDeletionToFile exerciseType >> updateVersion

exercisesAfterDeletionToFile :: ExerciseType -> IO ()
exercisesAfterDeletionToFile = exercisesAfterDeletion >=> writeExercisesToFile

exercisesAfterDeletion :: ExerciseType -> IO Exercises
exercisesAfterDeletion = \case
  ToDoEx -> combine [ toDoExercises >>= deleteChosen, doneExercises, missedExercises ]
  DoneEx -> combine [ toDoExercises, doneExercises >>= deleteChosen, missedExercises ]
  MissedEx -> combine [ toDoExercises, doneExercises, missedExercises >>= deleteChosen ]

deleteChosen :: Exercises -> IO Exercises
deleteChosen = getChosen >=> delete'

delete' :: ( Exercises, Int, Int ) -> IO Exercises
delete' = \( exercises, subjectNumber, exerciseNumber ) ->
 let sub = exercises & toSubjects & chosenSubject subjectNumber
     ex = subjectExercises sub exercises !! ( exerciseNumber - 1 )
 in removeExercise ex exercises & wrap

chosenSubject :: Int -> Subjects -> Subject
chosenSubject subjectNumber subjects = subjects !! ( subjectNumber - 1 )

subjectExercises :: Subject -> Exercises -> Exercises
subjectExercises subject = filter ( subIs subject )

removeExercise :: Exercise -> Exercises -> Exercises
removeExercise exercise = filter ( not . ( == exercise ) )
