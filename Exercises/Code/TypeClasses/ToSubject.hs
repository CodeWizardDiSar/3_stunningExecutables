module ToSubject where
import Prelude
  ( String, foldl, elem, otherwise, (++) )
import Renaming
  ( (.>) )
import Types
  ( Subject, ExerciseData( subject ), Exercise( ToDo, Done, Missed ), ToDoExercise( toDoData )
  , DoneExercise( doneData ), MissedExercise( missedData ), Exercises, Subjects )

class ToSubject a where toSubject :: a -> Subject

instance ToSubject Exercise where
  toSubject = \case 
    ToDo exercise -> toSubject exercise
    Done exercise -> toSubject exercise
    Missed exercise -> toSubject exercise

instance ToSubject ToDoExercise where
  toSubject = toDoData .> toSubject

instance ToSubject DoneExercise where
  toSubject = doneData .> toSubject

instance ToSubject MissedExercise where
  toSubject = missedData .> toSubject

instance ToSubject ExerciseData where
  toSubject = subject

class ToSubjects a where toSubjects :: a -> Subjects

instance ToSubjects Exercises where
  toSubjects = foldl ( \subjects exercise -> addToSubjects ( toSubject exercise ) subjects ) []

addToSubjects :: Subject -> Subjects -> Subjects
addToSubjects subject subjects
  | elem subject subjects = subjects
  | otherwise = subjects ++ [ subject ]
