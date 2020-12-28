module ToSubject where
import Prelude
  ( String )
import Renaming
  ( (>>>) )
import Types
  ( Subject, ExerciseData( subject ), Exercise( ToDo, Done, Missed ), ToDoExercise( toDoData )
  , DoneExercise( doneData ), MissedExercise( missedData ) )

class ToSubject a where toSubject :: a -> Subject

instance ToSubject Exercise where
  toSubject = \case 
    ToDo exercise -> toSubject exercise
    Done exercise -> toSubject exercise
    Missed exercise -> toSubject exercise

instance ToSubject ToDoExercise where
  toSubject = toDoData >>> toSubject

instance ToSubject DoneExercise where
  toSubject = doneData >>> toSubject

instance ToSubject MissedExercise where
  toSubject = missedData >>> toSubject

instance ToSubject ExerciseData where
  toSubject = subject
