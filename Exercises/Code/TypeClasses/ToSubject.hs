module ToSubject where
import Prelude
  ( String )
import Renaming
  ( (>>>) )
import Types
  ( Subject, ExerciseData( subjectName ), Exercise( exerciseData ) )

class ToSubject a where toSubject :: a -> Subject

instance ToSubject ExerciseData where
  toSubject = subjectName

instance ToSubject Exercise where
  toSubject = exerciseData >>> toSubject
