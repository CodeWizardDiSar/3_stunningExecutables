module TypeClasses where
import Prelude ( String )
import Renaming ( (>>>) )
import Types ( Strings, Subject, ExerciseData( subjectName ), Exercise( exerciseData ) )

-- Even more of a pleasure, type classes
class FromString a where fromString :: String -> a
class FromStrings a where fromStrings :: Strings -> a
class ToStringForFile a where toStringForFile :: a -> String
class ToStringForUser a where toStringForUser :: a -> String
class ToSubject a where toSubject :: a -> Subject

instance ToSubject ExerciseData where
  toSubject = subjectName

instance ToSubject Exercise where
  toSubject = exerciseData >>> toSubject
