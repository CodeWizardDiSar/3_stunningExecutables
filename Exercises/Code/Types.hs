module Types where
import Prelude ( Eq, String, Int, Bool )
import Renaming ( (>>>) )

-- Always a pleasure to have types (sorry python)
data HopefullySome a = IndeedItIs { getA :: a } | Nothing
  deriving ( Eq )

data Exercise = Done
  { exerciseData :: ExerciseData
  } | Missed
  { exerciseData :: ExerciseData 
  } | ToDo
  { exerciseData :: ExerciseData
  , getTDate :: Date
  }
  deriving ( Eq )

data ExerciseData = ED
  { subjectName :: Subject
  , exerciseNumber :: ExerciseNumber
  , exerciseName :: HopefullyExerciseName
  }
  deriving ( Eq )
   

data Date = D
  { day :: Day 
  , month :: Month
  , year :: Year 
  }
  deriving ( Eq )

type Day = Int
type Month = Int
type Year = Int

type Path = String
type Subject = String
type ExerciseNumber = String
type HeaderRow = String

type Strings = [ String ]

type Subjects = Strings
type Headers = Strings
type HopefullyExerciseName = HopefullySome String
type Exercises = [ Exercise ]

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
