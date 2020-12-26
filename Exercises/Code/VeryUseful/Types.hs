module Types where
import Prelude (Eq,String,Int,Bool)

-- Always a pleasure to have types (sorry python)
data HopefullySome a = IndeedItIs { getA :: a} | Nothing
  deriving ( Eq )

data Exercise =
  Done { getData :: ExerciseData} |
  Missed {getData :: ExerciseData} |
  ToDo { getData :: ExerciseData, getTDate :: Date }
  deriving ( Eq )

type Path = String
type Strings = [ String ]
type Date = [ Int ]
type SubjectName = String
type ExerciseNumber = String
type HopefullyExerciseName = HopefullySome String
type Exercises = [ Exercise ]
type ExerciseData = ( SubjectName, ExerciseNumber, HopefullyExerciseName )

-- Even more of a pleasure, type classes
class FromStringTo a where toType :: String -> a
class FileVersionOf a where toFileString :: a -> String
class Show a where show :: a -> String
