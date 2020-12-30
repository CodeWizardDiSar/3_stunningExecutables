module Types where
import Prelude ( Eq, String, Int )

-- Always a pleasure to have types (sorry python)
data HopefullySome a = IndeedItIs { getA :: a } | Nothing
  deriving ( Eq )

data Exercise =
  ToDo { toDo :: ToDoExercise } |
  Done { done :: DoneExercise } |
  Missed { missed :: MissedExercise }
  deriving ( Eq )

data ExerciseType = ToDoEx | DoneEx | MissedEx
  deriving ( Eq )

data ToDoExercise = ToDoExercise { toDoData :: ExerciseData , date :: Date }
  deriving ( Eq )

data DoneExercise = DoneExercise { doneData :: ExerciseData  }
  deriving ( Eq )

data MissedExercise = MissedExercise { missedData :: ExerciseData  }
  deriving ( Eq )

data ExerciseData =
  ED { subject :: Subject, number :: ExerciseNumber, name :: HopefullyExerciseName }
  deriving ( Eq )
   

data Date = Date { day :: Day, month :: Month, year :: Year }
  deriving ( Eq )

data Day = Day Int
  deriving ( Eq )
data Month = Month Int
  deriving ( Eq )
data Year = Year Int
  deriving ( Eq )

type Path = String
type Subject = String
type ExerciseNumber = String
type HeaderRow = String
type ChoicesWithTitle = String
type Title = String

type Strings = [ String ]

type Subjects = Strings
type Choices = Strings
type HopefullyExerciseName = HopefullySome String
type Exercises = [ Exercise ]
