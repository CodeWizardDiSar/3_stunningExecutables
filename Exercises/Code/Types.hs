module Types where

import Prelude ( Eq, String, Int, Ord )

-- Always a pleasure to have types (sorry python)
data HopefullySome a = IndeedItIs a | Nothing
  deriving ( Eq )

data ExercisesAndChosen =
  ExercisesAndChosen { exercises :: Exercises, chosen :: Exercise }
  deriving ( Eq )

data Exercise =
  ToDo { toDo :: ToDoExercise } |
  Done { done :: DoneExercise } |
  Other { missed :: OtherExercise }
  deriving ( Eq )

data ExerciseType = ToDoEx | DoneEx | OtherEx
  deriving ( Eq )

data ToDoExercise = ToDoExercise { exData :: ExData , date :: Date }
  deriving ( Eq )

type DoneExercise = ExData
type OtherExercise = ExData

data ExData =
  ED { subject :: Subject, number :: ExerciseNumber, name :: HopefullyExName }
  deriving ( Eq )
   
data Date = Date { day :: Day, month :: Month }
  deriving ( Eq )

type Day = Int
type Month = Int

type Path = String
type Subject = String
type ExerciseNumber = String
type HeaderRow = String
type Choice = String
type ChoicesWithTitle = String
type Title = String

type Strings = [ String ]

type Subjects = [ Subject ]
type Choices = [ Choice ]
type HopefullyExName = HopefullySome String
type Exercises = [ Exercise ]
