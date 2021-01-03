module Types where

import Prelude ( Eq, String, Int, Ord )

-- Always a pleasure to have types (sorry python)
data HopefullySome a = IndeedItIs { getA :: a } | Nothing
  deriving ( Eq )

data ExercisesAndChosen =
  ExercisesAndChosen { exercises :: Exercises, chosen :: Exercise }
  deriving ( Eq )

data Exercise =
  ToDo { toDo :: ToDoExercise } |
  Done { done :: DoneExercise } |
  Missed { missed :: MissedExercise }
  deriving ( Eq )

data ExerciseType = ToDoEx | DoneEx | MissedEx
  deriving ( Eq )

data ToDoExercise = ToDoExercise { exData :: ExData , date :: Date }
  deriving ( Eq )

type DoneExercise = ExData
type MissedExercise = ExData

data ExData =
  ED { subject :: Subject, number :: ExerciseNumber, name :: HopefullyExerciseName }
  deriving ( Eq )
   
data Date = Date { day :: Day, month :: Month, year :: Year }
  deriving ( Eq )

type Day = Int
type Month = Int
type Year = Int

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
type HopefullyExerciseName = HopefullySome String
type Exercises = [ Exercise ]
