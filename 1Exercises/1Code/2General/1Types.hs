{-# LANGUAGE ConstraintKinds #-} 
module Types where
import Prelude hiding (Word,Show,show)

data HopefullySome a = Indeed a|Nothing
data Exercise = Done   ExerciseData
              | Missed ExerciseData
              | ToDo   ExerciseData Date

type Strings  = [String]
type Line     = String  
type Lines    = [Line] 
type Word     = String
type Words    = [Word]
type Message  = String
type Messages = [Message]
type Boolean  = Bool

type Day                   = Int
type Month                 = Int
type Year                  = Int
type Date                  = (Day,Month,Year)
type SubjectName           = String
type ExerciseNumber        = String
type HopefullyExerciseName = HopefullySome String
type Exercises             = [Exercise]
type ExerciseData = (SubjectName,ExerciseNumber,HopefullyExerciseName)

class FromString a where fromString :: String->a
class ToString a   where toString   :: a->String
class Show a       where show       :: a->String
