module TypeClasses.ToSubject where

import Prelude
  ( String, foldl, elem, otherwise, (++) )
import VeryUseful.Renaming
  ( (.>) )
import Types
  ( Subject, ExData( subject ), Exercise( ToDo, Done, Other ), ToDoExercise( exData )
  , DoneExercise, OtherExercise, Exercises, Subjects )

class ToSubject a where toSubject :: a -> Subject

instance ToSubject Exercise where
  toSubject = \case 
    ToDo exercise -> toSubject exercise
    Done exercise -> toSubject exercise
    Other exercise -> toSubject exercise

instance ToSubject ToDoExercise where
  toSubject = exData .> toSubject

instance ToSubject ExData where
  toSubject = subject

class ToSubjects a where toSubjects :: a -> Subjects

instance ToSubjects Exercises where
  toSubjects =
    foldl (\subjects exercise -> addToSubjects ( toSubject exercise ) subjects ) []

addToSubjects :: Subject -> Subjects -> Subjects
addToSubjects subject subjects
  | elem subject subjects = subjects
  | otherwise = subjects ++ [ subject ]
