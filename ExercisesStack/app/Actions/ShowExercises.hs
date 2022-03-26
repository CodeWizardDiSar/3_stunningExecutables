module Actions.ShowExercises where

import Prelude 
  ( Bool, (!!), filter, (>>=), (==), (-), (>>), Int, IO, ($), getLine, (+) )
import Types
  ( Exercise, Exercises, Subject, Subjects, ExercisesAndChosen( ExercisesAndChosen ) )
import TypeClasses.ToSubject
  ( toSubject, toSubjects )
import TypeClasses.ToString
  ( toStringForUser, print, toString )
import Data.Function
  ( (&) )
import VeryUseful.Renaming 
  ( forEach, wrap, (.>), glue )
import Choices
  ( putNumbers )
import VeryUseful.UsefulFunctions
  ( doSequentially, tabBefore )
import TypeClasses.FromString
  ( fromString )

getChosen :: Exercises -> IO ExercisesAndChosen
getChosen = \exs -> ( exs & showSubjects ) >>
  getChoice >>= \subNum-> ( showSubjectExercises exs subNum ) >>
  getChoice >>= \exerciseNumber ->
    let sub = toChosenSub exs subNum
        ex = subjectExercises sub exs !! ( exerciseNumber - 1 )
    in ExercisesAndChosen exs ex & wrap

class ToChosenSub a where
  toChosenSub :: a -> Int -> Subject

instance ToChosenSub Exercises where
  toChosenSub exs = exs & toSubjects & toChosenSub

instance ToChosenSub Subjects where
  toChosenSub subs = ( + ( -1 ) ) .> ( subs !! )

showSubjects :: Exercises -> IO ()
showSubjects = toSubjects .> printSubjects 1

getChoice :: IO Int
getChoice = getLine >>= fromString .> wrap

showSubjectExercises :: Exercises -> Int -> IO ()
showSubjectExercises exercises subNum =
  filter ( subIs $ toSubjects exercises !! ( subNum - 1 ) ) exercises & printExercises

subIs :: Subject -> Exercise -> Bool
subIs subject = toSubject .> ( == subject )

printExercises :: Exercises -> IO ()
printExercises =
  forEach toStringForUser .> putNumbers .> forEach ( tabBefore .> print ) .>
  doSequentially

subjectExercises :: Subject -> Exercises -> Exercises
subjectExercises subject = filter ( subIs subject )

printSubjects :: Int -> Subjects -> IO ()
printSubjects = \i -> \case
  [] -> wrap ()
  sub:subs -> ( [ "\t", i & toString, ": ", sub ] & glue & print ) >>
              printSubjects ( i + 1 ) subs
