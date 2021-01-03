module ShowExercises where
import Prelude 
  ( Bool, (!!), filter, (>>=), (==), (-), (>>), Int, IO, ($), getLine, (+) )
import Types
  ( Exercise, Exercises, Subject, Subjects, ExercisesAndChosen( ExercisesAndChosen ) )
import ToSubject
  ( toSubject, toSubjects )
import ToString
  ( toStringForUser, print, toString )
import Data.Function
  ( (&) )
import Renaming 
  ( forEach, wrap, (.>), glue )
import Choices
  ( putNumbers )
import UsefulFunctions
  ( doSequentially, tabBefore )
import FromString
  ( fromString )

showSubjectExercises :: Exercises -> Int -> IO ()
showSubjectExercises exercises subjectNumber =
  filter ( subIs $ toSubjects exercises !! ( subjectNumber - 1 ) ) exercises & printExercises

subIs :: Subject -> Exercise -> Bool
subIs subject = toSubject .> ( == subject )

printExercises :: Exercises -> IO ()
printExercises =
  forEach toStringForUser .> putNumbers .> forEach ( tabBefore .> print ) .>
  doSequentially

getChosen :: Exercises -> IO ExercisesAndChosen
getChosen = \exercises -> ( exercises & showSubjects ) >>
  getChoice >>= \subjectNumber-> ( showSubjectExercises exercises subjectNumber ) >>
  getChoice >>= \exerciseNumber ->
    let sub = exercises & toSubjects & chosenSubject subjectNumber
        ex = subjectExercises sub exercises !! ( exerciseNumber - 1 )
    in ExercisesAndChosen exercises ex & wrap

chosenSubject :: Int -> Subjects -> Subject
chosenSubject subjectNumber subjects = subjects !! ( subjectNumber - 1 )

subjectExercises :: Subject -> Exercises -> Exercises
subjectExercises subject = filter ( subIs subject )

showSubjects :: Exercises -> IO ()
showSubjects = toSubjects .> printSubjects 1

printSubjects :: Int -> Subjects -> IO ()
printSubjects = \i -> \case
  [] -> wrap ()
  sub:subs -> ( [ "\t", i & toString, ": ", sub ] & glue & print ) >>
              printSubjects ( i + 1 ) subs

getChoice :: IO Int
getChoice = getLine >>= fromString .> wrap
