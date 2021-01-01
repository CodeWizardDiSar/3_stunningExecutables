module ShowExercises where
import Prelude 
  ( Bool, (!!), filter, (>>=), (==), (-), (>>), Int, IO, ($), getLine, (+) )
import Types
  ( Exercise, Exercises, Subject, Subjects )
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
showSubjectExercises exs subNum =
  filter (subIs $ toSubjects exs !! ( subNum - 1 )) exs & printExercises

subIs :: Subject -> Exercise -> Bool
subIs subject = toSubject .> ( == subject )

printExercises :: Exercises -> IO ()
printExercises =
  forEach toStringForUser .> putNumbers .> forEach ( tabBefore .> print ) .>
  doSequentially

getChosen :: Exercises -> IO ( Exercises, Int, Int )
getChosen = \exs -> ( exs & showSubjects ) >>
  getChoice >>= \subNum-> ( showSubjectExercises exs subNum ) >>
  getChoice >>= \exNum -> ( exs, subNum, exNum ) & wrap

showSubjects :: Exercises -> IO ()
showSubjects = toSubjects .> printSubjects 1

printSubjects :: Int -> Subjects -> IO ()
printSubjects = \i -> \case
  [] -> wrap ()
  sub:subs -> ( [ "\t", i & toString, ": ", sub ] & glue & print ) >>
    printSubjects ( i + 1 ) subs

getChoice :: IO Int
getChoice = getLine >>= fromString .> wrap
