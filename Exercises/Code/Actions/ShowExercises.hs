module ShowExercises where
import Prelude 
  ( Bool, (!!), filter, (>>=), (==), (-), (>>), Int, IO )
import Types
  ( Exercise, Exercises, Subject )
import ToSubject
  ( toSubject, toSubjects )
import ToString
  ( toStringForUser, print )
import Data.Function
  ( (&) )
import Renaming 
  ( forEach, wrap, (.>) )
import UsefulForActions
  ( getChoice, showSubjects )
import Choices
  ( putNumbers )
import UsefulFunctions
  ( doSequentially, tabBefore )

showSubjectExercises :: ( Exercises, Int ) -> IO ()
showSubjectExercises ( exs, subNum ) =
  let sub = toSubjects exs !! ( subNum - 1 )
  in filter (subIs sub) exs & printExercises

subIs :: Subject -> Exercise -> Bool
subIs subject = toSubject .> ( == subject )

printExercises :: Exercises -> IO ()
printExercises =
  forEach toStringForUser .> putNumbers .> forEach ( tabBefore .> print ) .>
  doSequentially

getExsSubjects :: IO Exercises -> IO Int
getExsSubjects getExs = getExs >>= showSubjects >> getChoice

getChosen :: Exercises -> IO ( Exercises, Int, Int )
getChosen = \exs -> ( exs & showSubjects ) >>
  getChoice >>= \subNum-> ( (exs, subNum ) & showSubjectExercises ) >>
  getChoice >>= \exNum -> ( exs, subNum, exNum ) & wrap
