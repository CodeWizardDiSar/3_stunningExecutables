module ShowExercises where
import Prelude 
  ( Bool, (!!), filter, (>>=), (==), (-), (>>), Int, IO, String )
import Types
  ( Exercise, Exercises )
import ToSubject
  ( toSubject )
import ToString
  ( toStringForUser, print )
import Data.Function
  ( (&) )
import Renaming 
  ( forEach, wrap, (>>>) )
import UsefulForActions
  ( getChoice, showSubjects, toSubjects )
import Choices
  ( putNumbers )
import UsefulFunctions
  ( doSequentially, tabBefore )

showExercises :: ( Exercises, Int ) -> IO ()
showExercises = \( exs, subNum )->
  let sub = toSubjects exs !! ( subNum - 1 )
  in filter (subIs sub) exs & printExercises

subIs :: String -> Exercise -> Bool
subIs = \subName -> toSubject >>> ( == subName )

printExercises :: Exercises -> IO ()
printExercises =
  forEach toStringForUser >>> putNumbers >>> forEach ( tabBefore >>> print ) >>>
  doSequentially

getExsSubjects :: IO Exercises -> IO Int
getExsSubjects getExs = getExs >>= showSubjects >> getChoice

getChosen :: Exercises -> IO ( Exercises, Int, Int )
getChosen = \exs -> ( exs & showSubjects ) >>
  getChoice >>= \subNum-> ( (exs, subNum ) & showExercises ) >>
  getChoice >>= \exNum -> ( exs, subNum, exNum ) & wrap
