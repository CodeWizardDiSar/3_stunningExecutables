module ShowExercises where
import Prelude 
  ( Bool, elem, (!!), filter, (>>=), (==), (-), (>>), Int, IO, String, foldl , otherwise
  , (++) )
import Types
  ( Exercise( exerciseData ), ExerciseData ( subjectName ), Exercises, Subject, Subjects
  , toStringForUser, toSubject )
import Data.Function
  ( (&) )
import Renaming 
  ( forEach, glue, wrap, convertIntToString, printString, (>>>) )
import Show 
  ( printEx )
import UsefulForActions
  ( getChoice, showSubjects )
import Choices
  ( numbered )
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
  forEach toStringForUser >>> numbered >>> forEach ( tabBefore >>> printString ) >>>
  doSequentially

toSubjects :: Exercises -> Subjects
toSubjects = foldl ( \subs ex -> addToSubjects ( toSubject ex ) subs ) [] 

addToSubjects :: Subject -> Subjects -> Subjects
addToSubjects subject subjects
  | elem subject subjects = subjects
  | otherwise = subjects ++ [ subject ]

getExsSubjects :: IO Exercises -> IO Int
getExsSubjects getExs = getExs >>= showSubjects >> getChoice

getChosen :: Exercises -> IO ( Exercises, Int, Int )
getChosen = \exs -> ( exs & showSubjects ) >>
  getChoice >>= \subNum-> ( (exs, subNum ) & showExercises ) >>
  getChoice >>= \exNum -> ( exs, subNum, exNum ) & wrap
