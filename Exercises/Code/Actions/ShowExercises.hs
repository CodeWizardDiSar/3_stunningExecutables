module ShowExercises where
import Prelude 
  ( Bool( True ), elem, (!!), filter, (>>=), (==), (-), (+), (>>), Int, IO, String, foldl
  , otherwise )
import Types
  ( Exercise( exerciseData ), ExerciseData ( subjectName ), Exercises, Subject, Subjects
  , toStringForUser )
import Data.Function
  ( (&) )
import Renaming 
  ( glue, wrap, convertIntToString, printString, (>>>) )
import Show 
  ( printEx )
import UsefulForActions
  ( getChoice, showSubjects )

showExercises :: ( Exercises, Int ) -> IO ()
showExercises = \( exs, subNum )->
  let sub = getSubjects exs !! ( subNum - 1 )
  in filter (subIs sub) exs & printExercises

subIs :: String -> Exercise -> Bool
subIs = \subName -> getExerciseSubjectName >>> ( == subName )

getExerciseSubjectName :: Exercise -> Subject
getExerciseSubjectName = exerciseData >>> subjectName 

printExercises :: Exercises -> IO ()
printExercises = printExercise 1

printExercise :: Int -> Exercises -> IO ()
printExercise = \i -> \case
  [] -> wrap ()
  ex:exs -> ( glue [ "\t", convertIntToString i, ": ", toStringForUser ex ] & printString ) >> 
    printExercise ( i + 1 ) exs

getSubjects :: Exercises -> Subjects
getSubjects = \case
  [] -> []
  ex:exs ->
    let sub = getExerciseSubjectName ex
        subs = getSubjects exs
    in elem sub subs & \case
      True -> subs
      _ -> sub:subs

getSubjects2 :: Exercises -> Subjects
getSubjects2 = foldl ( \subs ex -> getSubjects2h ( getExerciseSubjectName ex ) subs ) [] 

getSubjects2h :: Subject -> Subjects -> Subjects
getSubjects2h subject subjects
  | elem subject subjects = subjects
  | otherwise = subject : subjects

getExsSubjects :: IO Exercises -> IO Int
getExsSubjects getExs = getExs >>= showSubjects >> getChoice

getChosen :: Exercises -> IO ( Exercises, Int, Int )
getChosen = \exs -> ( exs & showSubjects ) >>
  getChoice >>= \subNum-> ( (exs, subNum ) & showExercises ) >>
  getChoice >>= \exNum -> ( exs, subNum, exNum ) & wrap
