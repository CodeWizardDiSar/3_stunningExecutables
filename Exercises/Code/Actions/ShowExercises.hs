module ShowExercises where
import Prelude 
  (Bool(..), elem, (!!), filter, (>>=), (==), (-), (+), (>>), Int, IO, String)
import Data.Function
  ((&))
import Renaming 
  (glue, wrap, convertIntToString, printString)
import Show 
  (printEx)
import Types
  (Exercise(..), Exercises)
import UsefulForActions
  (getChoice, showSubjects)

showExercises :: (Exercises, Int) -> IO ()
showExercises = \(exs,subNum)->
  let sub = getSubjects exs !! (subNum-1)
  in filter (subIs sub) exs & printExercises

subIs :: String -> Exercise -> Bool
subIs = \subName ex -> ex & getData & \(s,_,_) -> s == subName

printExercises :: Exercises -> IO ()
printExercises = printExercise 1

printExercise :: Int -> Exercises -> IO ()
printExercise = \i -> \case
  [] ->
    wrap ()
  ex:exs ->
    (glue [convertIntToString i, ": "]&printString) >>
    printEx ex >>
    printExercise (i+1) exs

getSubjects :: Exercises -> [String]
getSubjects = \case
  [] ->
    []
  ex:exs ->
    let sub = ex & getData & \(s,_,_)->s
        subs = getSubjects exs
    in elem sub subs & \case
      True ->
        subs
      _ ->
        sub:subs

getExsSubjects :: IO Exercises -> IO Int
getExsSubjects getExs = getExs >>= showSubjects >> getChoice

getChosen :: Exercises -> IO (Exercises, Int, Int)
getChosen = \exs -> (exs & showSubjects) >>
  getChoice >>= \subNum-> ( (exs, subNum) & showExercises) >>
  getChoice >>= \exNum -> (exs, subNum, exNum) & wrap
