module ShowExercises where
import Prelude 
  (Bool(..), elem, (!!), filter, (>>=), (==), (-), (+), (>>))
import Data.Function
  ((&))
import Renaming 
  (glue, wrap, convertIntToString, printString)
import Show 
  (printEx)
import Types
  (Exercise(..))
import UsefulForActions
  (getChoice, showSubjects)
import Control.Concurrent.Async
  (concurrently)

showExercises = \(exs,subNum)->
  let sub = getSubjects exs !! (subNum-1)
  in filter (subIs sub) exs & printExercises

subIs = \subName ex -> ex & getData & \(s,_,_) -> s == subName

printExercises = printExercise 1
printExercise = \i -> \case
  [] ->
    wrap ()
  ex:exs ->
    (glue [convertIntToString i, ": "]&printString) >>
    printEx ex >>
    printExercise (i+1) exs

inputOutputPair :: (a -> b) -> a -> (a, b)
inputOutputPair f a = (a, f a)

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

getExsSubjects getExs = concurrently (getExs >>= showSubjects) getChoice

getChosen = \exs -> (exs & showSubjects) >>
  getChoice >>= \subNum-> ( (exs, subNum) & showExercises) >>
  getChoice >>= \exNum -> (exs, subNum, exNum) & wrap
