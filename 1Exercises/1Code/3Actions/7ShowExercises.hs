module ShowExercises where
import Prelude       (Bool(..),elem,(!!),filter,concat,(==),(-))
import Data.Function ((&),($))
import Renaming (unwrapAnd,wrap,andThen,convertIntToString,printString)
import Show    (printEx)
import Types    (Exercise(..))
import UsefulForActions  (getChoice,showSubjects)

showExercises = \(exs,subNum)->
 let subs=getSubjects exs in
 filter (subIs (subs!!(subNum-1))) exs&printExercises

subIs = \subName ex-> getData ex& \(s,_,_)->s==subName
printExercises = printExercise 1

printExercise = \i -> \case
 []     ->wrap ()
 ex:exs->
  (concat [convertIntToString i,": "]&printString)`andThen`
  printEx ex`andThen`printExercise 2 exs

getSubjects = \case 
 []     -> []
 ex:exs -> 
  let
   sub  = ex&getData& \(s,_,_)->s
   subs = getSubjects exs 
  in
  elem sub subs& \case 
   True -> subs
   _    -> sub:subs

getChosen = \getExs->
 getExs        `unwrapAnd`\exs   ->((exs)&
 showSubjects) `andThen`
 getChoice     `unwrapAnd`\subNum->((exs,subNum)&
 showExercises)`andThen`
 getChoice     `unwrapAnd`\exNum ->(exs,subNum,exNum)&wrap
