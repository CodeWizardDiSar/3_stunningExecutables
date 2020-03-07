module Delete where
import Renaming (glue,printString,wrap,unwrapAnd,andThen,and)
import Renaming (convertIntToString,convertIntFromString)
import ExercisesFromFile (getToDo,getDone,getMissed)
import Prelude ((.),not,filter,(-),(!!),(+),elem,Bool(..),(==))
import Prelude (getLine,IO,Int)
import Data.Function ((&),($))
import Show (printEx)
import FileFromExercises (exercisesToString)
import FileManagement (writeToNextDataKeeper,updateVersion)
import Types (Strings,Exercises,Exercise(..))

-- deleteFrom list of actions
deleteList =
 [deleteFrom "todo",deleteFrom "done",deleteFrom "missed"]

deleteFrom = \exType ->
 getAllExs exType`unwrapAnd`
 (exercisesToString`and`writeToNextDataKeeper)`andThen`
 updateVersion
  
getAllExs = \case
 "todo"  -> join [deleteGet getToDo,getDone,getMissed]
 "done"  -> join [getToDo,deleteGet getDone,getMissed]
 "missed"-> join [getToDo,getDone,deleteGet getMissed]

join = \case
 []  ->wrap []
 a:as->
  a`unwrapAnd`\a'->
  join as`unwrapAnd`\joinedAs->
  wrap $ glue [a',joinedAs]

deleteGet = \getExs->
 getExs        `unwrapAnd`\exs   ->((exs)&
 showSubjects) `andThen`
 getChoice     `unwrapAnd`\subNum->((exs,subNum)&
 showExercises)`andThen`
 getChoice     `unwrapAnd`\exNum ->(exs,subNum,exNum)&
 deleteChosen

-- Show Subjects
showSubjects = \exs->(exs&
 getSubjects)`unwrapAnd`
 printSubjects 1

getSubjects = \case
 []    ->wrap []
 ex:exs->
  getSub ex& \sub->
  getSubjects exs`unwrapAnd`\subs->
  elem sub subs& \case 
   True->wrap subs
   _   ->wrap (sub:subs)

printSubjects :: Int->Strings->IO ()
printSubjects = \i-> \case
 []      ->wrap ()
 sub:subs->
  (printString $ glue [convertIntToString i,": ",sub])`andThen`
  printSubjects (i+1) subs
 
getSub = \case
 Done   (sub,_,_)  ->sub
 Missed (sub,_,_)  ->sub
 ToDo   (sub,_,_) _->sub

-- Get Choice
getChoice =
 getLine`unwrapAnd`
 (convertIntFromString`and`wrap)

-- Show Exercises
showExercises = \(exs,subNum)->
 getSubjects exs`unwrapAnd`\subs->
 let sub=subs!!(subNum-1) in
 printExercises 1 $ filter (subIs sub) exs

subIs = \sub-> \case
 ToDo   (subName,_,_) _->subName==sub
 Done   (subName,_,_)  ->subName==sub
 Missed (subName,_,_)  ->subName==sub
 
printExercises :: Int->Exercises->IO ()
printExercises = \i-> \case
 []    -> wrap ()
 ex:exs->
  (printString$glue [convertIntToString i,": "])`andThen`
  printEx ex`andThen`
  printExercises (i+1) exs

-- Delete Chosen
deleteChosen = \(exs,subNum,exNum)->
 getSubjects exs`unwrapAnd`\subs->
 let
  sub=subs!!(subNum-1)
  subExs=filter (subIs sub) exs
  ex=subExs!!(exNum-1)
 in
  wrap $ filter (not.(==ex)) exs
