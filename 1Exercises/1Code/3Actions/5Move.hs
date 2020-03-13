module Move where
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
import Delete (join,getSubjects,showSubjects,getChoice,showExercises)
import Delete (subIs)
import Add (date)
import UsefulFunctions (printStrings)

-- moveFrom list of actions
moveList =
 [moveFrom "todo",moveFrom "done",moveFrom "missed"]

moveFrom = \exType ->
 getAllExs exType`unwrapAnd`
 (exercisesToString`and`writeToNextDataKeeper)`andThen`
 updateVersion
  
getAllExs = \case
 "todo"  -> join [move getToDo,getDone,getMissed]
 "done"  -> join [getToDo,move getDone,getMissed]
 "missed"-> join [getToDo,getDone,move getMissed]

move = \getExs->
 getExs        `unwrapAnd`\exs   ->((exs)&
 showSubjects) `andThen`
 getChoice     `unwrapAnd`\subNum->((exs,subNum)&
 showExercises)`andThen`
 getChoice     `unwrapAnd`\exNum ->(exs,subNum,exNum)&
 moveChosen

-- Move Chosen
moveChosen = \(exs,subNum,exNum)->
 getSubjects exs`unwrapAnd`\subs->
 let
  sub=subs!!(subNum-1)
  subExs=filter (subIs sub) exs
  ex=subExs!!(exNum-1)
 in
  moveOld ex`unwrapAnd`\newEx->
  wrap $ newEx:(filter (not.(==ex)) exs)

moveOld = \ex->
 printStrings
  ["Move To?" ,"\t1: To Do" ,
   "\t2: Done","\t3: Missed"]`andThen`
 getLine`unwrapAnd` \case
 "1"->moveToToDo   ex
 "2"->moveToDone   ex
 "3"->moveToMissed ex
 _  ->printString "what?"`andThen`moveOld ex

moveToToDo = \case 
 ToDo   a b -> wrap $ ToDo a b
 Done   a   -> date`unwrapAnd`(ToDo a`and`wrap)
 Missed a   -> date`unwrapAnd`(ToDo a`and`wrap)
 
moveToDone = \case 
 ToDo   a b -> wrap $ Done a
 Done   a   -> wrap $ Done a
 Missed a   -> wrap $ Done a
 
moveToMissed = \case 
 ToDo   a b -> wrap $ Missed a
 Done   a   -> wrap $ Missed a
 Missed a   -> wrap $ Missed a
