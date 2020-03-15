module Move where
import Renaming            (glue,printString,wrap,unwrapAnd,andThen,and)
import Renaming            (convertIntToString,convertIntFromString)
import ExercisesFromFile   (getToDo,getDone,getMissed)
import Prelude             ((.),not,filter,(-),(!!),(+),elem,Bool(..),(==))
import Prelude             (getLine,IO,Int)
import Data.Function       ((&),($))
import Show                (printEx)
import StringFromExercises (exercisesToString)
import FileManagement      (writeToNextDataKeeper,updateVersion)
import Types               (Strings,Exercises,Exercise(..))
import Delete              (join)
import Add                 (getDate)
import UsefulFunctions     (printStrings)
import UsefulForActions    (showSubjects,getChoice,getSubjects,exercisesToFile)
import ShowExercises       (showExercises,subIs,getChosen)

-- moveFrom list of actions
moveList =
 [moveFrom "todo",moveFrom "done",moveFrom "missed"]

moveFrom = \exType ->
 getAllExs exType`unwrapAnd`exercisesToFile`andThen`updateVersion
  
getAllExs = \case
 "todo"  -> join [move getToDo,getDone,getMissed]
 "done"  -> join [getToDo,move getDone,getMissed]
 "missed"-> join [getToDo,getDone,move getMissed]

move = \getExs-> getChosen getExs`unwrapAnd`moveChosen

-- Move Chosen
moveChosen = \(exs,subNum,exNum)->
 let
  subs = getSubjects exs
  sub=subs!!(subNum-1)
  subExs=filter (subIs sub) exs
  ex=subExs!!(exNum-1)
 in
  moveOld ex`unwrapAnd`\newEx->
  wrap $ newEx:(filter (not.(==ex)) exs)

moveOld = \ex->
 printStrings ["Move To?","\t1: To Do","\t2: Done","\t3: Missed"]`andThen`
 getLine`unwrapAnd` \case
 "1"->moveToToDo    ex
 "2"->moveTo Done   ex
 "3"->moveTo Missed ex
 _  ->printString "what?"`andThen`moveOld ex

moveToToDo = \case 
 ToDo   a b -> wrap $ ToDo a b
 Done   a   -> getDate`unwrapAnd`(ToDo a`and`wrap)
 Missed a   -> getDate`unwrapAnd`(ToDo a`and`wrap)
 
moveTo = \x-> \case
 ToDo   a b -> wrap $ x a
 Done   a   -> wrap $ x a
 Missed a   -> wrap $ x a
