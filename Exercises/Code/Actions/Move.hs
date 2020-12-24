module Move where
import Renaming            (printString,wrap,unwrapAnd,andThen,
                            and)
import ExercisesFromFile   (getToDo,getDone,getMissed)
import Prelude             ((.),not,filter,(-),(!!),(+),elem,Bool(..),(==))
import Prelude             (sequence,getLine,IO,Int)
import Data.Function       ((&),($))
import Show                (printEx)
import StringFromExercises (exercisesToString)
import FileManagement      (writeToNextDataKeeper,updateVersion)
import Types               (Strings,Exercises,Exercise(..))
import Add                 (getDate)
import UsefulFunctions     (printStrings)
import UsefulForActions    (combine,showSubjects,getChoice,getSubjects,exercisesToFile)
import ShowExercises       (showExercises,subIs,getChosen)

-- moveFrom list of actions
moveActions = [moveFrom "todo",moveFrom "done",moveFrom "missed"]
moveFrom = \exType -> getAllExs exType`unwrapAnd`exercisesToFile`andThen`updateVersion
getAllExs = \case
 "todo"  -> combine [move getToDo,getDone,getMissed]
 "done"  -> combine [getToDo,move getDone,getMissed]
 "missed"-> combine [getToDo,getDone,move getMissed]
move = \getExs-> getChosen getExs`unwrapAnd`moveChosen

-- Move Chosen
moveChosen = \(exs,subNum,exNum)->
 let sub=getSubjects exs!!(subNum-1)
     ex=filter (subIs sub) exs!!(exNum-1)
 in moveOld ex`unwrapAnd`\newEx-> newEx:(filter (not.(==ex)) exs)&wrap

moveOld = \ex->
 printStrings ["Move To?","\t1: To Do","\t2: Done","\t3: Missed"]`andThen`
 getLine`unwrapAnd` \case
 "1"->moveToToDo    ex
 "2"->moveTo Done   ex
 "3"->moveTo Missed ex
 _  ->printString "what?"`andThen`moveOld ex

moveToToDo = \case 
 ToDo   a b -> ToDo a b&wrap
 Done   a   -> getDate`unwrapAnd`(ToDo a`and`wrap)
 Missed a   -> getDate`unwrapAnd`(ToDo a`and`wrap)
 
moveTo = \x-> \case
 ToDo   a b -> x a&wrap
 Done   a   -> x a&wrap
 Missed a   -> x a&wrap
