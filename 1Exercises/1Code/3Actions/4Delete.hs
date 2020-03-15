module Delete where
import Renaming            (glue,wrap,unwrapAnd,andThen)
import ExercisesFromFile   (getToDo,getDone,getMissed)
import Prelude             ((.),not,filter,(-),(!!),(+),elem,Bool(..),(==))
import Prelude             (getLine,IO,Int)
import Data.Function       ((&),($))
import StringFromExercises (exercisesToString)
import FileManagement      (writeToNextDataKeeper,updateVersion)
import Types               (Strings,Exercises,Exercise(..))
import UsefulForActions    (getChoice,exercisesToFile,getSubjects,showSubjects)
import ShowExercises    (showExercises,getChosen,subIs)

-- deleteFrom list of actions
deleteList =
 [deleteFrom "todo",deleteFrom "done",deleteFrom "missed"]

deleteFrom = \exType ->
 getAllExs exType`unwrapAnd`exercisesToFile`andThen`updateVersion
  
getAllExs = \case
 "todo"  -> join [delete getToDo,getDone,getMissed]
 "done"  -> join [getToDo,delete getDone,getMissed]
 "missed"-> join [getToDo,getDone,delete getMissed]

join = \case
 []  ->wrap []
 a:as->
  a`unwrapAnd`\a'->
  join as`unwrapAnd`\joinedAs->
  wrap $ glue [a',joinedAs]

delete = \getExs-> getChosen getExs`unwrapAnd`deleteChosen

-- Delete Chosen
deleteChosen = \(exs,subNum,exNum)->
 let
  subs=getSubjects exs
  sub=subs!!(subNum-1)
  subExs=filter (subIs sub) exs
  ex=subExs!!(exNum-1)
 in
  wrap $ filter (not.(==ex)) exs
