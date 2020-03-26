module Delete where
import Renaming            (glue,wrap,unwrapAnd,andThen,and)
import ExercisesFromFile   (getToDo,getDone,getMissed)
import Prelude             ((.),not,filter,(-),(!!),(+),elem,Bool(..),(==))
import Prelude             (getLine,IO,Int,sequence)
import Data.Function       ((&),($))
import StringFromExercises (exercisesToString)
import FileManagement      (writeToNextDataKeeper,updateVersion)
import Types               (Strings,Exercises,Exercise(..))
import UsefulForActions    (combine,getChoice,exercisesToFile,getSubjects,showSubjects)
import ShowExercises    (showExercises,getChosen,subIs)
import Control.Monad    ((>=>))

-- deleteFrom list of actions
deleteList = [deleteFrom "todo",deleteFrom "done",deleteFrom "missed"]
deleteFrom = \exType -> (exType&(getAllExs>=>exercisesToFile))`andThen`updateVersion
getAllExs = \case
 "todo"  -> combine [getToDo&delete,getDone,getMissed]
 "done"  -> combine [getToDo,getDone&delete,getMissed]
 "missed"-> combine [getToDo,getDone,getMissed&delete]
delete = getChosen>=>deleteChosen
deleteChosen = \(exs,subNum,exNum)->
 let sub=getSubjects exs!!(subNum-1)
     ex=filter (subIs sub) exs!!(exNum-1)
 in wrap $ filter (not.(==ex)) exs
