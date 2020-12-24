module Delete where
import Renaming            (wrap)
import ExercisesFromFile   (getToDo,getDone,getMissed)
import Prelude             ((.),not,filter,(-),(!!),(==))
import Data.Function       ((&))
import StringFromExercises (exercisesToString)
import FileManagement      (updateVersion)
import Types               (Strings,Exercises,Exercise(..))
import UsefulForActions    (combine,exercisesToFile,
                            getSubjects)
import ShowExercises       (getChosen,subIs)
import Control.Monad       ((>=>))

-- deleteFrom list of actions
deleteActions = [deleteFrom "todo",deleteFrom "done",deleteFrom
                                                  "missed"]
deleteFrom = getAllExs>=>exercisesToFile>=> \_->updateVersion
getAllExs = \case
 "todo"  -> combine [getToDo&getDeleteChosen,getDone,getMissed]
 "done"  -> combine [getToDo,getDone&getDeleteChosen,getMissed]
 "missed"-> combine [getToDo,getDone,getMissed&getDeleteChosen]
getDeleteChosen = getChosen>=>deleteChosen
deleteChosen = \(exs,subNum,exNum)->
 let sub=getSubjects exs!!(subNum-1)
     ex=filter (subIs sub) exs!!(exNum-1)
 in filter (not.(==ex)) exs&wrap
