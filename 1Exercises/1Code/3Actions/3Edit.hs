module Edit where
import Add                 (getDate)
import UsefulForActions    (combine,askFor,exercisesToFile,
                            getSubjects)
import Renaming            (printString,wrap,unwrapAnd,
                            andThen,and)
import ExercisesFromFile   (getToDo,getDone,getMissed)
import Prelude             ((.),not,filter,(-),(!!),(==),
                            getLine)
import Data.Function       ((&))
import FileManagement      (updateVersion)
import Types               (Exercise(..),HopefullySome(..))
import UsefulFunctions     (printStrings)
import ShowExercises       (getChosen,subIs)
import Control.Monad       ((>=>))

-- edit list of actions
editList = [edit "todo",edit "done",edit "missed"]
edit     = getAllExs>=>exercisesToFile>=> \_->updateVersion
getAllExs = \case
 "todo"  -> combine [getToDo&getAndEditChosen,getDone,
                     getMissed]
 "done"  -> combine [getToDo,getDone&getAndEditChosen,
                     getMissed]
 "missed"-> combine [getToDo,getDone,
                     getMissed&getAndEditChosen]
getAndEditChosen = getChosen>=>editChosen

-- Edit Chosen
editChosen = \(exs,subNum,exNum)->
 let sub=getSubjects exs!!(subNum-1)
     ex=filter (subIs sub) exs!!(exNum-1)
 in modify ex`unwrapAnd`((:filter (not.(==ex)) exs)`and`wrap)

modify = \case
 ToDo   (s,eNum,eName) d ->
  chooseAttributeWithDate`unwrapAnd` \case
   "1"->getSubject`unwrapAnd` \newSub  -> ToDo (newSub,eNum,
                                                eName) d&wrap
   "2"->getENum   `unwrapAnd` \newENum -> ToDo (s,newENum,
                                                eName) d&wrap
   "3"->getEName  `unwrapAnd` \newEName-> ToDo (s,eNum,
                                                IndeedItIs
                                                newEName)
                                                d &wrap
   "4"->getDate   `unwrapAnd` \newDate -> ToDo (s,eNum,eName)
                                                newDate&wrap
 Done   (s,eNum,eName)   ->
  chooseAttribute`unwrapAnd` \case
   "1"->getSubject`unwrapAnd` \newSub  ->Done (newSub,eNum,
                                               eName)&wrap
   "2"->getENum   `unwrapAnd` \newENum ->Done (s,newENum,eName)
                                         &wrap
   "3"->getEName  `unwrapAnd` \newEName->Done (s,eNum,
                                               IndeedItIs
                                               newEName)&wrap
 Missed (s,eNum,eName)   ->
  chooseAttribute`unwrapAnd` \case
   "1"->getSubject`unwrapAnd` \newSub  ->Missed (newSub,eNum,
                                                 eName)&wrap
   "2"->getENum   `unwrapAnd` \newENum ->Missed (s,newENum,
                                                 eName)&wrap
   "3"->getEName  `unwrapAnd` \newEName->Missed (s,eNum,
                                                 IndeedItIs
                                                 newEName)&wrap

chooseAttribute         = printBasic`andThen`getLine
chooseAttributeWithDate = printBasic`andThen`printDate`andThen`
                          getLine

printBasic = printStrings ["1: Subject","2: Exercise Number",
                           "3: Exercise Name"]
printDate  = printString "4: Date"

getSubject = askFor "New Subject?"
getENum    = askFor "New Exercise Number?"
getEName   = askFor "New Exercise Name?"
