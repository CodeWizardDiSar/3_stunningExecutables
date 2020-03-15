module Edit where
import Add                 (getDate)
import UsefulForActions    (getChoice,printSubjects,askFor,exercisesToFile,getSubjects,showSubjects,getSub)
import Renaming            (glue,printString,wrap,unwrapAnd,andThen,and)
import Renaming            (convertIntToString,convertIntFromString)
import ExercisesFromFile   (getToDo,getDone,getMissed)
import Prelude             ((.),not,filter,(-),(!!),(+),elem,Bool(..),(==))
import Prelude             (getLine,IO,Int)
import Data.Function       ((&),($))
import Show                (printEx)
import StringFromExercises (exercisesToString)
import FileManagement      (writeToNextDataKeeper,updateVersion)
import Types               (Strings,Exercises,Exercise(..),HopefullySome(..))
import UsefulFunctions     (printStrings)
import Delete              (join)
import ShowExercises       (getChosen,showExercises,subIs)

-- edit list of actions
editList = [edit "todo",edit "done",edit "missed"]

edit = \exType -> getAllExs exType`unwrapAnd`exercisesToFile`andThen`updateVersion

getAllExs = \case
 "todo"  -> join [editGet getToDo,getDone,getMissed]
 "done"  -> join [getToDo,editGet getDone,getMissed]
 "missed"-> join [getToDo,getDone,editGet getMissed]

editGet = \getExs-> getChosen getExs`unwrapAnd`editChosen

-- Edit Chosen
editChosen = \(exs,subNum,exNum)->
 let
  subs = getSubjects exs
  sub=subs!!(subNum-1)
  subExs=filter (subIs sub) exs
  ex=subExs!!(exNum-1)
 in
 modify ex`unwrapAnd`((:filter (not.(==ex)) exs)`and`wrap)

modify = \case
 ToDo   (s,eNum,eName) d ->
  chooseAttributeWithDate`unwrapAnd` \case
   "1"->getSubject`unwrapAnd` \newSub  ->ToDo (newSub,eNum,eName) d         &wrap
   "2"->getENum   `unwrapAnd` \newENum ->ToDo (s,newENum,eName) d           &wrap
   "3"->getEName  `unwrapAnd` \newEName->ToDo (s,eNum,IndeedItIs newEName) d&wrap
   "4"->getDate   `unwrapAnd` \newDate ->ToDo (s,eNum,eName) newDate        &wrap
 Done   (s,eNum,eName)   ->
  chooseAttribute        `unwrapAnd` \case
   "1"->getSubject`unwrapAnd` \newSub  ->Done (newSub,eNum,eName)         &wrap
   "2"->getENum   `unwrapAnd` \newENum ->Done (s,newENum,eName)           &wrap
   "3"->getEName  `unwrapAnd` \newEName->Done (s,eNum,IndeedItIs newEName)&wrap
 Missed (s,eNum,eName)   ->
  chooseAttribute        `unwrapAnd` \case
   "1"->getSubject`unwrapAnd` \newSub  ->Missed (newSub,eNum,eName)         &wrap
   "2"->getENum   `unwrapAnd` \newENum ->Missed (s,newENum,eName)           &wrap
   "3"->getEName  `unwrapAnd` \newEName->Missed (s,eNum,IndeedItIs newEName)&wrap

chooseAttributeWithDate = printBasic`andThen`printDate`andThen`getLine
chooseAttribute = printBasic`andThen`getLine

printBasic = printStrings ["1: Subject","2: Exercise Number","3: Exercise Name"]
printDate = printString "4: Date"

getSubject = askFor "New Subject?"
getENum    = askFor "New Exercise Number?"
getEName   = askFor "New Exercise Name?"
