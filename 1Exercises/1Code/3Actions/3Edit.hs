module Edit where
import Add (date,askFor)
import Renaming (glue,printString,wrap,unwrapAnd,andThen,and)
import Renaming (convertIntToString,convertIntFromString)
import ExercisesFromFile (getToDo,getDone,getMissed)
import Prelude ((.),not,filter,(-),(!!),(+),elem,Bool(..),(==))
import Prelude (getLine,IO,Int)
import Data.Function ((&),($))
import Show (printEx)
import FileFromExercises (exercisesToString)
import FileManagement (writeToNextDataKeeper,updateVersion)
import Types (Strings,Exercises,Exercise(..),HopefullySome(..))
import UsefulFunctions (printStrings)
import Delete (join,showSubjects,getSubjects,printSubjects,getSub)
import Delete (getChoice,showExercises,subIs,printExercises)

-- edit list of actions
editList =
 [edit "todo",edit "done",edit "missed"]

edit = \exType ->
 getAllExs exType`unwrapAnd`
 (exercisesToString`and`writeToNextDataKeeper)`andThen`
 updateVersion
  
getAllExs = \case
 "todo"  -> join [editGet getToDo,getDone,getMissed]
 "done"  -> join [getToDo,editGet getDone,getMissed]
 "missed"-> join [getToDo,getDone,editGet getMissed]

editGet = \getExs->
 getExs        `unwrapAnd`\exs   ->((exs)&
 showSubjects) `andThen`
 getChoice     `unwrapAnd`\subNum->((exs,subNum)&
 showExercises)`andThen`
 getChoice     `unwrapAnd`\exNum ->(exs,subNum,exNum)&
 editChosen

-- Edit Chosen
editChosen = \(exs,subNum,exNum)->
 getSubjects exs`unwrapAnd`\subs->
 let
  sub=subs!!(subNum-1)
  subExs=filter (subIs sub) exs
  ex=subExs!!(exNum-1)
 in
 modify ex`unwrapAnd`\newEx->
 wrap $ newEx:(filter (not.(==ex)) exs)

modify = \case
 ToDo   (s,eNum,eName) d ->
  chooseAttributeWithDate`unwrapAnd` \case
   "1"->getNewSubject`unwrapAnd` \newSub  ->wrap $ ToDo (newSub,eNum,eName) d
   "2"->getNewENum   `unwrapAnd` \newENum ->wrap $ ToDo (s,newENum,eName) d
   "3"->getNewEName  `unwrapAnd` \newEName->wrap $ ToDo (s,eNum,IndeedItIs newEName) d
   "4"->getNewDate   `unwrapAnd` \newDate ->wrap $ ToDo (s,eNum,eName) newDate
 Done   (s,eNum,eName)   ->
  chooseAttribute        `unwrapAnd` \case
   "1"->getNewSubject`unwrapAnd` \newSub  ->wrap $ Done (newSub,eNum,eName) 
   "2"->getNewENum   `unwrapAnd` \newENum ->wrap $ Done (s,newENum,eName)
   "3"->getNewEName  `unwrapAnd` \newEName->wrap $ Done (s,eNum,IndeedItIs newEName)
 Missed (s,eNum,eName)   ->
  chooseAttribute        `unwrapAnd` \case
   "1"->getNewSubject`unwrapAnd` \newSub  ->wrap $ Missed (newSub,eNum,eName)
   "2"->getNewENum   `unwrapAnd` \newENum ->wrap $ Missed (s,newENum,eName)
   "3"->getNewEName  `unwrapAnd` \newEName->wrap $ Missed (s,eNum,IndeedItIs newEName)

chooseAttributeWithDate =
 printBasic`andThen`
 printDate`andThen`
 getLine

chooseAttribute =
 printBasic`andThen`
 getLine

printBasic = printStrings ["1: Subject","2: Exercise Number","3: Exercise Name"]
printDate = printString "4: Date"

getNewSubject = askFor "New Subject?"
getNewENum    = askFor "New Exercise Number?"
getNewEName   = askFor "New Exercise Name?"
getNewDate    = date
