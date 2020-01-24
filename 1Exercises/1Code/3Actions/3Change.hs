module Change where
import Renaming (printString,append,forEach,unwrapAnd,forEachDo)
import Renaming (andThen,unwrapAnd,and,printErrorMessage)
import Types (show,Exercise(..),HopefullySome(..))
import UsefulFunctions (printStrings)
import ExercisesFromFile (exercises)
import FileFromExercises (exercisesToString)
import FileManagement (writeToNextDataKeeper,updateVersion)
import Data.Function (($),(&))
import Prelude (getLine,length,read,Bool(..),take,drop,filter)
import Prelude ((-),(<),(>),(+),(!!),(&&),return)
import Show 

-- Change List of Actions
changeList = [filterAndChange toDo,
  filterAndChange done,
  filterAndChange missed]
filterAndChange = \exerciseType->
 exercises`unwrapAnd`(filter exerciseType`and`change)
change = \exs-> 
 (forEachDo (show`and`printString) exs)`andThen`(askFor "Exercise Number" exs)
 
askFor = \s exs-> printString s`getLineUnwrapAnd`\a->
 case read a < length exs && read a > 0 of
  True->changeEx (read a-1) exs
  _   ->printString "Choose an existing exercise"
getLineUnwrapAnd = \a->(a`andThen`getLine`unwrapAnd`)

changeEx = \i exs -> (exs!!i)& \case
 ToDo _ _ ->
  printStrings
   ["1: Change Subect",
    "2: Change Number",
    "3: Change Name"  ,
    "4: Change Date"  ]`getLineUnwrapAnd`\case
     "1" -> (changeSubject i exs)`unwrapAnd`(exercisesToString`and`writeToNextDataKeeper)`andThen`updateVersion
     "2" -> (changeNumber  i exs)`unwrapAnd`(exercisesToString`and`writeToNextDataKeeper)`andThen`updateVersion
     "3" -> (changeName    i exs)`unwrapAnd`(exercisesToString`and`writeToNextDataKeeper)`andThen`updateVersion
     "4" -> (changeDate    i exs)`unwrapAnd`(exercisesToString`and`writeToNextDataKeeper)`andThen`updateVersion
     _   -> printString "Hmm not possible"
 _        ->
  printStrings
   ["1: Change Subect",
    "2: Change Number",
    "3: Change Name"  ]`getLineUnwrapAnd`\case
     "1" -> (changeSubject i exs)`unwrapAnd`(exercisesToString`and`writeToNextDataKeeper)`andThen`updateVersion 
     "2" -> (changeNumber  i exs)`unwrapAnd`(exercisesToString`and`writeToNextDataKeeper)`andThen`updateVersion
     "3" -> (changeName    i exs)`unwrapAnd`(exercisesToString`and`writeToNextDataKeeper)`andThen`updateVersion
     _   -> printString "Hmm not possible"

changeSubject = \i exs ->
 printString "New Subject?"`getLineUnwrapAnd`\s ->
 exs!!i& \case 
  ToDo   exData date ->
   return$take i exs`append`newSubToDoEx   exData date s`append`drop (i+1) exs
  Done   exData      ->
   return$take i exs`append`newSubDoneEx   exData      s`append`drop (i+1) exs
  Missed exData      ->
   return$take i exs`append`newSubMissedEx exData      s`append`drop (i+1) exs

newSubToDoEx   = \(_,num,name) d s-> [ToDo   (s,num,name) d]
newSubDoneEx   = \(_,num,name) s  -> [Done   (s,num,name)  ]
newSubMissedEx = \(_,num,name) s  -> [Missed (s,num,name)  ]

changeNumber = \i exs ->
 printString "New Number?"`getLineUnwrapAnd`\n ->
 exs!!i& \case 
  ToDo   exData date ->
   return$take i exs`append`newNumToDoEx   exData date n`append`drop (i+1) exs
  Done   exData      -> 
   return$take i exs`append`newNumDoneEx   exData      n`append`drop (i+1) exs
  Missed exData      ->
   return$take i exs`append`newNumMissedEx exData      n`append`drop (i+1) exs

newNumToDoEx   = \(sub,_,name) d n-> [ToDo   (sub,n,name) d]
newNumDoneEx   = \(sub,_,name) n  -> [Done   (sub,n,name)  ]
newNumMissedEx = \(sub,_,name) n  -> [Missed (sub,n,name)  ]

changeName = \i exs ->
 printString "New Name?"`getLineUnwrapAnd`\na ->
 exs!!i& \case 
  ToDo   exData date ->
   return$take i exs`append`newNameToDoEx   exData date na`append`drop (i+1) exs
  Done   exData      ->
   return$take i exs`append`newNameDoneEx   exData      na`append`drop (i+1) exs
  Missed exData      -> 
   return$take i exs`append`newNameMissedEx exData      na`append`drop (i+1) exs

newNameToDoEx   = \(sub,num,_) d name-> [ToDo   (sub,num,IndeedItIs name) d]
newNameDoneEx   = \(sub,num,_) name  -> [Done   (sub,num,IndeedItIs name)  ]
newNameMissedEx = \(sub,num,_) name  -> [Missed (sub,num,IndeedItIs name)  ]

changeDate = \i exs ->
 printString "New Day (Number)?"  `getLineUnwrapAnd`\d ->
 printString "New Month (Number)?"`getLineUnwrapAnd`\m ->
 printString "New Year?"          `getLineUnwrapAnd`\y ->
 exs!!i& \case 
  ToDo exData _ ->
   return$take i exs`append`[ToDo exData [read d,read m,read y]]`append`drop (i+1) exs
  _             ->
   printErrorMessage "Not Possible To Change Date in not To Do Exercise"
