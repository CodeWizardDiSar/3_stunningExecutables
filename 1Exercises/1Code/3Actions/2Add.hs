module Add where
import Renaming          (printString,andThen,append,unwrapAnd,and,keepAnd)
import Renaming          (inputTo,wrap,glue,forEach)
import Types             (Exercise(..),HopefullySome(..))
import Prelude           (Bool(..),concat,length,(>),getLine,IO,($),read)
import ExercisesFromFile (exercises)
import FileFromExercises (exercisesToString)
import FileManagement    (writeToNextDataKeeper,updateVersion)

-- Add List Of Actions
addList = [addTo toDo,addTo done,addTo missed]
-- Add To
addTo = (`unwrapAnd`\ex->exercises`unwrapAnd`(
 (ex:)`and`exercisesToString`and`writeToNextDataKeeper)`andThen`
 updateVersion)
-- To Do, Done, Missed
toDo   = subNumName`unwrapAnd`\sNN->date`unwrapAnd`(ToDo sNN`and`wrap)
done   = subNumName`unwrapAnd`(Done  `and`wrap)
missed = subNumName`unwrapAnd`(Missed`and`wrap)
-- Get Subject,Exercise Number and Exercise Name
subNumName = askFor "Subject Name?"   `unwrapAnd`\sub   -> 
             askFor "Exercise Number?"`unwrapAnd`\number->
             askFor "Exercise Name?"  `unwrapAnd`\name  -> 
             case name of 
              "" -> wrap (sub,number,Nothing)
              _  -> wrap (sub,number,IndeedItIs name)
askFor = \s -> printString s`getLineUnwrapAnd`\a->
 case length a>19 of
  True->printString "More than 19 chars is not pretty"`andThen`askFor s
  _   ->wrap a
getLineUnwrapAnd = \a->(a`andThen`getLine`unwrapAnd`)
-- Get Day,Month and Year (as you might have guessed: date)
date =
 printString "Day Of The Month? (number)"`getLineUnwrapAnd`\day-> 
 printString "Month? (number)"           `getLineUnwrapAnd`\month->
 printString "Year?"                     `getLineUnwrapAnd`\year->
 wrap $ forEach read [day,month,year]
