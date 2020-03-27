module Add where
import Renaming            (andThen,unwrapAnd,and,wrap,forEach)
import Types               (Exercise(..),HopefullySome(..))
import Prelude             (read)
import ExercisesFromFile   (getExercises)
import FileManagement      (updateVersion)
import UsefulForActions    (askFor,exercisesToFile)
import Data.Function       ((&))

-- Add List Of Actions
addList = [getToDo&add,getDone&add,getMissed&add]
add = (`unwrapAnd`\ex->getExercises`unwrapAnd`((ex:)`and`
                       exercisesToFile)`andThen`updateVersion)
getDone   = getSubjectNumberName`unwrapAnd`(Done    `and`wrap)
getMissed = getSubjectNumberName`unwrapAnd`(Missed  `and`wrap)
getToDo   = getSubjectNumberName`unwrapAnd`
                   \sNN->getDate`unwrapAnd`(ToDo sNN`and`wrap)

-- Get Subject,Exercise Number and Exercise Name
getSubjectNumberName =
 askFor "Subject Name?"   `unwrapAnd`\sub   -> 
 askFor "Exercise Number?"`unwrapAnd`\number->
 askFor "Exercise Name?"  `unwrapAnd`\case
  ""  -> (sub,number,Nothing)        &wrap 
  name-> (sub,number,IndeedItIs name)&wrap 

-- Get Day,Month and Year (as you might have guessed: date)
getDate =
 askFor "Day Of The Month? (number)"`unwrapAnd`\day-> 
 askFor "Month? (number)"           `unwrapAnd`\month->
 askFor "Year?"                     `unwrapAnd`\year->
  forEach read [day,month,year]&wrap
