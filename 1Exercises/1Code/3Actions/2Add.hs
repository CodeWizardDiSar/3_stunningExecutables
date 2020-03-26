module Add where
import Renaming            (printString,andThen,append,
                            unwrapAnd,and,keepAnd,
                            inputTo,wrap,glue,forEach)
import Types               (Exercise(..),HopefullySome(..))
import Prelude             (Bool(..),concat,length,(>),getLine,
                            IO,($),read)
import ExercisesFromFile   (getExercises)
import StringFromExercises (exercisesToString)
import FileManagement      (writeToNextDataKeeper,
                            updateVersion)
import UsefulForActions    (askFor,exercisesToFile)
import Control.Monad       ((>=>))
import Data.Function       ((&))

-- Add List Of Actions
addList = [getToDo&add,getDone&add,getMissed&add]
add = (`unwrapAnd`\ex->
 getExercises`unwrapAnd`((ex:)`and`exercisesToFile)`andThen`
 updateVersion)
getDone   = getSubjectNumberName`unwrapAnd`(Done    `and`wrap)
getMissed = getSubjectNumberName`unwrapAnd`(Missed  `and`wrap)
getToDo   = getSubjectNumberName`unwrapAnd`
                   \sNN->getDate`unwrapAnd`(ToDo sNN`and`wrap)

-- Get Subject,Exercise Number and Exercise Name
getSubjectNumberName =
 askFor "Subject Name?"   `unwrapAnd`\sub   -> 
 askFor "Exercise Number?"`unwrapAnd`\number->
 askFor "Exercise Name?"  `unwrapAnd`\case
  ""  -> wrap (sub,number,Nothing)
  name-> wrap (sub,number,IndeedItIs name)

-- Get Day,Month and Year (as you might have guessed: date)
getDate =
 askFor "Day Of The Month? (number)"`unwrapAnd`\day-> 
 askFor "Month? (number)"           `unwrapAnd`\month->
 askFor "Year?"                     `unwrapAnd`\year->
 wrap $ forEach read [day,month,year]
