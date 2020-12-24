module Add where
import Renaming
  ((>>>),wrap,forEachIn)
import Types
  (Exercise(..),HopefullySome(..))
import Prelude
  (read,(>>=),(>>))
import ExercisesFromFile
  (getExercises)
import FileManagement
  (updateVersion)
import UsefulForActions
  (askFor,exercisesToFile)
import Data.Function
  ((&))

-- Add List Of Actions
addActions = [ getToDo & add, getDone & add, getMissed & add ]

add = (>>= \ex -> getExercises >>= ( (ex:) >>> exercisesToFile ) >> updateVersion)

getDone   = getSubjectNumberName >>= (Done>>>wrap)

getMissed = getSubjectNumberName >>= (Missed>>>wrap)

getToDo   = getSubjectNumberName >>= \sNN-> getDate >>= (ToDo sNN>>>wrap)

-- Get Subject,Exercise Number and Exercise Name
getSubjectNumberName =
  askFor "Subject Name?"   >>= \sub   -> 
  askFor "Exercise Number?">>= \number->
  askFor "Exercise Name?"  >>= \case
    ""  -> (sub,number,Nothing)        &wrap 
    name-> (sub,number,IndeedItIs name)&wrap 

-- Get Day,Month and Year (as you might have guessed: date)
getDate =
  askFor "Day Of The Month? (number)">>= \day-> 
  askFor "Month? (number)"           >>= \month->
  askFor "Year?"                     >>= \year->
    forEachIn read [day,month,year] & wrap
