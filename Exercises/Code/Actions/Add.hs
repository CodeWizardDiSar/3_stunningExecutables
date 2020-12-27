module Add where
import Prelude
  ( read, (>>=), (>>), IO, Int, String, uncurry, Monad, sequence )
import Types
  ( Exercise( ToDo, Done, Missed ), HopefullySome( IndeedItIs, Nothing ), HopefullyExerciseName
  , ExerciseData( ED ), Date ( D ), Strings )
import TypeClasses
  ( FromStrings, fromStrings, fromString )
import Renaming
  ( (>>>), wrap, forEach, printErrorMessage )
import ExercisesFromFile
  ( getExercisesFromFile )
import FileManagement
  ( updateVersion )
import UsefulForActions
  ( printAndGetAnswer, writeExercisesToFile )
import Data.Function
  ( (&) )
import Control.Monad.Zip
  ( mzipWith, mzip, MonadZip )
import Control.Invertible.Monoidal
  ( pairADefault )

instance MonadZip IO where
  mzip = pairADefault

addActions :: [ IO () ]
addActions =
  [ getToDoExerciseFromUser >>= add, getExerciseFromUser Done >>= add
  , getExerciseFromUser Missed >>= add ]

add :: Exercise -> IO ()
add exerciseFromUser =
  getExercisesFromFile >>= ( exerciseFromUser : ) >>> writeExercisesToFile >> updateVersion

getExerciseFromUser :: ( ExerciseData -> Exercise ) -> IO Exercise
getExerciseFromUser exerciseType =
  getExerciseDataStringsFromUser >>= stringsToExercise exerciseType >>> wrap

stringsToExercise :: ( ExerciseData -> Exercise ) -> [ String ] -> Exercise
stringsToExercise exerciseType = fromStrings >>> exerciseType

getToDoExerciseFromUser :: IO Exercise
getToDoExerciseFromUser = mzipWith ToDo getExerciseData getDate

getExerciseData :: IO ExerciseData
getExerciseData = getExerciseDataStringsFromUser >>= fromStrings >>> wrap

stringToHopefullyExerciseName :: String -> HopefullyExerciseName
stringToHopefullyExerciseName = \case
  "" -> Nothing
  exerciseName -> IndeedItIs exerciseName

getExerciseDataStringsFromUser :: IO Strings
getExerciseDataStringsFromUser = printAndGetAnswers [ "Subject?", "Number?", "Name?" ]

getDate :: IO Date
getDate = printAndGetAnswers dateQuestions >>= fromStrings >>> wrap

dateQuestions :: Strings
dateQuestions = [ "Day Of The Month? (number)" , "Month? (number)" , "Year?" ]

printAndGetAnswers :: Strings -> IO Strings 
printAndGetAnswers = forEach printAndGetAnswer >>> sequence

instance FromStrings ExerciseData where
  fromStrings = \case
    [ subjectName, exerciseNumber, exerciseNameString ] ->
      ED subjectName exerciseNumber ( exerciseNameString & stringToHopefullyExerciseName )
    _ -> printErrorMessage "Programmer messed up in collecting exercise info from user"

instance FromStrings Date where
  fromStrings = \case 
    [ d, m, y ] -> D ( fromString d ) ( fromString m ) ( fromString y )
    _ -> printErrorMessage "Programmer messed up in collecting date info from user"
