module Add where
import Prelude
  (read, (>>=), (>>), IO, Int, String, uncurry, Monad, sequence)
import Renaming
  ((>>>), wrap, forEach)
import Types
  (Exercise(..), HopefullySome(..), ExerciseData, HopefullyExerciseName)
import ExercisesFromFile
  (getExercisesFromFile)
import FileManagement
  (updateVersion)
import UsefulForActions
  (askAndGetAnswer, writeExercisesToFile)
import Data.Function
  ((&))
import Control.Monad.Zip
  (mzipWith, mzip, MonadZip)
import Control.Invertible.Monoidal
  (pairADefault)
import Language.Fixpoint.Misc
  (fM)

-- Add List Of Actions
addActions :: [IO ()]
addActions =
  [ getToDoExerciseFromUser >>= add, getExerciseFromUser Done >>= add
  , getExerciseFromUser Missed >>= add ]

instance MonadZip IO where
  mzip = pairADefault

add :: Exercise -> IO ()
add exerciseFromUser =
  getExercisesFromFile >>= (exerciseFromUser:) >>> writeExercisesToFile >>
  updateVersion

getExerciseFromUser :: (ExerciseData -> Exercise) -> IO Exercise
getExerciseFromUser exerciseType =
  getExerciseDataStrings >>= stringsToExercise exerciseType >>> wrap

stringsToExercise :: (ExerciseData -> Exercise) -> [String] -> Exercise
stringsToExercise exerciseType = stringsToExerciseData >>> exerciseType

getToDoExerciseFromUser :: IO Exercise
getToDoExerciseFromUser = mzipWith ToDo getExerciseData getDate

getExerciseData :: IO ExerciseData
getExerciseData = getExerciseDataStrings >>= stringsToExerciseData >>> wrap

stringsToExerciseData :: [String] -> ExerciseData
stringsToExerciseData = \case
  [subjectName, exerciseNumber, exerciseNameString] ->
    (subjectName, exerciseNumber, exerciseNameString & stringToHopefullyExerciseName)

stringToHopefullyExerciseName :: String -> HopefullyExerciseName
stringToHopefullyExerciseName = \case
  "" ->
    Nothing
  exerciseName ->
    IndeedItIs exerciseName

getExerciseDataStrings :: IO [String]
getExerciseDataStrings =
  askAndGetAnswers [ "Subject Name?", "Exercise Number?", "Exercise Name?" ]

getDate :: IO [Int]
getDate =
  askAndGetAnswers [ "Day Of The Month? (number)" , "Month? (number)" , "Year?" ] >>=
    (read `forEach`) >>> wrap

askAndGetAnswers :: [String] -> IO [String] 
askAndGetAnswers = (askAndGetAnswer `forEach`) >>> sequence

