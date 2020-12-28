module Add where
import Prelude
  ( (>>=), (>>), IO, sequence )
import Types
  ( Exercise( ToDo, Done, Missed ), ExerciseData, Date, Strings, ToDoExercise( ToDoExercise ) )
import FromString
  ( fromStrings, FromUserStrings, fromUserStrings )
import Renaming
  ( (>>>), wrap, forEach )
import ExercisesFromFile
  ( getExercisesFromFile )
import FileManagement
  ( updateVersion )
import UsefulForActions
  ( printAndGetAnswer, writeExercisesToFile )
import Control.Monad.Zip
  ( mzipWith, mzip, MonadZip )
import Control.Invertible.Monoidal
  ( pairADefault )

instance MonadZip IO where
  mzip = pairADefault

addActions :: [ IO () ]
addActions =
  [ getToDoExerciseFromUser >>= ToDo >>> add, getExerciseFromUser Done >>= add
  , getExerciseFromUser Missed >>= add ]

add :: Exercise -> IO ()
add exerciseFromUser =
  getExercisesFromFile >>= ( exerciseFromUser : ) >>> writeExercisesToFile >> updateVersion

getExerciseFromUser :: FromUserStrings a => ( a -> Exercise ) -> IO Exercise
getExerciseFromUser exerciseConstructor =
  dataStringsFromUser >>= fromUserStrings >>> exerciseConstructor >>> wrap

getToDoExerciseFromUser :: IO ToDoExercise
getToDoExerciseFromUser = mzipWith ToDoExercise exerciseDataFromUser dateFromUser

exerciseDataFromUser :: IO ExerciseData
exerciseDataFromUser = dataStringsFromUser >>= fromUserStrings >>> wrap

dataStringsFromUser :: IO Strings
dataStringsFromUser = printAndGetAnswers [ "Subject?", "Number?", "Name?" ]

dateFromUser :: IO Date
dateFromUser = printAndGetAnswers dateQuestions >>= fromStrings >>> wrap

dateQuestions :: Strings
dateQuestions = [ "Day? (number)", "Month? (number)", "Year?" ]

printAndGetAnswers :: Strings -> IO Strings 
printAndGetAnswers = forEach printAndGetAnswer >>> sequence
