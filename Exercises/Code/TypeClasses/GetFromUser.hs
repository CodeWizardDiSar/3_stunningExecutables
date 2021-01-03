module GetFromUser where

import Prelude 
  ( IO, (>>=), sequence, Monad )
import Types
  ( ToDoExercise( ToDoExercise ), DoneExercise , MissedExercise, ExData, Strings, Date
  , ExerciseType ( ToDoEx, DoneEx, MissedEx ), Exercise( ToDo, Done, Missed ) )
import Renaming
  ( (.>), wrap, forEach )
import Control.Monad.Zip
  ( mzipWith, mzip, MonadZip )
import Control.Invertible.Monoidal
  ( pairADefault )
import FromString
  ( fromStrings, fromUserStrings )
import UsefulForActions
  ( printAndGetAnswer )

instance MonadZip IO where
  mzip = pairADefault

myMZipWith :: MonadZip m => ( a -> b -> c ) ->  m a -> m b -> m c
myMZipWith = mzipWith

getExerciseFromUser :: ExerciseType -> IO Exercise
getExerciseFromUser = \case
  ToDoEx -> getFromUser >>= ToDo .> wrap
  DoneEx -> getFromUser >>= Done .> wrap
  MissedEx -> getFromUser >>= Missed .> wrap

class GetFromUser a where getFromUser :: IO a

instance GetFromUser ToDoExercise where 
  getFromUser = mzipWith ToDoExercise getFromUser getFromUser

instance GetFromUser ExData where 
  getFromUser = getFromUser >>= fromUserStrings .> wrap

instance GetFromUser Strings where 
  getFromUser = printAndGetAnswers [ "Subject?", "Number?", "Name?" ]

instance GetFromUser Date where 
  getFromUser = printAndGetAnswers dateQuestions >>= fromStrings .> wrap

dateQuestions :: Strings
dateQuestions = [ "Day? (number)", "Month? (number)", "Year?" ]

printAndGetAnswers :: Strings -> IO Strings 
printAndGetAnswers = forEach printAndGetAnswer .> sequence
