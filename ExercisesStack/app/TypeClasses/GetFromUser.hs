module TypeClasses.GetFromUser where

import Prelude 
  ( IO, (>>=), sequence, Monad )
import Types
  ( ToDoExercise( ToDoExercise ), DoneExercise , OtherExercise, ExData, Strings, Date
  , ExerciseType ( ToDoEx, DoneEx, OtherEx ), Exercise( ToDo, Done, Other ) )
import VeryUseful.Renaming
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
  OtherEx -> getFromUser >>= Other .> wrap

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
dateQuestions = [ "Day? (number)", "Month? (number)" ]

printAndGetAnswers :: Strings -> IO Strings 
printAndGetAnswers = forEach printAndGetAnswer .> sequence
