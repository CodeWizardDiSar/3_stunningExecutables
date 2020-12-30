module GetFromUser where
import Prelude 
  ( IO, (>>=), sequence )
import Types
  ( ToDoExercise( ToDoExercise ), DoneExercise( DoneExercise )
  , MissedExercise( MissedExercise ), ExerciseData, Strings, Date )
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

class GetFromUser a where getFromUser :: IO a

instance GetFromUser ToDoExercise where 
  getFromUser = mzipWith ToDoExercise getFromUser getFromUser

instance GetFromUser DoneExercise where 
  getFromUser = getFromUser >>= DoneExercise .> wrap  

instance GetFromUser MissedExercise where 
  getFromUser = getFromUser >>= MissedExercise .> wrap  

instance GetFromUser ExerciseData where 
  getFromUser = getFromUser >>= fromUserStrings .> wrap

instance GetFromUser Strings where 
  getFromUser = printAndGetAnswers [ "Subject?", "Number?", "Name?" ]

instance GetFromUser Date where 
  getFromUser = printAndGetAnswers dateQuestions >>= fromStrings .> wrap

dateQuestions :: Strings
dateQuestions = [ "Day? (number)", "Month? (number)", "Year?" ]

printAndGetAnswers :: Strings -> IO Strings 
printAndGetAnswers = forEach printAndGetAnswer .> sequence
