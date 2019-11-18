module Insert where
import Types
import GeneralFunctions
  
insertExercise = putStrLn "insertExercise"
insertSubject  = putStrLn "insertSubject"

insert = do
  subject <- askAndGet "subject plz"
  --findOrAddSuject
  numberInString <- askAndGet "exercise number plz"
  let number = read numberInString :: Int
  date <- askAndGet "date plz"
  insertExercise
  --saveExercise $ InfoToExercise 
  --return $ InfoToExercise number subject

--findOrAddSubject = do
--  openDataFile
--
--openDataFile = do
--  answer <- isThereADataFile
--  case answer of 
--    True -> 
--
