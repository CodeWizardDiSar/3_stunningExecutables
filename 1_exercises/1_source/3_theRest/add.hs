module Add where
import Types
import General
import FileToSubs
  
insertExercise = putStrLn "insertExercise"
insertSubject  = putStrLn "insertSubject"

insert =
  insertSubject
-- fileSubs >>= \subs ->
-- getSubName >>= \subName ->
-- case elem subName $ map fst subs of
--   True  -> addSub subName >> addEx
--   False -> addSub >> addEx
-- --findOrAddSuject
-- numberInString <- askAndGet "exercise number plz"
-- let number = read numberInString :: Int
-- date <- askAndGet "date plz"
-- insertExercise
-- --saveExercise $ InfoToExercise 
-- --return $ InfoToExercise number subject
--
--etSubName =
-- askAndGet subNameMes
--
--ubNameMes = "Subject Name If You Wish Master"
--
---findOrAddSubject = do
---  openDataFile
---
---openDataFile = do
---  answer <- isThereADataFile
---  case answer of 
---    True -> 
---
