module RemoveExercise where
 
import Data.List
import Data.List.Split
 
from x = x
element n = (!!(n-1))
extractExercisesFrom = map (splitOn ",") . lines
exercisesCsv = "/home/gnostis/Dropbox/2_Hobbies/1_Prog/1_Projects/1_UniRelated/1_Exs/exercises.csv"
commaSplit = intercalate ","

removeExercise = do
  putStrLn "Εισάγετε Μάθημα Άσκηκης" 
  subject <- getLine
  putStrLn "Εισάγετε Τίτλο Άσκηκης" 
  title <- getLine
  fileAsString <- readFile exercisesCsv
  let exercises = length fileAsString `seq` extractExercisesFrom fileAsString 
  let subjectMatches e = (subject ==) $ element 1 $ from e
  let titleMatches e = (title ==) $ element 2 $ from e
  let toBeRemoved e = subjectMatches e && titleMatches e
  let newExercises = filter ( not.toBeRemoved ) $ from exercises
  newExercises `seq` writeFile exercisesCsv $ unlines $ map commaSplit newExercises
