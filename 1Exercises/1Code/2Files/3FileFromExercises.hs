module FileFromExercises where
import Renaming  (forEach,and,glue,convertIntToString)
import Types     (FileVersionOf,Date,toFileString)
import Types     (HopefullySome(..),Exercise(..))
import Types     (HopefullyExerciseName,Line,Exercises)
import Prelude   (String,Int)
import Data.List (intercalate)
import Data.Function ((&))

-- Exercises to string to be saved in the file
exercisesToString = forEach convertToLine`and`glue::Exercises->String
convertToLine = (\case
 Done   (n,nu,e)   ->
  glue ["d ",n," ",nu," ",toFileString e,"\n"]
 Missed (n,nu,e)   ->
  glue ["m ",n," ",nu," ",toFileString e,"\n"]
 ToDo   (n,nu,e) da->
  glue ["t ",n," ",nu," ",toFileString e," ",toFileString da,"\n"]
 )::Exercise->Line
-- toFileString for HopefullyExerciseName,Date,Int
instance FileVersionOf HopefullyExerciseName where
 toFileString = \case
  Nothing     ->"_"
  IndeedItIs e->e 
instance FileVersionOf Date where
 toFileString = forEach toFileString`and`intercalate "/"
instance FileVersionOf Int where
 toFileString = convertIntToString
