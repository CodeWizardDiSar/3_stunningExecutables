module FileFromExercises where
import Renaming  (forEach,and,glue,convertIntToString,append)
import Types     (FileVersionOf,Date,toFileString)
import Types     (HopefullySome(..),Exercise(..))
import Types     (HopefullyExerciseName,Line,Exercises)
import Prelude   (String,Int)
import Data.List (intercalate)
import Data.Function ((&))

-- Exercises to string to be saved in the file
exercisesToString = forEach (convertToLine`and`(`append`"\n"))`and`glue::Exercises->String
convertToLine = (\case
 Done   (n,nu,e)   ->
  intercalate "," ["d",n,nu,toFileString e]
 Missed (n,nu,e)   ->
  intercalate "," ["m",n,nu,toFileString e]
 ToDo   (n,nu,e) da->
  intercalate "," ["t",n,nu,toFileString e,toFileString da]
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
