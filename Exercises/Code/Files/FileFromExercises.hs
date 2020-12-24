module StringFromExercises where
import Renaming 
  (forEachIn, (>>>), glue, convertIntToString, append)
import Types 
  (FileVersionOf, Date, toFileString, HopefullySome(..), Exercise(..), HopefullyExerciseName
  ,Exercises)
import Prelude
  (String, Int, (++))
import Data.List
  (intercalate)

-- Exercises to string to be saved in the file
exercisesToString :: Exercises -> String
exercisesToString = ( ( toFileString >>> ( ++ "\n" ) ) `forEachIn` ) >>> glue

-- toFileString for Exercise,HopefullyExerciseName,Date,Int
instance FileVersionOf Exercise where
 toFileString = \case
  Done   (n,nu,e) ->
    intercalate "," ["d", n, nu, toFileString e]
  Missed (n,nu,e) ->
    intercalate "," ["m", n, nu, toFileString e]
  ToDo   (n,nu,e) da ->
    intercalate "," ["t", n, nu, toFileString e, toFileString da]
instance FileVersionOf HopefullyExerciseName where
 toFileString = \case Nothing->"_";IndeedItIs e->e 
instance FileVersionOf Date where
 toFileString = (toFileString`forEachIn`) >>>intercalate "/"
instance FileVersionOf Int where
 toFileString = convertIntToString
