module StringFromExercises where
import Renaming 
  (forEach, (>>>), glue, convertIntToString, append)
import Types 
  (FileVersionOf, Date, toFileString, HopefullySome(..), Exercise(..), HopefullyExerciseName
  ,Exercises)
import Prelude
  (String, Int, (++))
import Data.List
  (intercalate)

exercisesToString :: Exercises -> String
exercisesToString = ( ( toFileString >>> ( ++ "\n" ) ) `forEach` ) >>> glue

instance FileVersionOf Exercise where
 toFileString = \case
  Done (n,nu,e) -> intercalate "," ["d", n, nu, toFileString e]
  Missed (n,nu,e) -> intercalate "," ["m", n, nu, toFileString e]
  ToDo (n,nu,e) da -> intercalate "," ["t", n, nu, toFileString e, toFileString da]

instance FileVersionOf HopefullyExerciseName where
 toFileString = \case Nothing->"_";IndeedItIs e->e 

instance FileVersionOf Date where
 toFileString = (toFileString`forEach`) >>>intercalate "/"

instance FileVersionOf Int where
 toFileString = convertIntToString
