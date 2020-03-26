module StringFromExercises where
import Renaming      (forEach,and,glue,convertIntToString,
                      append)
import Types         (FileVersionOf,Date,toFileString,
                      HopefullySome(..),Exercise(..),
                      HopefullyExerciseName,Line,Exercises)
import Prelude       (String,Int)
import Data.List     (intercalate)
import Data.Function ((&))

-- Exercises to string to be saved in the file
exercisesToString =
 forEach (toFileString`and`(`append`"\n"))`and`
 glue::Exercises->String

-- toFileString for Exercise,HopefullyExerciseName,Date,Int
instance FileVersionOf Exercise where
 toFileString = \case
  Done   (n,nu,e)   -> intercalate "," ["d",n,nu,
                                        toFileString e]
  Missed (n,nu,e)   -> intercalate "," ["m",n,nu,
                                        toFileString e]
  ToDo   (n,nu,e) da-> intercalate "," ["t",n,nu,
                                        toFileString e,
                                        toFileString da]
instance FileVersionOf HopefullyExerciseName where
 toFileString = \case Nothing->"_";IndeedItIs e->e 
instance FileVersionOf Date where
 toFileString = forEach toFileString`and`intercalate "/"
instance FileVersionOf Int where
 toFileString = convertIntToString
