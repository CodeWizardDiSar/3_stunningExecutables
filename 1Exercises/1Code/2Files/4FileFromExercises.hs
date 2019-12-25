{-# LANGUAGE LambdaCase,FlexibleInstances #-} 
module FileFromExercises where
import Prelude hiding (Nothing,and)
import Types
import FileManagement
import Renaming

exercisesStringVersionOf = forEach convertToLine`and`glue
convertToLine =
  \case Done   (n,nu,e)   ->glue ["d ",n," ",nu," ",toString e,"\n"]
        Missed (n,nu,e)   ->glue ["m ",n," ",nu," ",toString e,"\n"]
        ToDo   (n,nu,e) da->glue ["t ",n," ",nu," ",toString e," ",toString da,"\n"]
exercisesStringVersionOf :: Exercises->String
convertToLine :: Exercise->Line

instance StringVersionOf HopefullyExerciseName where
  toString = \case Nothing     ->"_"
                   IndeedItIs e->e 
instance StringVersionOf Date where
  toString = \(d,m,y)->glue [toString d,"/",toString m,"/",toString y]
instance StringVersionOf Int where
  toString = convertToString
