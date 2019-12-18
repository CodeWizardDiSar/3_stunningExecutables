{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-} 
{-# LANGUAGE ConstraintKinds #-}
module ExsToFcs where
import Prelude hiding (Nothing,and)
import Types
import General
import Renaming

exercisesToString = forEach convertToLine`and`glue
convertToLine =
  \case Done   (n,nu,e)   ->glue ["d ",n," ",nu," ",toString e,"\n"]
        Missed (n,nu,e)   ->glue ["m ",n," ",nu," ",toString e,"\n"]
        ToDo   (n,nu,e) da->glue ["t ",n," ",nu," ",toString e," ",toString da,"\n"]
exercisesToString :: Exercises->String
convertToLine :: Exercise->Line

instance ToString HopefullyExerciseName where
  toString = \case Nothing ->"_"
                   Indeed e->e 
instance ToString Date where
  toString = \(d,m,y)->glue [toString d,"/",toString m,"/",toString y]
instance ToString Int where
  toString = convertToString
