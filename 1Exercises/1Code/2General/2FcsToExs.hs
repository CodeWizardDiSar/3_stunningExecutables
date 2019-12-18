{-# LANGUAGE LambdaCase,FlexibleInstances #-} 
module FcsToExs where
import Prelude hiding (Nothing,and)
import Data.List.Split
import Control.Arrow
import Types 
import General
import Renaming

exercises = readCurrentDataKeeper`unwrapAnd`(convertToExercises`and`wrap)
readCurrentDataKeeper = currentDataKeeper`unwrapAnd`readFile
convertToExercises    = splitInLines`and`forEach convertToExercise
convertToExercise     = splitInWords`and`
  \case ["d",sn,nu,en]   ->Done   (sn,nu,fromString en)
        ["m",sn,nu,en]   ->Missed (sn,nu,fromString en)
        ["t",sn,nu,en,da]->ToDo   (sn,nu,fromString en)$fromString da
        _                ->printErrorMessage "Line To Exercise"
convertToExercises :: String->Exercises
convertToExercise  :: Line->Exercise

instance FromString HopefullyExerciseName where
  fromString= \case "_"   ->Nothing
                    exName->Indeed exName
instance FromString Date where
  fromString=splitOn "/"`and`
    \case [d,m,y]->(fromString d,fromString m,fromString y)
          _      ->printErrorMessage "Date"
instance FromString Int where
  fromString=convertFromString
