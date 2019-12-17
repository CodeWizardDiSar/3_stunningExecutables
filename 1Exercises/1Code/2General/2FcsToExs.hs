{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-} 
module FcsToExs where
import Prelude hiding (Nothing,fromString,splitInLines,and)
import Data.List.Split
import Control.Arrow
import Types 
import General
import Renaming

exercises = readCurrentDataKeeper`unwrapAnd`(convertToExercises`and`wrap)
readCurrentDataKeeper = currentDataKeeper`unwrapAnd`readFile
convertToExercises    = splitInLines`and`forEach convertToExercise
convertToExercise     = splitInWords`and`
  \case ["d",sn,nu,en]   ->Don (sn,nu,fromString en)
        ["m",sn,nu,en]   ->Mis (sn,nu,fromString en)
        ["t",sn,nu,en,da]->Tdo (sn,nu,fromString en)$fromString da
        _                ->printErrorMessage "Line To Exercise"
convertToExercises :: FCS->EXS
convertToExercise  :: LIN->EXR

--from string
instance FST HEN where fromString= \case "_"->Nothing;exName->Indeed exName
instance FST DAT where
  fromString=splitOn "/"`and`
    \case [d,m,y]->(fromString d,fromString m,fromString y)
          _      ->printErrorMessage "Date"
instance FST INT where fromString=convertFromString
