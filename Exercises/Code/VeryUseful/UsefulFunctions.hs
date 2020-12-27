module UsefulFunctions where
import Prelude 
  ( IO, (+), (-), sequence_, (++), (>>), String, Int )
import Data.Function
  ( (&) )
import Types
  ( Strings )
import Renaming
  ( (>>>), andThen, wrap, printString, forEachDo, repeatNTimes, printErrorMessage , glue
  , convertIntFromString, convertIntToString )

doSequentially :: [ IO () ] -> IO ()
doSequentially = sequence_

tabBefore :: String -> String
tabBefore = 1 & tabsBefore

tabsBefore :: Int -> String -> String
tabsBefore = \i s-> ( repeatNTimes i '\t' ) ++ s

printStrings :: Strings -> IO ()
printStrings = forEachDo printString

printEmptyLine :: IO ()
printEmptyLine = printString ""

printEmptyLines :: Int -> IO ()
printEmptyLines = \case
  0 -> wrap ()
  i -> printEmptyLine >> ( i - 1 & printEmptyLines )

addNToString :: Int -> String -> String
addNToString = \n -> convertIntFromString >>> ( + n ) >>> convertIntToString

addOneToString :: String -> String
addOneToString = addNToString 1

subOneFromString :: String -> String
subOneFromString = addNToString (-1)
