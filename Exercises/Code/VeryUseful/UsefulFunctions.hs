module UsefulFunctions where
import Prelude 
  (IO, (+), (-), sequence_)
import Data.Function
  ((&))
import Types
  (Strings)
import Renaming
  ((>>>), andThen, wrap, printString, forEachDo, append, repeatNTimes, printErrorMessage
  ,glue, convertIntFromString, convertIntToString)

doSequentially = sequence_ :: [IO ()] -> IO ()

tabBefore = 1 & tabsBefore
tabsBefore = \i s-> (repeatNTimes i '\t') `append` s

printStrings    = forEachDo printString
printEmptyLine  = printString ""
printEmptyLines = \case 0-> wrap ()
                        i-> printEmptyLine `andThen` 
                            (i-1&printEmptyLines)

addNToString     = \n-> convertIntFromString >>> (+n) >>> convertIntToString
addOneToString   = addNToString 1
subOneFromString = addNToString (-1)
