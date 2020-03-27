module UsefulFunctions where
import Prelude       (IO,(+),(-),sequence_)
import Data.Function ((&))
import Types         (Strings)
import Renaming      (and,andThen,wrap,printString,forEachDo,
                      append,repeatNTimes,printErrorMessage,
                      glue,convertIntFromString,
                      convertIntToString)

-- Avoind multiple `andThen`s (Renaming for more)
doSequentially = sequence_::[IO ()]->IO ()

--Tabbing
tabBefore = 1&tabsBefore
tabsBefore = \i s-> (repeatNTimes i '\t')`append`s

-- Printing
printStrings    = forEachDo printString
printEmptyLine  = printString ""
printEmptyLines = \case 0-> wrap ()
                        i-> printEmptyLine`andThen`
                            (i-1&printEmptyLines)

-- Add/Subtract One To/From String
addNToString     = \n-> convertIntFromString`and`(+n)`and`
                        convertIntToString
addOneToString   = addNToString 1
subOneFromString = addNToString (-1)

-- For Each with 2 arguments
forEach2 f (a:as) (b:bs) = f a b : forEach2 f as bs
forEach2 _ []     []     = []
forEach2 _ _      _      = printErrorMessage "ForEach2"
