module UsefulFunctions where
import Prelude       (String,Int,IO,(+),(-),foldl)
import Data.Function ((&))
import Types         (Message,Messages,Strings)
import Renaming      (and,andThen,wrap,printString,forEachDo)
import Renaming      (append,repeatNTimes,forEach,printErrorMessage,glue)
import Renaming      (convertIntFromString,convertIntToString)

tabBefore = 1&tabsBefore
tabsBefore = \i s-> (repeatNTimes i '\t')`append`s
doSequentially = foldl andThen (wrap ())::[IO ()]->IO ()
forEach2 :: (a->b->c)->[a]->[b]->[c]
forEach2 = \f as bs->case (as,bs) of
 ([],[])     -> []
 (a:as,b:bs) -> f a b : forEach2 f as bs
 _           -> printErrorMessage "ForEach2"

-- Printing
printStrings = forEachDo printString::Strings->IO ()
printEmptyLine = printString ""::IO ()
printEmptyLines = \case
 0-> wrap ()
 i-> printEmptyLine`andThen`(i-1&printEmptyLines)

-- Add/Subtract One To/From String
addNToString = \n->
 convertIntFromString`and`(+n)`and`
 convertIntToString
addOneToString = addNToString 1
subOneFromString = addNToString (-1)
