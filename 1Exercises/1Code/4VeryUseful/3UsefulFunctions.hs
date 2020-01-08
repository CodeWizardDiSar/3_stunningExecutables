module UsefulFunctions where
import Prelude       (String,Int,IO,(+),(-),foldl)
import Data.Function ((&))
import Types         (Message,Messages,Strings)
import Renaming      (and,andThen,wrap,printString,forEachDo)
import Renaming      (append,repeatNTimes,forEach)
import Renaming      (convertIntFromString,convertIntToString)
import Renaming      (printErrorMessage,glue)

-- Printing
printStrings     = forEachDo printString                ::Strings->IO () 
wrapInNLs = ("\n"`append`)`and`(`append`"\n")           ::String->String
printEmptyLine = printString ""                         ::IO ()
doSequentially = foldl andThen (wrap ())                ::[IO ()]->IO ()
printEmptyLines = (\case
  0->wrap ()
  i->printEmptyLine`andThen`(i-1&printEmptyLines))      ::Int->IO ()
-- Add One To String
addOneToString =
 convertIntFromString`and`(+1)`and`convertIntToString   ::String->String
-- Add One To String
subOneFromString =
 convertIntFromString`and`(+(-1))`and`convertIntToString::String->String
-- Tabbing
tabsBefore = (\i s->
 (repeatNTimes i '\t')`append`s
 )::Int->String->String
tabBefore = 1&tabsBefore                                ::String->String
-- ForEach2
forEach2 = \f as bs -> case (as,bs) of
 (a:as,b:bs) -> f a b : forEach2 f as bs
 ([],[])     -> []
 _           -> printErrorMessage "ForEach2"
forEach2 :: (a->b->c)->[a]->[b]->[c]
