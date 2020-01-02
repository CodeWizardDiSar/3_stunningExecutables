{-# LANGUAGE LambdaCase #-} 
module UsefulFunctions where
import Prelude       (String,Int,IO,(+),(-),foldl)
import Data.Function ((&))
import Types         (Message,Messages,Strings)
import Renaming      (and,andThen,wrap,printString,forEachDo)
import Renaming      (append,repeatNTimes,forEach)
import Renaming      (convertIntFromString,convertIntToString)
import Renaming      (printErrorMessage)
-- Printing
printStrsWithNLs = printStrings`and`wrapInEmptyLines ::Strings->IO ()
printStrings     = forEachDo printString             ::Strings->IO () 
wrapInEmptyLines = (\p->
 printEmptyLine`andThen`p`andThen`printEmptyLine)    ::IO ()->IO ()
printEmptyLine = printString ""                      ::IO ()
doSequentially = foldl andThen (wrap ())             ::[IO ()]->IO ()
printEmptyLines = (\case
  0->wrap ()
  i->printEmptyLine`andThen`(i-1&printEmptyLines))   ::Int->IO ()
-- Add One To String
addOneToString =
 convertIntFromString`and`(+1)`and`convertIntToString::String->String
-- Title And Options
titleAndOptions = (\x y->
 [tabBefore x]`append`forEach(2&tabsBefore)y
 )::Message->Messages->Messages
-- Tabbing
tabsBefore = (\i s->
 (repeatNTimes i '\t')`append`s
 )::Int->String->String
tabBefore = 1&tabsBefore                             ::String->String
-- One to One Application
oneToOneApplication = (\case
  ((f:fs),(a:as)) -> f a:oneToOneApplication (fs,as)
  ([],[])         -> []
  _               -> printErrorMessage "One To One Application"
 )
--oneToOneApplication :: ([a->b],[a])->[b]
