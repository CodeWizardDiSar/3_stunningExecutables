{-# LANGUAGE LambdaCase #-} 
module Useful where
import Prelude hiding (all,and)
import Control.Arrow
import Control.Category
import Data.Function
import Types
import Renaming

printStrsWithNLs = printStrings`and`wrapInEmptyLines
printStrings     = forEachDo printString
wrapInEmptyLines = \p->printEmptyLine`andThen`p`andThen`printEmptyLine
printEmptyLine   = printString ""
doSequentially   = foldl andThen (wrap ()) 
printStrsWithNLs :: Strings->IO ()          
printStrings     :: Strings->IO ()          
wrapInEmptyLines :: IO ()->IO ()          
printEmptyLine   :: IO ()               
doSequentially   :: [IO ()]->IO ()

printEmptyLines  = \case
  0->wrap ()
  i->printEmptyLine`andThen`(i-1&printEmptyLines)
printEmptyLines  :: Int->IO ()          

titleAndOptions  = \x y->[tabBefore x]`append`forEach (2&tabsBefore) y
tabsBefore       = \i s->(repeatNTimes i '\t')`append`s
tabBefore        = 1&tabsBefore
titleAndOptions  :: Message->Messages->Messages
tabsBefore       :: Int->String->String
tabBefore        :: String->String
