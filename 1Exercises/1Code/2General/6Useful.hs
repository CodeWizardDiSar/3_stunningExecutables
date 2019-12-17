{-# LANGUAGE LambdaCase #-} 
module Useful where
import Prelude hiding (all,and)
import Control.Arrow
import Control.Category
import Data.Function
import Types
import Renaming

emptyLine = printString ""
printStrings = mmp printString
emptyLines = \case 0->wrap ();i->emptyLine`andThen`(i-1&emptyLines)
wrapInNewLines = \p->emptyLine>>p>>emptyLine
printStringsWrappedInNewLines = printStrings`and`wrapInNewLines
tabsBefore = \i s->(rnt i '\t')`append`s
tabBefore = 1&tabsBefore
titleAndOptions = \x y->[tabBefore x]`append`forEach (2&tabsBefore) y
askFor = \x->printStringsWrappedInNewLines x>>getLine

emptyLine :: IOU               
printStringsWrappedInNewLines :: STS->IOU          
emptyLines :: INT->IOU          
wrapInNewLines :: IOU->IOU          
printStrings :: STS->IOU          
titleAndOptions :: MSG->MGS->MGS
tabsBefore :: INT->STR->STR
