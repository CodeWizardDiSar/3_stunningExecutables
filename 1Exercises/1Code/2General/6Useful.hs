{-# LANGUAGE LambdaCase #-} 
module Useful where
import Control.Arrow
import Data.Function
import Types
import Renaming

newLine = printString ""                               -- New LIne
printStrings = mmp printString                              -- Print StringS
newLines = \case 0->wim ();i->newLine>>((i-1)&newLines) -- New LineS
wrapInNewLines = \p->newLine>>p>>newLine
printStringsWrappedInNewLines = printStrings>>>wrapInNewLines
tabsBefore = \i s->(rnt i '\t')++s
tabBefore = 1&tabsBefore
titleAndOptions = \x y->[tabBefore x]++map (2&tabsBefore) y
printAndGet = \x->printStringsWrappedInNewLines x>>getLine

newLine :: IOU               
printStringsWrappedInNewLines :: STS->IOU          
newLines :: INT->IOU          
wrapInNewLines :: IOU->IOU          
printStrings :: STS->IOU          
titleAndOptions :: MSG->MGS->MGS
tabsBefore :: INT->STR->STR
