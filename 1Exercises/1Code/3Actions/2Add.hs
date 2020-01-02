{-# LANGUAGE LambdaCase #-} 
module Add where
import Prelude       (Bool(..),concat,length,(>),getLine)
import Renaming      (printString,andThen,append,unwrapAnd,and,keepAnd)
import Renaming      (inputTo)

addList       = [addToToDo,addToDone,addToMissed]
addToToDo     = askForSubName []
askForSubName = askFor"Subject Name?"`keepAnd`askForExNum
askForExNum   = askFor"Exercise Number?"`keepAnd`askForExName
askForExName  = askFor"Exercise Name?"`keepAnd`askForDate
askForDate    = askFor"Date?"`keepAnd`(concat`and`printString)
askFor question nextAction = \l->
 printString question`andThen`getLine`unwrapAnd`\x->
 (length x>14)`inputTo`\case
  True->whine`andThen`askFor question nextAction l
  _   ->(l`append`[x])`inputTo`nextAction
whine = printString "More than 14 chars is not pretty"
addToDone   = printString "Add To Done"
addToMissed = printString "Add To Missed"
