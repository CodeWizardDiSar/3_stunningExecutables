{-# LANGUAGE LambdaCase #-} 
module Add where
import Prelude hiding (Nothing,and)
import Data.Function ((&))
import Renaming (printString,andThen,append,unwrapAnd,and,keepAnd)

addToList     = [addToToDo,addToDone,addToMissed]
addToToDo     = askForSubName []
askForSubName = askFor"Subject Name?"`keepAnd`askForExNum
askForExNum   = askFor"Exercise Number?"`keepAnd`askForExName
askForExName  = askFor"Exercise Name?"`keepAnd`askForDate
askForDate    = askFor"Date?"`keepAnd`(concat`and`printString)
askFor question nextAction = \l->
  ask question`unwrapAnd`\x->length x>14& \case
    True->whine`andThen`askFor question nextAction l
    _   ->l`append`[x]&nextAction
whine = printString "More than 14 chars is not pretty"

--let newExs = evalState (add (suna,exnu,exna,date) exercises)

--newExs :: State Exercises Exercises
--newExs = do
--  exs <- get

ask = \s -> printString s`andThen`getLine
addToDone   = printString "Add To Done"
addToMissed = printString "Add To Missed"
