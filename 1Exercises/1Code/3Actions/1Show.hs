{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-} 
module Show where
import Prelude hiding (Show,show,all,and,Nothing)
import Control.Arrow
import Data.Function
import Renaming
import Useful
import Types
import FcsToExs 
import RootMenu
import Actions

s :: Show a => a->String
from = ($)
fill               = \i s->take i`from`(s&withInfiniteSpaces)
withInfiniteSpaces = \s->s`append`repeat ' '
fill15ForEach      = forEach (fill 15)
s = show
instance Show Exercise where
  show =
    \case Done (n,nu,e)   ->[n,nu,s e]     &(fill15ForEach`and`glue)
          Missed (n,nu,e)   ->[n,nu,s e]     &(fill15ForEach`and`glue)
          ToDo (n,nu,e) da->[n,nu,s e,s da]&(fill15ForEach`and`glue)

appendNewLine = (`append`"\n")
instance Show Exercises where
  show = forEach (show`and`tabBefore`and`appendNewLine)`and`glue 
instance Show HopefullyExerciseName where show = \case Nothing->"No Name";Indeed e->e 
instance Show Date where show = \(d,m,y)->glue [show d,"/",show m,"/",show y]
instance Show Int where show = convertToString

pri=show`and`printString
done   = \case (Done ed)  ->True;_->False
missed = \case (Missed ed)->True;_->False
toDo   = \case (ToDo ed d)->True;_->False
all    = \_               ->True

filterAndPrint = \f->exercises`unwrapAnd`(filter f`and`pri)
showToDo   = filterAndPrint toDo
showDone   = filterAndPrint done
showMissed = filterAndPrint missed
showAll    = filterAndPrint all
