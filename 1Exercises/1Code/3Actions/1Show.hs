{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-} 
module Show where
import Prelude hiding (all,and,Nothing)
import Control.Arrow
import Data.Function
import Renaming
import Useful
import Types
import FcsToExs 
import MenuOptions
import ActionOptions

s :: SHO a => a->STR
from = ($)
fill               = \i s->take i`from`(s&withInfiniteSpaces)
withInfiniteSpaces = \s->s`append`repeat ' '
fill15ForEach      = forEach (fill 15)

s = sho
instance SHO EXR where
  sho =
    \case Don (n,nu,e)   ->[n,nu,s e]     &(fill15ForEach`and`glue)
          Mis (n,nu,e)   ->[n,nu,s e]     &(fill15ForEach`and`glue)
          Tdo (n,nu,e) da->[n,nu,s e,s da]&(fill15ForEach`and`glue)

appendNewLine = (`append`"\n")
instance SHO EXS where
  sho = forEach (sho`and`tabBefore`and`appendNewLine)`and`glue 
instance SHO HEN where sho = \case Nothing->"No Name";Indeed e->e 
instance SHO DAT where sho = \(d,m,y)->glue [sho d,"/",sho m,"/",sho y]
instance SHO INT where sho = convertToString

pri=sho`and`printString
done   = \case (Don ed)  ->True;_->False
missed = \case (Mis ed)  ->True;_->False
toDo   = \case (Tdo ed d)->True;_->False
all    = \_              ->True

filterAndPrint = \f->exercises`unwrapAnd`(filter f`and`pri)
showToDo   = filterAndPrint toDo
showDone   = filterAndPrint done
showMissed = filterAndPrint missed
showAll    = filterAndPrint all
