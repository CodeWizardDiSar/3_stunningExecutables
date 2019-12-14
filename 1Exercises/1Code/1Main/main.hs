{-# LANGUAGE LambdaCase #-} 
module Main where
import Prelude hiding (all)
import Data.Function
import Control.Arrow
import Types
import General
import FcsToExs
import ExsToFcs
import Renaming
import Messages
import ActionMessages
import Useful
import Show
import Add
import Change
import Move

--main=cdk>>=rdf>>=(fte>>>etf>>>wnk)>>upv
main = 2&nls >> pst welm >> menu
menu = pwmgldoasdf mems  [msho,add,cha,mov]
msho = pwmgldoasdf shoms $ map tme [stdo,sdon,smsd,sall]
add  = pwmgldoasdf shoms $ map tme [stdo,sdon,smsd,sall]
cha  = pwmgldoasdf shoms $ map tme [stdo,sdon,smsd,sall]
mov  = pwmgldoasdf shoms $ map tme [stdo,sdon,smsd,sall]
tme  = (>> menu) -- then menu

pwmgldoasdf= \x y-> pwmgl x>>=doasdf y
doasdf= \[a,s,d,f]-> \case 
  "a"->a
  "s"->s
  "d"->d
  "f"->f
  "" ->exi 
  _  ->cnf & tme

