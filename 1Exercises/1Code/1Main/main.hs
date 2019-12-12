{-# LANGUAGE LambdaCase #-} 
module Main where
import Data.Function
import Control.Arrow
import Types
import General
import FcsToExs
import ExsToFcs
import Renaming
import Messages
import Useful
--import Show
--import Add
--import Change
--import Move

--main=cdk>>=rdf>>=(fte>>>etf>>>wnk)>>upv
main=2&nls>>pst welm>>menu
menu=pme>>getLine>>= \case 
    "1"->sho>>menu
    "2"->add>>menu
    "3"->cha>>menu
    "4"->mov>>menu
    "5"->exi
    _  ->cnf>>menu
pme=pws mems--Print menu   

sho=pst "Show"
add=pst "Add"
cha=pst "Change"
mov=pst "Move"
exi=pst byem
cnf=pst cnfm
