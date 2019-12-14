{-# LANGUAGE LambdaCase #-} 
module Messages where
import Data.Function
import Useful
import Renaming

exi=pst byem
cnf=pst cnfm

cdmm="Command me master" -- CommanD Me Message
shom="a: Show"           -- SHOw Message
addm="s: Add"            -- ADD Message
cham="d: Change"         -- CHAnge Message
movm="f: Move"           -- MOVe Message
exim="enter: Exit"       -- EXIt Message

mems=msf cdmm [shom,addm,cham,movm,exim]     -- MEnu MessageS
welm=tbd 1 "Welcome to the exercises manager"-- WELcoming Message
byem="bye!"                                  -- BYE Message
cnfm="Ehhh what?"                            -- CoNFused Message
