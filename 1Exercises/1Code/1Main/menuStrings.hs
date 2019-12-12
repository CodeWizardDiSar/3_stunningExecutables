{-# LANGUAGE LambdaCase #-} 
module Messages where
import Data.Function
import Renaming

cdmm="Command me master"--CommanD Me Message
shom="1: Show"          --SHOw Message
addm="2: Add"           --ADD Message
cham="3: Change"        --CHAnge Message
movm="4: Move"          --MOVe Message
exim="5: Exit"          --EXIt Message

tbd i s=(rep i "\t"&cnc)++s--TaBbeD
mems=[tbd 1 cdmm]++map (tbd 2) [shom,addm,cham,movm,exim]--MEnu MessageS
welm=tbd 1 "Welcome to the exercises manager"--WELcoming Message
byem="bye!"                                  --BYE Message
cnfm="Ehhh what?"                            --CoNFused Message
