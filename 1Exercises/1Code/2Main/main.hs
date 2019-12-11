{-# LANGUAGE LambdaCase #-} 
module Main where
import Data.Function
import Types
import General
import Add
import ShowToDo
import FileToSubs
import SubsToFile
import Control.Arrow

cha=pst "change"
mdo=pst "mdo"
sdo=pst "showDone"

wem="\tWelcome the stunning exercises manager master"--WElcoming Msg
mim="\tWhat do you want me to do?"                   --menu Intro Msg
tdm="\t\t1: Show To Do baby"                         --Show To do Msg
aem="\t\t2: Add exercise plz"                        --Add Exercise Msg
csm="\t\t3: Change something"                        --Change Something Msg
mdm="\t\t4: I finished it bitches"                   --Move to Done Msg
sdm="\t\t5: Show Done so I feel good about myself"   --Show Done Msg
exm="\t\t6: Exit"                                    --EXit Msg
bym="Bye!"                                           --BYe Msg
cfm="Hmm, I'm not sure what you want"                --ConFused Msg
mms=[mim,tdm,aem,csm,mdm,sdm,exm]::STS               --menu MsgS
pws=pss>>>wnl                    ::STS->IOU--Print Wrapped in NLs Strings
pme=pws mms                      ::IOU     --Print menu   

menu=pme>>getLine>>= \case 
    "1"->std>>menu
    "2"->add>>menu
    "3"->cha>>menu
    "4"->mdo>>menu
    "5"->sdo>>menu
    "6"->pst bym
    _  ->pst cfm>>menu

--main=2&nls>>pst wem>>menu
--main=fts>>=stf>>upv
main=date
