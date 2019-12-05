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

change   = putStrLn "change"
moveDone = putStrLn "moveDone"
showDone = putStrLn "showDone"

welcomingMsg="\tWelcome the stunning exercises manager master"::STR
menuIntroMsg="\tWhat do you want me to do?"                   ::STR
showToDoMsg ="\t\t1: Show To Do baby"                         ::STR
addMsg      ="\t\t2: Add exercise plz"                        ::STR
changeMsg   ="\t\t3: I want to change something"              ::STR
moveDoneMsg ="\t\t4: Move exercise to Done baby"              ::STR
showDoneMsg ="\t\t5: Show Done so I feel good about myself"   ::STR
exitMsg     ="\t\t6: Exit"                                    ::STR
byeMsg      ="Bye!"                                           ::STR
confusedMsg ="Hmm, I'm not sure what you want"                ::STR
wM          =welcomingMsg                                     ::STR
mIM         =menuIntroMsg                                     ::STR
sTDM        =showToDoMsg                                      ::STR
aM          =addMsg                                           ::STR
cM          =changeMsg                                        ::STR
mDM         =moveDoneMsg                                      ::STR
sDM         =showDoneMsg                                      ::STR
eM          =exitMsg                                          ::STR
menuMsgs    =[mIM,sTDM,aM,cM,mDM,sDM,eM]                      ::Sts
printMenu   =(printWINSts menuMsgs)                           ::IOU
printWINSts =(pss>>>wnl)                      ::Sts->IOU

menu = do
  printMenu
  getLine >>= \case 
    "1" -> showToDo            >>menu
    "2" -> add                 >>menu
    "3" -> change              >>menu
    "4" -> moveDone            >>menu
    "5" -> showDone            >>menu
    "6" -> pst byeMsg
    _   -> pst confusedMsg>>menu

main = 2&nls>>pst welcomingMsg>>menu
