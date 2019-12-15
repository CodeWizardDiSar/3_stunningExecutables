{-# LANGUAGE LambdaCase #-} 
module Messages where
import Data.Function
import Useful
import Renaming

--exi=printString byeMessage
--cnf=printString cnfm

commandMeMessage="Command me master"
addMessage="a: Add"            
showMessage="s: Show"           -- SHOw Message
changeMessage="d: Change"         -- CHAnge Message
moveMessage="f: Move"           -- MOVe Message
exitOption="enter: Exit"       -- EXIt Message

menuMessages=titleAndOptions commandMeMessage [addMessage,showMessage,changeMessage,moveMessage,exitOption]
welcomingMessage=tabsBefore 1 "Welcome to the exercises manager"
byeMessage="bye!"
showConfusionMessage="Ehhh what?"
