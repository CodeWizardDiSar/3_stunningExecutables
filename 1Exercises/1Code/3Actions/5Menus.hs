{-# LANGUAGE LambdaCase #-} 
module Menus where
import Renaming        (append,forEach)
import UsefulFunctions (titleAndOptions,oneToOneApplication)
-- Menus
[rootMenu,addMenu,showMenu,changeMenu,moveFromMenu,moveToMenu] =
 oneToOneApplication
  (forEach titleAndOptions
    ["Command me master","Add to" ,
     "Show"             ,"Change" ,
     "Move from"        ,"Move to"]   ,
   [rootOptions     ,exceptAllOptions,
    showOptions     ,exceptAllOptions,
    exceptAllOptions,exceptAllOptions])
-- Options
rootOptions = [addOption,showOption,changeOption,moveOption,exitOption]
[addOption,showOption,changeOption,moveOption,exitOption] =
 ["a: Add","s: Show","d: Change","f: Move","enter: Exit"]
exceptAllOptions = ["a: To Do","s: Done","d: Missed",exitOption]
showOptions      = take 3 exceptAllOptions`append`["f: All",exitOption]
