module Menus where
import Renaming        (append,forEach,glue,and)
import UsefulFunctions (wrapInNLs,tabBefore,tabsBefore)
import UsefulFunctions (forEach2)
import Types           (Strings)
import Prelude         (take,($),String)
import Data.Function   ((&))

-- Menus
[rootMenu,addMenu,showMenu,changeMenu,moveFromMenu,moveToMenu] = 
  forEach2 titleAndOptions titles optionsList
titles =
 ["Command me master","Add to"   ,"Show"   ,
  "Change"           ,"Move from","Move to"]         ::Strings
optionsList =
 [rootOptions     ,exceptAllOptions,showOptions     ,
  exceptAllOptions,exceptAllOptions,exceptAllOptions]::[Strings]
-- Title And Options
titleAndOptions = (\x y->
  [tabBefore x]`append`(forEach (2&tabsBefore) y)&
  forEach (`append`"\n")&glue
 )::String->Strings->String
-- Options
rootOptions = 
 ["a: Add","s: Show","d: Change","f: Move","u: Undo"]`append`[exitOption]
exitOption = "enter: Exit"
exceptAllOptions = ["a: To Do","s: Done","d: Missed",exitOption]
showOptions      = take 3 exceptAllOptions`append`["f: All",exitOption]
