module Menus where
import Renaming        (append,forEach,glue,and)
import UsefulFunctions (wrapInNLs,tabBefore,tabsBefore)
import UsefulFunctions (forEach2)
import Types           (Strings)
import Prelude         (take,($),String)
import Data.Function   ((&))

-- Menus
[rootMenu,addMenu,showMenu,editMenu,
 deleteMenu,moveMenu] = 
  forEach2 titleAndOptions titles optionsList

titles =
 ["Command me master","Add to","Show",
  "Edit","Delete From","Move From"]::Strings

optionsList =
 [rootOptions     ,exceptAllOptions,
  showOptions     ,exceptAllOptions,
  exceptAllOptions,exceptAllOptions]::[Strings]

-- Title And Options
titleAndOptions = (\x y->
  [tabBefore x]`append`
  (forEach (2&tabsBefore) y)&
  forEach (`append`"\n")&glue
 )::String->Strings->String

-- Options
rootOptions = 
 ["1: Add","2: Show","3: Edit",
  "4: Delete","5: Move","6: Undo",exitOption]

exceptAllOptions =
 ["1: To Do" ,"2: Done","3: Missed",exitOption]

showOptions =
 take 3 exceptAllOptions`append`
 ["4: All",exitOption]

exitOption = "enter: Exit"
