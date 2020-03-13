module Menus where
import Renaming        (append,forEach,glue,and,convertIntToString)
import UsefulFunctions (wrapInNLs,tabBefore,tabsBefore)
import UsefulFunctions (forEach2)
import Types           (Strings)
import Prelude         (take,($),String,(+))
import Data.Function   ((&))

-- Menus
[rootMenu,addMenu,showMenu,editMenu,deleteMenu,moveMenu] = 
 forEach2 titleAndOptions titles optionsList

titleAndOptions = \t os->
 [tabBefore t]`append`(forEach (2&tabsBefore) os)&forEach (`append`"\n")&glue
titles = ["Command me master","Add to","Show","Edit","Delete From","Move From"]
optionsList =
 [rootOptions,exceptAllOptions,showOptions,exceptAllOptions,
  exceptAllOptions,exceptAllOptions]

-- Options
putNumbers = putNumber 1
putNumber = \i -> \case 
 []   -> []
 s:ss -> glue [convertIntToString i,": ", s]:putNumber (i+1) ss

execerciseTypes = ["To Do","Done","Missed"]
exceptAllOptions = putNumbers execerciseTypes`append`[exitOption]
rootOptions = 
 putNumbers ["Add","Show","Edit","Delete","Move","Undo"]`append`[exitOption]
showOptions = putNumbers (execerciseTypes`append`["All"])`append`[exitOption]
exitOption = "enter: Exit"
