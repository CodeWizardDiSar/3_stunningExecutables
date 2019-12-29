{-# LANGUAGE LambdaCase #-} 
module Menus where
import Renaming (append,forEach)
import Useful (titleAndOptions)

rootMenu = titleAndOptions "Command me master"
  [addOption,showOption,changeOption,moveOption,exitOption]
[addOption,showOption,changeOption,moveOption,exitOption] =
  ["a: Add","s: Show","d: Change","f: Move","enter: Exit"]

--To Put Together
--[addMenu        
-- ,showMenu       
-- ,changeMenu     
-- ,moveFromMenu   
-- ,moveToMenu]=forEach (\f -> f x) (forEach titleAndOptions
--  ["Add to"   
--  ,"Show"     
--  ,"Change"   
--  ,"Move from"
--  ,"Move to"  ])
--[options
--,showOptions
--,options
--,options
--,options]
addMenu        = titleAndOptions "Add to"    options
showMenu       = titleAndOptions "Show"      showOptions
changeMenu     = titleAndOptions "Change"    options
moveFromMenu   = titleAndOptions "Move from" options
moveToMenu     = titleAndOptions "Move to"   options
options        = ["a: To Do","s: Done","d: Missed",exitOption]
showOptions    = take 3 options`append`["f: All",exitOption]
