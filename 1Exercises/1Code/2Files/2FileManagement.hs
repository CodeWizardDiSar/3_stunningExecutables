{-# LANGUAGE LambdaCase #-} 
module FileManagement where
import Prelude          (IO,String,FilePath,Bool(..),(+),flip)
import Renaming         (checkThat,fileExists)
import Paths            (versionKeeper,tempVersionKeeper,dataKeeperPrefix)
import Data.Function    ((&))
import Renaming         (readFromFile,writeToFile)
import Renaming         (unwrapAnd,andThen,wrap,and,append)
import Renaming         (convertFromString,convertToString)
import System.Directory (renameFile)
-- Managing Data and Version Keepers
getVersion =
 checkThat (versionKeeper&fileExists)`unwrapAnd`\case
  True->readFromFile versionKeeper                    
  _   ->writeToFile versionKeeper "0"`andThen`wrap "0"::IO String     
updateVersion =
 getVersion`unwrapAnd`
 (addOneToString`and`writeToFile tempVersionKeeper)`andThen`
 renameFile tempVersionKeeper versionKeeper           ::IO ()     
addOneToString =
 convertFromString`and`(+1)`and`convertToString       ::String->String
nextDataKeeper =
 getVersion`unwrapAnd`(addOneAndAddDKPrefix`and`wrap) ::IO FilePath
currentDataKeeper =
 getVersion`unwrapAnd`(addDKPrefix`and`wrap)          ::IO FilePath
addOneAndAddDKPrefix  = addOneToString`and`addDKPrefix::String->FilePath
addDKPrefix           = (dataKeeperPrefix`append`)    ::String->FilePath
writeToNextDataKeeper = (\s->
 nextDataKeeper`unwrapAnd`flip writeToFile s)         ::String->IO ()
