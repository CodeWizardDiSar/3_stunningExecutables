{-# LANGUAGE LambdaCase #-} 
module FileManagement where
import Prelude          (IO,String,FilePath,Bool(..),(+),flip)
import Renaming         (checkThat,fileExists)
import Paths            (versionKeeper,tempVersionKeeper,dataKeeperPrefix)
import Data.Function    ((&))
import Renaming         (readFromFile,writeToFile)
import Renaming         (unwrapAnd,andThen,wrap,and,append)
import Renaming         (convertIntFromString,convertIntToString)
import System.Directory (renameFile)
import UsefulFunctions  (addOneToString)
-- Managing the Version Keeper
getVersion =
 checkThat (versionKeeper&fileExists)`unwrapAnd`\case
  True->readFromFile versionKeeper                    
  _   ->writeToFile versionKeeper "0"`andThen`wrap "0"::IO String     
updateVersion =
 getVersion`unwrapAnd`
 (addOneToString`and`writeToFile tempVersionKeeper)`andThen`
 renameFile tempVersionKeeper versionKeeper           ::IO ()     
-- Managing Data Keepers
currentDataKeeper =
 getVersion`unwrapAnd`(addDKPrefix`and`wrap)          ::IO FilePath
addDKPrefix = (dataKeeperPrefix`append`)              ::String->FilePath
nextDataKeeper =
 getVersion`unwrapAnd`
 (addOneToString`and`addDKPrefix`and`wrap)            ::IO FilePath
writeToNextDataKeeper = (\s->
 nextDataKeeper`unwrapAnd`flip writeToFile s)         ::String->IO ()
