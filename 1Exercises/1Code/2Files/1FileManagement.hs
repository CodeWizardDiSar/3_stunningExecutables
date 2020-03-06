module FileManagement where
import Renaming         (checkThat,fileExists,printString)
import Renaming         (readFromFile,writeToFile)
import Renaming         (unwrapAnd,andThen,wrap,and,append)
import Renaming         (convertIntFromString,convertIntToString)
import UsefulFunctions  (addOneToString,subOneFromString)
import Prelude          (IO,String,FilePath,Bool(..),(+),flip)
import System.Directory (renameFile)
import Data.Function    ((&))

-- Paths
homeDir           = "/home/gnostis"
desktopDir        = homeDir     `append`"/Desktop"
exercisesDir      = desktopDir  `append`"/3StunningExecutables/1Exercises"
dataDir           = exercisesDir`append`"/3Data"
versionKeeper     = dataDir     `append`"/ver"   
tempVersionKeeper = dataDir     `append`"/verTmp"
dataKeeperPrefix  = dataDir     `append`"/data"  

-- Get, Update and Downdate verion
getVersion =
 checkThat (versionKeeper&fileExists)`unwrapAnd`\case
  True->
   readFromFile versionKeeper                    
  _   ->
   writeToFile versionKeeper "0"`andThen`
   wrap "0"::IO String     

updateVersion =
 getVersion`unwrapAnd`(
  addOneToString`and`
  writeToTemp
 )`andThen`
 renameTemp::IO ()     

downdateVersion =
 getVersion`unwrapAnd`\case
  "0"->
   printString "Who you kidding brother?"
  s  ->
   (subOneFromString`and`
    writeToTemp) s`andThen`
   renameTemp::IO ()     

writeToTemp = writeToFile tempVersionKeeper
renameTemp = renameFile tempVersionKeeper versionKeeper

-- Get Current and Next Data Keeper + Write to Next
getCurrentDataKeeper =
 getVersion`unwrapAnd`
 (addDKPrefix`and`
  wrap)::IO FilePath

getNextDataKeeper =
 getVersion`unwrapAnd`
 (addOneToString`and`
  addDKPrefix   `and`
  wrap)::IO FilePath

writeToNextDataKeeper = (\s->
 getNextDataKeeper`unwrapAnd`
 flip writeToFile s)::String->IO ()

addDKPrefix = (dataKeeperPrefix`append`)::String->FilePath
