module FileManagement where
import Renaming         (checkThat,fileExists,printString,
                         readFromFile,writeToFile,
                         unwrapAnd,andThen,wrap,(>>>),append)
import UsefulFunctions  (addOneToString,subOneFromString)
import Prelude          (Bool(..), flip, (++))
import System.Directory (renameFile)
import Data.Function    ((&))

-- Paths
homeDir           = "/home/gnostis"
desktopDir        = homeDir     ++"/Desktop"
exercisesDir      = desktopDir  ++
                    "/StunningExecutables/Exercises"
dataDir           = exercisesDir++"/Data"
versionKeeper     = dataDir     ++"/ver"   
tempVersionKeeper = dataDir     ++"/verTmp"
dataKeeperPrefix  = dataDir     ++"/data"  

-- Get, Update and Downdate Verion
getVersion =
 checkThat (versionKeeper&fileExists)`unwrapAnd`\case
  True-> readFromFile versionKeeper                    
  _   -> writeToFile versionKeeper "0"`andThen`wrap "0"
updateVersion =
 getVersion`unwrapAnd`(addOneToString>>>writeToTemp)`andThen`
 renameTemp
downdateVersion =
 getVersion`unwrapAnd`\case
  "0"-> printString "Who you kidding brother?"
  s  -> (subOneFromString>>>writeToTemp) s`andThen`renameTemp
writeToTemp = writeToFile tempVersionKeeper
renameTemp = renameFile tempVersionKeeper versionKeeper

-- Get Current and Next Data Keeper + Write to Next
getCurrentDataKeeper =
 getVersion`unwrapAnd`(addDKPrefix>>>wrap)
getNextDataKeeper =
 getVersion`unwrapAnd`(addOneToString>>>addDKPrefix>>>wrap)
writeToNextDataKeeper = \s->
 getNextDataKeeper`unwrapAnd`flip writeToFile s
addDKPrefix = (dataKeeperPrefix++)
