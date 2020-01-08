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
githubDir         = homeDir     `append`"/Desktop/1Github"
exercisesDir      = githubDir   `append`"/3StunningExecutables/1Exercises"
dataDir           = exercisesDir`append`"/3Data"
versionKeeper     = dataDir     `append`"/ver"   
tempVersionKeeper = dataDir     `append`"/verTmp"
dataKeeperPrefix  = dataDir     `append`"/data"  
-- Managing the Version Keeper
getVersion = checkThat (versionKeeper&fileExists)`unwrapAnd`\case
  True->readFromFile versionKeeper                    
  _   ->writeToFile versionKeeper "0"`andThen`wrap "0"::IO String     
updateVersion = getVersion`unwrapAnd`
 (addOneToString`and`writeToFile tempVersionKeeper)`andThen`
 renameFile tempVersionKeeper versionKeeper           ::IO ()     
downdateVersion = getVersion`unwrapAnd`\case
 "0" -> printString "Who you kidding brother?"
s   -> (subOneFromString`and`writeToFile tempVersionKeeper) s`andThen`
      renameFile tempVersionKeeper versionKeeper      ::IO ()     
-- Managing Data Keepers
currentDataKeeper =
 getVersion`unwrapAnd`(addDKPrefix`and`wrap)          ::IO FilePath
addDKPrefix = (dataKeeperPrefix`append`)              ::String->FilePath
nextDataKeeper = getVersion`unwrapAnd`
 (addOneToString`and`addDKPrefix`and`wrap)            ::IO FilePath
writeToNextDataKeeper = (\s->
 nextDataKeeper`unwrapAnd`flip writeToFile s)         ::String->IO ()
