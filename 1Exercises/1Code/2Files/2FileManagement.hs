{-# LANGUAGE LambdaCase #-} 
module FileManagement where
import Prelude hiding (and)
import Data.Function ((&))
import System.Directory (renameFile)
import Paths (versionKeeper,tempVersionKeeper,dataKeeperPrefix)
import Renaming (unwrapAnd,andThen,and,wrap,append,convertToString)
import Renaming (convertFromString,fileExists,checkThat,writeToFile)
import Renaming (readFromFile)

getVersion = checkThat (versionKeeper&fileExists)`unwrapAnd`\case
  True->readFromFile versionKeeper                    
  _   ->writeToFile versionKeeper "0"`andThen`wrap "0"::IO String     

updateVersion = getVersion`unwrapAnd`
  (addOneToString`and` writeToFile tempVersionKeeper)`andThen`
  renameFile tempVersionKeeper versionKeeper::IO ()     

addOneToString =
  convertFromString`and`(+1)`and`convertToString::String->String

nextDataKeeper    = getVersion`unwrapAnd`(addOneAndAddDKPrefix`and`wrap)
currentDataKeeper = getVersion`unwrapAnd`(addDKPrefix`and`wrap)
addOneAndAddDKPrefix  = addOneToString`and`addDKPrefix
addDKPrefix           = (dataKeeperPrefix`append`)
writeToNextDataKeeper = \s->nextDataKeeper`unwrapAnd`flip writeFile s 
currentDataKeeper     :: IO FilePath     
nextDataKeeper        :: IO FilePath     
addOneAndAddDKPrefix  :: String->FilePath
addDKPrefix           :: String->FilePath
writeToNextDataKeeper :: String->IO ()
