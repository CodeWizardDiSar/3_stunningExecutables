{-# LANGUAGE LambdaCase #-} 
module General where
import Prelude hiding (and)
import Control.Monad
import Data.Function
import Control.Arrow
import System.Directory
import System.Process
import Types
import Paths
import Renaming
import Useful

version = versionKeeperExists`unwrapAnd`\case True->readVersionKeeper
                                              _   ->write0ToVKAndWrap0 
versionKeeperExists  = versionKeeper&fileExists
readVersionKeeper    = readFile versionKeeper
write0ToVKAndWrap0   = writeToVersionKeeper "0"`andThen`wrap "0"
writeToVersionKeeper = writeFile versionKeeper
version              :: IO String     
versionKeeperExists  :: IO Boolean     
readVersionKeeper    :: IO String     
write0ToVKAndWrap0   :: IO String     
writeToVersionKeeper :: String->IO ()

updateVersion = version`unwrapAnd`addOneAndWriteToTVK`andThen`makeTVKVK

addOneAndWriteToTVK = addOneToString`and`writeToTVK
addOneToString      = convertFromString`and`(+1)`and`convertToString
writeToTVK          = writeFile tempVersionKeeper
makeTVKVK           = renameFile tempVersionKeeper versionKeeper
updateVersion       :: IO ()     
addOneToString      :: String->String
writeToTVK          :: String->IO ()
makeTVKVK           :: IO ()     

currentDataKeeper     = version`unwrapAnd`(addDKPrefix`and`wrap)
nextDataKeeper        = version`unwrapAnd`(addOneAndAddDKPrefix`and`wrap)
addOneAndAddDKPrefix  = addOneToString`and`addDKPrefix
addDKPrefix           = (dataKeeperPrefix`append`)
writeFileFL           = flip writeFile
writeToNextDataKeeper = \s->nextDataKeeper`unwrapAnd`writeFileFL s 
currentDataKeeper     :: IO FilePath     
nextDataKeeper        :: IO FilePath     
addOneAndAddDKPrefix  :: String->FilePath
addDKPrefix           :: String->FilePath
writeFileFL           :: String->FilePath->IO ()
writeToNextDataKeeper :: String->IO ()
