{-# LANGUAGE LambdaCase #-} 
module FileManagement where
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

version              = versionKeeperExists`unwrapAnd`actAccordingly
versionKeeperExists  = versionKeeper&fileExists
actAccordingly       = \case True->readVersionKeeper;_->write0ToVKAndWrap0
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
addOneToString      = read`and`(+1)`and`convertToString
writeToTVK          = writeFile tempVersionKeeper
makeTVKVK           = renameFile tempVersionKeeper versionKeeper
updateVersion       :: IO ()     
addOneAndWriteToTVK :: String->IO ()     
addOneToString      :: String->String
writeToTVK          :: String->IO ()
makeTVKVK           :: IO ()     

currentDataKeeper     = version`unwrapAnd`(addDKPrefix`and`wrap)
addDKPrefix           = (dataKeeperPrefix`append`)
nextDataKeeper        = version`unwrapAnd`(addOneAndAddDKPrefix`and`wrap)
addOneAndAddDKPrefix  = addOneToString`and`addDKPrefix
writeToNextDataKeeper = \s->nextDataKeeper`unwrapAnd`flip writeFile s 
currentDataKeeper     :: IO FilePath     
nextDataKeeper        :: IO FilePath     
addOneAndAddDKPrefix  :: String->FilePath
addDKPrefix           :: String->FilePath
writeToNextDataKeeper :: String->IO ()
