module FileManagement where

import Prelude
  ( Bool( True ), flip, (++), (>>=), (>>), String, IO )
import Renaming
  ( fileExists, readFromFile, writeToFile, wrap, (.>) )
import UsefulFunctions
  ( addOneToString, subOneFromString )
import System.Directory
  ( renameFile )
import Data.Function
  ( (&) )
import Types
  ( Path )
import ToString
  ( print )

homeDir :: Path
homeDir = "/home/gnostis"

desktopDir :: Path
desktopDir = homeDir ++ "/Desktop"

exercisesDir :: Path
exercisesDir = desktopDir ++ "/StunningExecutables/Exercises"

dataDir :: Path
dataDir = exercisesDir ++ "/Data"

versionKeeper :: Path
versionKeeper = dataDir ++ "/ver"   

tempVersionKeeper :: Path
tempVersionKeeper = dataDir ++ "/verTmp"

dataKeeperPrefix :: Path
dataKeeperPrefix = dataDir ++ "/data"  

getVersion :: IO String
getVersion =
  versionKeeper & fileExists >>= \case
    True -> readFromFile versionKeeper                    
    _ -> writeToFile versionKeeper "0" >> wrap "0"

updateVersion :: IO ()
updateVersion = getVersion >>= addOneToString .> writeToTemp >> renameTemp

downdateVersion :: IO ()
downdateVersion =
  getVersion >>= \case
    "0" -> print "Who you kidding brother?"
    version -> subOneFromString version & writeToTemp >> renameTemp

writeToTemp :: String -> IO ()
writeToTemp = writeToFile tempVersionKeeper

renameTemp :: IO ()
renameTemp = renameFile tempVersionKeeper versionKeeper

getCurrentDataKeeper :: IO String
getCurrentDataKeeper = getVersion >>= addDKPrefix .> wrap

getNextDataKeeper :: IO Path
getNextDataKeeper = getVersion >>= addOneToString .> addDKPrefix .> wrap

writeToNextDataKeeper :: String -> IO ()
writeToNextDataKeeper = \s -> getNextDataKeeper >>= flip writeToFile s

addDKPrefix :: String -> Path
addDKPrefix = ( dataKeeperPrefix ++ )
