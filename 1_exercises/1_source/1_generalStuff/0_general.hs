{-# LANGUAGE LambdaCase #-} 

module General where
import Control.Monad
import Data.Function
import Control.Arrow
import System.Directory
import Types

printString = putStrLn :: String -> IO ()
newline = printString "" :: IO ()
printStrings = mapM_ printString :: Strings -> IO ()
exists = doesFileExist  :: FilePath -> IO Bool
readFrom = readFile :: FilePath -> IO String
contents = readFile :: FilePath -> IO String
writeTo = writeFile :: FilePath -> String -> IO ()
dataDir = "../0_data" :: FilePath
dataPrefix = dataDir ++ "/data" :: FilePath
versionFile = dataDir ++ "/version" :: FilePath
newVersFile = dataDir ++ "/versionNew" :: FilePath

foldrF f = foldr (\a b -> f a ++ b) []

version :: IO String
version =
  versionFile & exists >>=
  \case True  -> readFrom versionFile
        False -> writeAndReturn "0" 

writeAndReturn s =
  writeTo versionFile s >> return s

currVersFile :: IO FilePath
currVersFile =
  version >>= ((dataPrefix++) >>> return)

nextVersFile :: IO FilePath
nextVersFile =
  version >>= 
  (strPlusOne >>> (dataPrefix++) >>>
  return)

strPlusOne :: String -> String
strPlusOne = read >>> (+1) >>> show

updateVersion :: IO ()
updateVersion =
  version >>=
  (strPlusOne >>> writeTo newVersFile) >>
  renameFile newVersFile versionFile 

getNumber :: IO Int
getNumber = getLine >>= (read >>> return)

askAndGet :: String -> IO String
askAndGet s = newline >> printString s >> getLine
