{-# LANGUAGE LambdaCase #-} 

module General where
import Control.Monad
import Data.Function
import Control.Arrow
import System.Directory
import Types

exists = doesFileExist 
readFrom = readFile
contents = readFile
writeTo = writeFile

myFoldr myFForA =
  foldr (\a b -> myFForA a ++ b) []

versionFile :: FilePath
versionFile = "../0_data/version"
newVersionFile = "../0_data/versionNew"

version :: IO String
version =
  versionFile & exists >>=
  \case True  -> readFrom versionFile
        False -> writeTo versionFile "0" >> return "0"

currVersFile :: IO FilePath
currVersFile =
  version >>= (("../0_data/data"++) >>> return)

nextVersFile :: IO FilePath
nextVersFile =
  version >>= 
  (read >>> (+1) >>> show >>>
  ("../0_data/data"++) >>>
  return)

updateVersion :: IO ()
updateVersion =
  version >>=
  (read >>> (+1) >>> show >>> writeTo newVersionFile) >>
  renameFile newVersionFile versionFile 

getNumber :: IO Int
getNumber =
  getLine >>= (read >>> return)

askAndGet :: String -> IO String
askAndGet s = 
  newline >> printString s >> getLine

printString = putStrLn
newline = printString ""

printStrings :: Strings -> IO ()
printStrings = mapM_ printString
