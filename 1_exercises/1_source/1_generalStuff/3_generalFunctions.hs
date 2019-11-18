module GeneralFunctions where
import System.Directory
import Types

getNumber :: IO Int
getNumber = do
  snum <- getLine
  return $ read snum

askAndGet :: String -> IO String
askAndGet s = do
  newline
  printString s
  getLine

printString = putStrLn
newline = printString ""

printStrings :: Strings -> IO ()
printStrings = mapM_ printString

