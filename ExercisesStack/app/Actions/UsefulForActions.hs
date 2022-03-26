module Actions.UsefulForActions where

import Prelude
  ( (.), (>), length, Bool( True ), getLine, IO, (>>), (++), (>>=), String )
import Types
  ( Exercises )
import TypeClasses.ToString
  ( toStringForFile, print )
import VeryUseful.Renaming
  ( wrap, (.>) )
import Data.Function
  ( (&) )
import Files.FileManagement
  ( updateVersion, writeToNextDataKeeper )

exsToFileAndUpdate :: Exercises -> IO ()
exsToFileAndUpdate exs = exs & toStringForFile & writeToNextDataKeeper >> updateVersion

printAndGetAnswer :: String -> IO String
printAndGetAnswer = \s ->
  print s >> getLine >>= \a->
  case length a > 20 of
    True -> printAnnoyingMessage >> printAndGetAnswer s
    _ -> wrap a

printAnnoyingMessage :: IO ()
printAnnoyingMessage = print "More than 20 chars is not pretty"
