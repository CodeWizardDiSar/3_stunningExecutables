module UsefulForActions where

import Prelude
  ( not, (.), filter, (>), length, Bool( True ), getLine, IO, elem
  , (>>), (++), (>>=), String, foldl, otherwise )
import Types
  ( Exercises, Subject, Subjects )
import ToSubject
  ( toSubjects )
import ToString
  ( toStringForFile, toString, print )
import FromString
  ( fromString )
import Renaming
  ( wrap, (.>), glue )
import FileManagement
  ( writeToNextDataKeeper )
import Data.Function
  ( (&) )
import Control.Monad
  ( (>=>) )
import IsEarlierThan
  ( isEarlierThan )
import FileManagement
  ( updateVersion )

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
