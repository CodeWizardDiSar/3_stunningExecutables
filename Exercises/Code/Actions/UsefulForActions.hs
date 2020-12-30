module UsefulForActions where
import Prelude
  ( sequence, not, (.), filter, (>), length, Bool( True ), getLine, Int, IO, (+), elem
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

writeExercisesToFile :: Exercises -> IO ()
writeExercisesToFile = toStringForFile .> writeToNextDataKeeper

getChoice :: IO Int
getChoice = getLine >>= ( fromString .> wrap )

combine :: [ IO Exercises ] -> IO Exercises
combine = sequence >=> ( glue .> wrap )

showSubjects :: Exercises -> IO ()
showSubjects = toSubjects .> printSubjects 1

printSubjects :: Int -> Subjects -> IO ()
printSubjects = \i -> \case
  [] -> wrap ()
  sub:subs -> ( [ "\t", i & toString, ": ", sub ] & glue & print ) >>
    printSubjects ( i + 1 ) subs

sortChrono :: Exercises -> Exercises
sortChrono = \case
  [] -> [] 
  ex:exs -> sortChrono ( filter ( not . isEarlierThan ex ) exs) ++ [ ex ] ++
    sortChrono ( filter ( isEarlierThan ex ) exs)

printAndGetAnswer :: String -> IO String
printAndGetAnswer = \s ->
  print s >> getLine >>= \a->
  case length a > 20 of
    True -> printAnnoyingMessage >> printAndGetAnswer s
    _ -> wrap a

printAnnoyingMessage :: IO ()
printAnnoyingMessage = print "More than 20 chars is not pretty"
