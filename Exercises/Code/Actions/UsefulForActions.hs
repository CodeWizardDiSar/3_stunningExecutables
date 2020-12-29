module UsefulForActions where
import Prelude
  ( sequence, not, (.), filter, (>), length, Bool( True ), getLine, Int, IO, (+), elem
  , (>>), (++), (>>=), String, foldl, otherwise )
import Types
  ( Exercises, Subject, Subjects )
import ToSubject
  ( toSubject )
import ToString
  ( toStringForFile, toString, print )
import FromString
  ( fromString )
import Renaming
  ( wrap, (>>>), glue )
import FileManagement
  ( writeToNextDataKeeper )
import Data.Function
  ( (&) )
import Control.Monad
  ( (>=>) )
import IsEarlierThan
  ( isEarlierThan )

writeExercisesToFile :: Exercises -> IO ()
writeExercisesToFile = toStringForFile >>> writeToNextDataKeeper

getChoice :: IO Int
getChoice = getLine >>= ( fromString >>> wrap )

combine :: [ IO Exercises ] -> IO Exercises
combine = sequence >=> ( glue >>> wrap )

showSubjects :: Exercises -> IO ()
showSubjects = exercisesToSubjects >>> printSubjects 1

toSubjects :: Exercises -> Subjects
toSubjects = foldl ( \subs ex -> addToSubjects ( toSubject ex ) subs ) [] 

addToSubjects :: Subject -> Subjects -> Subjects
addToSubjects subject subjects
  | elem subject subjects = subjects
  | otherwise = subjects ++ [ subject ]

exercisesToSubjects :: Exercises -> Subjects
exercisesToSubjects = \case
  [] -> []
  ex:exs ->
    let sub = ex & toSubject
        subs = exs & exercisesToSubjects
    in elem sub subs & \case
       True -> subs
       _ -> sub:subs

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
