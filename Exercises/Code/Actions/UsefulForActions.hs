module UsefulForActions where
import Prelude
  ( sequence, not, (<), (.), filter, (&&), (>), (||), (==), repeat, take, length, Bool(..)
  , getLine, Int, IO, (+), ($), elem, (-), (!!), (>>), (++), (>>=), String, foldl, otherwise )
import Types
  ( Exercise( ToDo ), Exercises, ToDoExercise( date ), subject, Subject, Subjects, Date ( D ) )
import ToSubject
  ( toSubject )
import ToString
  ( toStringForFile, toString )
import FromString
  ( fromString )
import Renaming
  ( wrap, (>>>), glue, forEach, printString, printErrorMessage )
import FileManagement
  ( writeToNextDataKeeper )
import Data.Function
  ( (&) )
import Control.Monad
  ( (>=>) )

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
  sub:subs -> ( [ "\t", i & toString, ": ", sub ] & glue & printString ) >>
    printSubjects (i+1) subs

sortChrono :: Exercises -> Exercises
sortChrono = \case
  [] -> [] 
  ex:exs -> sortChrono ( filter ( before ex ) exs) ++ [ ex ] ++
    sortChrono ( filter ( not . before ex) exs)

before :: Exercise -> Exercise -> Bool
before ( ToDo e1 ) ( ToDo e2 ) = date e2 `isBefore` date e1
before _ _ =
  printErrorMessage "Programmer messed up: trying to sort chronologically non-ToDo exercise"

isBefore :: Date -> Date -> Bool
isBefore ( D d m y )  ( D d' m' y' ) =
  y < y' || ( y == y' && m < m' ) || ( y == y' && m == m' && d < d' )

printAndGetAnswer :: String -> IO String
printAndGetAnswer = \s ->
  printString s >> getLine >>= \a->
  case length a > 20 of
    True -> printAnnoyingMessage >> printAndGetAnswer s
    _ -> wrap a

printAnnoyingMessage :: IO ()
printAnnoyingMessage = printString "More than 20 chars is not pretty"
