module UsefulForActions where
import Prelude
  ( sequence, not, (<), (.), filter, (&&), (>), (||), (==), repeat, take, length, Bool(..)
  , getLine, Int, IO, (+), ($), elem, (-), (!!), (>>), (++), (>>=), String )
import Types
  ( Exercise, Exercises, exerciseData, getTDate, subjectName, Subject, Subjects, Date ( D ) )
import Renaming
  ( wrap, (>>>), unwrapAnd, glue, forEach, printString, convertIntToString
  , convertIntFromString )
import StringFromExercises
  ( exercisesToString )
import FileManagement
  ( writeToNextDataKeeper )
import Data.Function
  ( (&) )
import Control.Monad
  ( (>=>) )

beautify :: String -> String
beautify = ( "\t" ++ ) >>> ( ++ "\n" )

putTogether :: [ String ] -> String
putTogether = ( ( (++ repeat ' ') >>> take 20 ) `forEach` ) >>> glue

printBeutified :: String -> IO ()
printBeutified = beautify >>> printString

writeExercisesToFile :: Exercises -> IO ()
writeExercisesToFile = exercisesToString >>> writeToNextDataKeeper

getChoice :: IO Int
getChoice = getLine >>= ( convertIntFromString >>> wrap )

combine :: [ IO Exercises ] -> IO Exercises
combine = sequence >=> ( glue >>> wrap )

showSubjects :: Exercises -> IO ()
showSubjects = exercisesToSubjects >>> printSubjects 1

exercisesToSubjects :: Exercises -> Subjects
exercisesToSubjects = \case
  [] -> []
  ex:exs ->
    let sub = ex & getSub
        subs = exs & exercisesToSubjects
    in elem sub subs & \case
       True -> subs
       _ -> sub:subs

printSubjects :: Int -> Subjects -> IO ()
printSubjects = \i -> \case
  [] -> wrap ()
  sub:subs -> ( [ "\t", i & convertIntToString, ": ", sub ] & glue & printString ) >>
    printSubjects (i+1) subs

getSub :: Exercise -> Subject
getSub = exerciseData >>> subjectName

sortChrono :: Exercises -> Exercises
sortChrono = \case
  [] -> [] 
  ex:exs -> sortChrono ( filter ( before ex ) exs) ++ [ ex ] ++
    sortChrono ( filter ( not . before ex) exs)

before :: Exercise -> Exercise -> Bool
before = \e1 e2 -> getTDate e2 `isBefore` getTDate e1

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
