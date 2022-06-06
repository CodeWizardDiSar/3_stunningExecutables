module Actions.Show where

import Prelude 
  ( String, IO, (>>), (>>=), (++) )
import Types
  ( HeaderRow, ExerciseType( ToDoEx, DoneEx, OtherEx ), Exercises )
import TypeClasses.ToString
  ( print )
import VeryUseful.Renaming
  ( forEach, (.>) )
import VeryUseful.UsefulFunctions
  ( doSequentially )
import Files.ExercisesFromFile
  ( toDoExercises, doneExercises, missedExercises )
import Data.Function
  ( (&) )
import Helpers
  ( glue20CharsEach, beautify )
import Data.List
  ( partition )
import TypeClasses.IsEarlierThan
  ( isEarlierThan )

showActions :: [ IO () ]
showActions =
  forEach ( printBeautified headerRow >> ) [ show ToDoEx, show DoneEx, show OtherEx, showAll ]

printBeautified :: String -> IO ()
printBeautified = beautify .> print

headerRow :: HeaderRow
headerRow = glue20CharsEach [ "Subject", "Number", "Name", "Date" ]

show :: ExerciseType -> IO ()
show = \case
  ToDoEx -> printBeautified "ToDo" >> toDoExercises >>= sortChronologically .> print
  DoneEx -> printBeautified "Done" >> doneExercises >>= print
  OtherEx -> printBeautified "Other" >> missedExercises >>= print

sortChronologically :: Exercises -> Exercises
sortChronologically = \case
  [] -> [] 
  ex:exs -> partition ( `isEarlierThan` ex ) exs & \(earlier,later) ->
            sortChronologically earlier ++ [ ex ] ++ sortChronologically later

showAll :: IO ()
showAll = doSequentially [ show ToDoEx, show DoneEx, show OtherEx ]
