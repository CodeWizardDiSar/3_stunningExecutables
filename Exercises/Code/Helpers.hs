module Helpers where

import Prelude 
  ( String, (++), repeat, take, IO, sequence, filter, not, (.), (==) ) 
import Types
  ( Exercises, ExercisesAndChosen( ExercisesAndChosen ) )
import Renaming
  ( (.>), forEach, glue, wrap ) 

beautify :: String -> String
beautify = ( "\t" ++ ) .> ( ++ "\n" )

glue20CharsEach :: [ String ] -> String
glue20CharsEach = forEach ( ( ++ repeat ' ' ) .> take 20 ) .> glue

removeChosen :: ExercisesAndChosen -> Exercises
removeChosen ( ExercisesAndChosen exercises chosen ) =
  filter ( not . ( == chosen ) ) exercises
