module Helpers where
import Prelude 
  ( String, (++), repeat, take, IO, sequence ) 
import Types
  ( Exercises ) 
import Renaming
  ( (.>), forEach, glue, wrap ) 
import Control.Monad
  ( (>=>) )

beautify :: String -> String
beautify = ( "\t" ++ ) .> ( ++ "\n" )

glue20CharsEach :: [ String ] -> String
glue20CharsEach = forEach ( ( ++ repeat ' ') .> take 20 ) .> glue

combine :: [ IO Exercises ] -> IO Exercises
combine = sequence >=> ( glue .> wrap )
