module Helpers where
import Prelude 
  ( String, (++), repeat, take ) 
import Renaming
  ( (.>), forEach, glue ) 

beautify :: String -> String
beautify = ( "\t" ++ ) .> ( ++ "\n" )

glue20CharsEach :: [ String ] -> String
glue20CharsEach = forEach ( ( ++ repeat ' ') .> take 20 ) .> glue
