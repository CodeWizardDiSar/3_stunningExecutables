module Helpers where
import Prelude 
  ( String, (++), repeat, take ) 
import Renaming
  ( (>>>), forEach, glue ) 

beautify :: String -> String
beautify = ( "\t" ++ ) >>> ( ++ "\n" )

putTogether :: [ String ] -> String
putTogether = forEach ( ( ++ repeat ' ') >>> take 20 ) >>> glue
