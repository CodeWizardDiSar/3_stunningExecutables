module Renaming where
import Prelude 
  ( String, Int, Monad, readFile, writeFile, replicate, lines, error, read, show, concat
  , putStrLn, map, mapM_, return, id, (>>=), (>>), (++), flip, (.) )
import System.Directory
  ( doesFileExist )

infixl 9 >>>
(>>>) :: ( a -> b ) -> ( b -> c ) -> ( a -> c )
(>>>) = flip (.)

wrap :: Monad m => a -> m a
wrap = return
forEachDo :: Monad m => ( a -> m b ) -> [ a ] -> m ()
forEachDo = mapM_

( forEach, glue, splitInLines, printErrorMessage, fileExists, readFromFile
 ,writeToFile, repeatNTimes) =
  ( map, concat :: [ [ a ] ] -> [ a ], lines , error :: String -> a, doesFileExist
  , readFile, writeFile, replicate )
