module Renaming where
import Prelude 
  ( String, Int, Monad, readFile, writeFile, replicate, lines, error, read, show, concat
  , putStrLn, map, mapM_, return, id, (>>=), (>>), (++), flip, (.) )
import System.Directory
  ( doesFileExist )

infixl 9 >>>
(>>>) :: (a->b)->(b->c)->(a->c)
(>>>) = flip (.)

andThen   :: Monad m=>m a->m b->m b
andThen   = (>>)
unwrapAnd :: Monad m=>m a->(a->m b)->m b
unwrapAnd = (>>=)
wrap      :: Monad m=>a->m a
wrap      = return
forEachDo :: Monad m=>(a->m b)->[a]->m ()
forEachDo = mapM_

( forEach, glue, splitInLines, convertIntToString, convertIntFromString, printString
 ,printErrorMessage, fileExists, readFromFile, writeToFile, repeatNTimes) =
  ( map, concat :: [ [ a ] ] -> [ a ], lines, show :: Int -> String, read :: String -> Int
  , putStrLn, error :: String -> a, doesFileExist, readFile, writeFile, replicate )
