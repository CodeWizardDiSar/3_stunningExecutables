{-# LANGUAGE LambdaCase #-} 
module Renaming where
import Control.Arrow
import Data.Function
import System.Directory
import Types hiding (makeStringFrom)

from              = ($)
keepAnd     = ($)
append            = (++)
and               = (>>>)
andThen           = (>>)
unwrapAnd         = (>>=)
unwrapped         = (=<<)
forEach           = map
printString       = putStrLn 
wrap              = return
glue              = concat        
fileExists        = doesFileExist 
convertToString   = show
printErrorMessage = error
splitInLines      = lines
splitInWords      = words
forEachDo         = mapM_
repeatNTimes      = replicate
from              :: (a->b)->a->b
append            :: [a]->[a]->[a]
and               :: (a->b)->(b->c)->(a->c)
andThen           :: Monad m=>m a->m b->m b
unwrapAnd         :: Monad m=>m a->(a->m b)->m b
unwrapped         :: Monad m=>(a->m b)->m a->m b
printString       :: String->IO ()     
wrap              :: Monad m=>a->m a
glue              :: [[a]]->[a]   
fileExists        :: FilePath->IO Boolean     
convertToString   :: Int->String     
printErrorMessage :: String->a       
splitInLines      :: String->Lines
splitInWords      :: String->Words
forEachDo         :: (Monad m, Foldable t)=>(a->m b)->t a->m ()
repeatNTimes      :: Int->a->[a]
