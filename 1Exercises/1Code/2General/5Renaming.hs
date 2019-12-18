{-# LANGUAGE LambdaCase #-} 
module Renaming where
import Control.Arrow
import Data.Function
import System.Directory
import Types hiding (show)

append            = (++)
and               = (>>>)
andThen           = (>>)
unwrapAnd         = (>>=)
forEach           = map
printString       = putStrLn 
wrap              = return
glue              = concat        
fileExists        = doesFileExist 
convertFromString = read         
convertToString   = show          
printErrorMessage = error
splitInLines      = lines
splitInWords      = words
mmp               = mapM_     -- Monad MaP
rnt               = replicate -- Repeat N Times (in a list)

append            :: [a]->[a]->[a]
and               :: (a->b)->(b->c)->(a->c)
andThen           :: Monad m=>m a->m b->m b
unwrapAnd         :: Monad m=>m a->(a->m b)->m b
printString       :: String->IO ()     
wrap              :: Monad m=>a->m a
glue              :: [[a]]->[a]   
fileExists        :: FilePath->IO Boolean     
convertFromString :: String->Int     
convertToString   :: Int->String     
printErrorMessage :: String->a       
splitInLines      :: String->Lines
splitInWords      :: String->Words
mmp               :: (Monad m, Foldable t)=>(a->m b)->t a->m ()
rnt               :: Int->a->[a]
