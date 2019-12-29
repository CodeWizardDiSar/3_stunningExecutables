{-# LANGUAGE LambdaCase #-} 
module Renaming where
import Prelude          (String,Int,Monad,Foldable,readFile,writeFile)
import Prelude          (replicate,words,lines,error,read,show,concat)
import Prelude          (putStrLn,map,mapM_,return,repeat,take,id)
import Prelude          ((=<<),(>>=),(>>),(++),($))
import Control.Arrow    ((>>>))
import System.Directory (doesFileExist)
-- Jokers
[checkThat] = take 1`from`repeat id
-- Normal Operators
([from,keepAnd],append,and)
   = (take 2 $ repeat ($),(++),(>>>):: (a->b)->(b->c)->(a->c))
-- Monad operators And Functions
andThen   = (>>)
unwrapAnd = (>>=)
unwrapped = (=<<)
wrap      = return
forEachDo = mapM_
andThen   :: Monad m=>m a->m b->m b
unwrapAnd :: Monad m=>m a->(a->m b)->m b
unwrapped :: Monad m=>(a->m b)->m a->m b
wrap      :: Monad m=>a->m a
forEachDo :: (Monad m, Foldable t)=>(a->m b)->t a->m ()
-- Very General Fuctions
(forEach        ,printString      ,glue             ,fileExists  ,
 convertToString,convertFromString,printErrorMessage,splitInLines,
 splitInWords   ,repeatNTimes     ,writeToFile      ,readFromFile) =
  (map  ,putStrLn ,concat   ,doesFileExist,
   show ,read     ,error    ,lines        ,
   words,replicate,writeFile,readFile)
glue              :: [[a]]->[a]   
convertToString   :: Int->String     
convertFromString :: String->Int
printErrorMessage :: String->a       
