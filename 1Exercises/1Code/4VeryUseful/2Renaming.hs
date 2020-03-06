module Renaming where
import Prelude          (String,Int,Monad,Foldable,readFile,writeFile)
import Prelude          (replicate,words,lines,error,read,show,concat)
import Prelude          (putStrLn,map,mapM_,return,repeat,take,id)
import Prelude          ((=<<),(>>=),(>>),(++),($))
import Control.Arrow    ((>>>))
import Data.Function    ((&))
import System.Directory (doesFileExist)

-- Jokers
[checkThat,doNothing] = take 2`from`repeat id
[from     ,keepAnd  ] = take 2 $ repeat ($)

-- Normal Operators
(inputTo,append,and) = ((&),(++),(>>>):: (a->b)->(b->c)->(a->c))

-- Monad operators And Functions
andThen   = (>>)
andThen   :: Monad m=>m a->m b->m b
unwrapAnd = (>>=)
unwrapAnd :: Monad m=>m a->(a->m b)->m b
unwrapped = (=<<)
unwrapped :: Monad m=>(a->m b)->m a->m b
wrap      = return
wrap      :: Monad m=>a->m a
forEachDo = mapM_
forEachDo :: Monad m=>(a->m b)->[a]->m ()

-- Very General Fuctions
(forEach           ,glue                ,splitInWords,splitInLines     ,
 convertIntToString,convertIntFromString,printString ,printErrorMessage,
 fileExists        ,readFromFile        ,writeToFile ,repeatNTimes     ) =
 (map              ,concat::[[a]]->[a]  ,words       ,lines            ,
  show::Int->String,read  ::String->Int ,putStrLn    ,error::String->a ,
  doesFileExist    ,readFile            ,writeFile   ,replicate        )
