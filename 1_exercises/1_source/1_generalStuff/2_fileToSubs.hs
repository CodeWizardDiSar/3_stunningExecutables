{-# LANGUAGE LambdaCase #-} 

module FileToSubs where
import Data.Function
import Control.Arrow
import Control.Monad.State
import System.Directory
import Types
import General

exists = doesFileExist 
contents = readFile
noSubs = []

fileSubs :: IO Subjects
fileSubs = 
  dataFile & exists >>=
  \case True  -> subsFromFile
        False -> return noSubs

subsFromFile :: IO Subjects
subsFromFile =
  dataFile & contents >>= (subsFromString >>> return)

subsFromString :: String -> Subjects
subsFromString =
  lines >>> (subs & evalState)

subs :: SLSubs
subs =
  get >>= (remSubs & ifNotEmpty)

ifNotEmpty :: SLSubs -> Lines -> SLSubs
ifNotEmpty = \s ->
  \case [] -> return []
        _  -> s

remSubs :: SLSubs
remSubs =
  sub >>= \s ->
  subs >>= \ss ->
  return $ s:ss

sub :: SLSub
sub =
  getName >>= \name ->
  exsTo "ToDo" >>= \done ->
  exsTo "SubEnd" >>= \todo ->
  return (name,done,todo) 

getName =
  get >>= \(fst:rem) ->
  put rem >> return fst

exsTo :: Line -> SLExs
exsTo = \stopLine ->
  get >>= \(fst:rem) ->
  case fst == stopLine of
    True  -> put rem >> return []
    False -> remExsTo stopLine

remExsTo :: Line -> SLExs
remExsTo = \stopLine ->
  ex >>= \x ->
  exsTo stopLine >>= \xs ->
  return $ x:xs

ex :: SLEx
ex = 
  get >>= \(fst:rem) ->
  put rem >> (return $ processed fst)

processed :: String -> Exercise
processed =
  words >>>
  \case
    ["NoName",num] -> (Nothing,read num)
    [name,num]     -> (Just name,read num)
    x              -> show x & (exError ++) & error 

exError = "Wrong Exercise Format in file: "
