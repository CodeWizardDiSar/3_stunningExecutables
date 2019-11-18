{-# LANGUAGE LambdaCase #-} 

module FileToSubs where
import Control.Arrow
import Control.Monad.State
import System.Directory
import Types

dataFile = "../0_data/data"
isThereA = doesFileExist 
readFrom = readFile
flipArgsOf = flip
noSubs = []

fileSubs :: IO Subjects
fileSubs = 
  isThereA dataFile >>=
  \case True  -> subsFromFile
        False -> return noSubs

subsFromFile :: IO Subjects
subsFromFile =
  readFrom dataFile >>= subsFromFileConts

subsFromFileConts :: String -> IO Subjects
subsFromFileConts =
  lines >>> evalState subs >>> return

subs :: SLSubs
subs =
  get >>= ifNotEmpty remSubs

ifNotEmpty :: State a [b] -> [c] -> State a [b]
ifNotEmpty f =
  \case [] -> return []
        _  -> f

remSubs :: SLSubs
remSubs =
  sub >>= \s ->
  subs >>= \ss ->
  return $ s:ss

sub :: SLSub
sub =
  getName >>= \name ->
  exsUntil "ToDo" >>= \done ->
  exsUntil "SubEnd" >>= \todo ->
  return (name,done,todo)

getName =
  get >>= \(fst:rem) ->
  put rem >> return fst

exsUntil :: Line -> SLExs
exsUntil stopLine =
  get >>= \(fst:rem) ->
  case fst == stopLine of
    True  -> put rem >> return []
    False -> remExsUntil stopLine

remExsUntil :: Line -> SLExs
remExsUntil stopLine =
  ex >>= \x ->
  exsUntil stopLine >>= \xs ->
  return $ x:xs

ex :: SLEx
ex = 
  get >>= \(fst:rem) ->
  put rem >>
  (return $ processed fst)

processed :: String -> Exercise
processed =
  words >>>
  \case
    ["NoName",num] -> (Nothing,read num)
    [name,num]     -> (Just name,read num)
    x              -> error $ exError ++ show x

exError = "Wrong Exercise Format in file: "
