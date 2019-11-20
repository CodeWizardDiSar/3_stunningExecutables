{-# LANGUAGE LambdaCase #-} 

module FileToSubs where
import System.Directory
import Control.Arrow
import Control.Monad.State
import Data.Function
import Types
import General

noSubs = []

applyToFst :: (String -> SL a) -> SL a
applyToFst f = do
  fst:rem <- get
  put rem
  f fst

glue2 :: Monad m => m a -> m b -> m (a,b)
glue2 f s = do
  f' <- f
  s' <- s
  return (f',s') 

glue3 :: Monad m => m a -> m b -> m c -> m (a,b,c)
glue3 f s t = do
  f' <- f
  s' <- s
  t' <- t
  return (f',s',t') 

glue2List :: Monad m => m a -> m [a] -> m [a]
glue2List fst rem = do
  x <- fst
  xs <- rem
  return $ x:xs

(=:) :: Monad m => m a -> m [a] -> m [a]
(=:) = glue2List

(==:) :: Monad m => m a -> m b -> m (a,b)
(==:) = glue2

fileToSubs :: IO Subjects
fileToSubs = 
  currVersFile >>= \f -> exists f >>=
  \case True  -> f & contents >>= (subsFromString >>> return)
        False -> error $ "Current File does NOT Exist: " ++ f

subsFromString :: String -> Subjects
subsFromString = lines >>> evalState subs 

subs :: SL Subjects
subs = get >>= ifNotEmpty remSubs

ifNotEmpty :: SL Subjects -> Lines -> SL Subjects
ifNotEmpty s = \case [] -> return []
                     _  -> s

remSubs :: SL Subjects
remSubs = sub =: subs

sub :: SL Subject
sub = glue3 getName (exsTo "ToDo") (exsTo "SubEnd")

getName :: SL SubName
getName = applyToFst return

exsTo :: Line -> SL Exercises
exsTo stopLine = do
  fst:rem <- get
  case fst == stopLine of
    True  -> put rem >> return []
    False -> remExsTo stopLine

remExsTo :: Line -> SL Exercises
remExsTo stopLine = ex =: (exsTo stopLine)

ex :: SL Exercise
ex = applyToFst (process >>> return)

process :: String -> Exercise
process =
  words >>> \case
    ["NoName",num] -> (Nothing,read num)
    [name,num]     -> (Just name,read num)
    x              -> error $ exError ++ show x 

exError = "Wrong Exercise Format in file: "
