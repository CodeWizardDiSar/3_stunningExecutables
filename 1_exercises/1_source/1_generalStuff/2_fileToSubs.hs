{-# LANGUAGE LambdaCase,FlexibleContexts,TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleInstances #-} 

module FileToSubs where
import System.Directory
import Control.Arrow
import Control.Monad.State
import Data.Function
import Data.List.Split
import Types
import General

fileToSubs :: IO Sbs
fileToSubs = cvf >>= cns >>= (fromFStr >>> wim)

instance FromFileStr Sbs where
  fromFStr = lines >>> endBy ["SubEnd"] >>> map fromFLines

instance FromFileStr MEN where
  fromFStr = \case "NoName" -> Nothing; x -> Just x 

instance FromFileLines Sub where
  fromFLines = \(n:e) -> case splitOn ["ToDo"] e of 
    [d,t] -> SU n (fromFLines d) (fromFLines t)
    _     -> printErrMsg "Sub"

instance FromFileLine a => FromFileLines [a] where
  fromFLines = map fromFLine

instance FromFileLine DEx where
  fromFLine = words >>> \case
    [na,n] -> DE (fromFStr na) (read n)
    _      -> printErrMsg "Done Exercise"

instance FromFileLine TEx where
  fromFLine = words >>> \case
    [na,n,d,m] -> TE (fromFStr na) (read n) (read d, read m)
    _          -> printErrMsg "To Do Exercise"

printErrMsg s = error $ "Wrong " ++ s ++ " format in file"
