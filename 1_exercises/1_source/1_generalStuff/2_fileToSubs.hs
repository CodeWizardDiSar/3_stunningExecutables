{-# LANGUAGE LambdaCase,FlexibleContexts,TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleInstances #-} 

module FileToSubs where
import System.Directory
import Control.Arrow
import Control.Monad.State
import Data.Function
import Types
import General

class FromFileConts a where
  fromStrF :: String -> a

class FromFileLines a where
  fromStrsF :: String -> a

instance FromFileConts Subjects where
  fromStrF = subLines >>> map fromStrsF 

instance FromFileString DoneExs where
  fromStrsF = subsfromstring

instance FromFileString ToDoExs where
  fromStrsF = subsfromstring

instance FromFileString Subject where
  fromStrsF = subsFromString

instance FromFileString DoneEx where
  fromStrsF = subsfromstring

instance FromFileString ToDoEx where
  fromStrsF = subsfromstring

instance FromFileString MExName where
  fromStrsF = \case "NoName" -> Nothing;x -> Just x 

subLines = lines >>> splitOn ["SubEnd"]
