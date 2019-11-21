{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-} 

module SubsToFile where
import Control.Arrow
import Types
import General

subsToFile :: Subjects -> IO ()
subsToFile = toStrF >>> \s -> writeToNextVers >>= \w -> w s

writeToNextVers :: IO (String -> IO ())
writeToNextVers = nextVersFile >>= (writeFile >>> return)

class ToStringsForDecor a where
  toStrsD :: a -> Strings

instance ToStringsForDecor Subject where
  toStrsD (SU n d t) = [n,toStrF d,toStrF t]

instance ToStringsForDecor DoneEx where
  toStrsD (DE m n) = [toStrF m,show n]

instance ToStringsForDecor ToDoEx where
  toStrsD (TE m n (d,mo)) = [toStrF m,show n,show d,show mo]

class ToStringForFile a where
  toStrF :: a -> String

instance ToStringForFile Subjects where
  toStrF = map toStrF >>> concat

instance ToStringForFile DoneExs where
  toStrF = map toStrF >>> concat

instance ToStringForFile ToDoExs where
  toStrF = map toStrF >>> concat

instance ToStringForFile Subject where
  toStrF = toStrsD >>> decorateWith subDecor

instance ToStringForFile DoneEx where
  toStrF = toStrsD >>> decorateWith doneExDecor

instance ToStringForFile ToDoEx where
  toStrF = toStrsD >>> decorateWith toDoExDecor

instance ToStringForFile MExName where
  toStrF = getMEN >>> \case Nothing -> "NoName"; Just n -> n 

decorateWith :: Decor -> Strings -> String
decorateWith d s = case (s,d) of
  (s:ss,d:ds) -> concat [s,d,decorateWith ds ss]
  ([],[])     -> []
  _           -> error "decoration failed"

subDecor = ["\n","ToDo\n","SubEnd\n"] :: Decor
doneExDecor = [" ","\n"] :: Decor
toDoExDecor = [" "," "," ","\n"] :: Decor
