{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-} 

module SubsToFile where
import Control.Arrow
import Types
import General

subsToFile :: Sbs -> IOU
subsToFile = toFStr >>> \s -> writeToNextVers >>= \w -> w s

writeToNextVers :: IO (STR -> IOU)
writeToNextVers = nvf >>= (wrf >>> wim)

instance ToStrsForDecor Sub where
  toStrsD (SU n d t) = [n,toFStr d,toFStr t]

instance ToStrsForDecor DEx where
  toStrsD (DE m n) = [toFStr m,show n]

instance ToStrsForDecor TEx where
  toStrsD (TE na n (d,m)) = [toFStr na,show n,show d,show m]

instance ToFileStr a => ToFileStr [a] where
  toFStr = mco toFStr

instance ToFileStr Sub where
  toFStr = toStrsD >>> decorateWith subDecor

instance ToFileStr DEx where
  toFStr = toStrsD >>> decorateWith doneExDecor

instance ToFileStr TEx where
  toFStr = toStrsD >>> decorateWith toDoExDecor

instance ToFileStr MEN where
  toFStr = \case Nothing -> "NoName"; Just n -> n 

decorateWith :: Dcr -> Sts -> STR
decorateWith d s = case (s,d) of
  (s:ss,d:ds) -> concat [s,d,decorateWith ds ss]
  ([],[])     -> []
  _           -> error "decoration failed"

subDecor    = ["\n","ToDo\n","SubEnd\n"] :: Dcr
doneExDecor = [" ","\n"] :: Dcr
toDoExDecor = [" "," "," ","\n"] :: Dcr
