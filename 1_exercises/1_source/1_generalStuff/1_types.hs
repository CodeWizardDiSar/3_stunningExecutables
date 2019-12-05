{-# LANGUAGE ConstraintKinds #-} 

module Types where
import Control.Monad.State

class FromFileStr    a where fromFStr  ::STR->a
class FromFileLines  a where fromFLines::Lns->a
class FromFileLine   a where fromFLine ::Lin->a
class ToFileStr      a where toFStr    ::a  ->STR
class ToStrsForDecor a where toStrsD   ::a  ->Sts

type STR=String    --String 
type FPa=FilePath  --File Path
type Boo=Bool      --Boolean
type Mon=Monad     --Monad
type IOU=IO ()     --IO unit
type IOS=IO STR    --IO String 
type IOB=IO Boo    --IO Boolean 
type IOF=IO FPa    --IO File path
type Day=Int       -- 
type Mnt=Int       -- 
type Siz=Int       -- 
type SNa=STR       -- 
type ENa=STR       -- 
type Lin=STR       -- 
type Dcr=Sts       -- 
type MEN=Maybe ENa -- 
type DEs=[DEx]     -- 
type TEs=[TEx]     -- 
type Sts=[STR]     -- 
type Sbs=[Sub]     -- 
type Lns=[Lin]     -- 
type Dat=(Day,Mnt) -- 

data DEx=DE MEN Int
data TEx=TE MEN Int Dat
data Sub=SU SNa DEs TEs
