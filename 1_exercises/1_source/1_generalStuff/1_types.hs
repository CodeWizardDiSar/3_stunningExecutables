{-# LANGUAGE ConstraintKinds #-} 

module Types where
import Control.Monad.State

class FromFileStr    a where fromFStr  ::STR->a
class FromFileLines  a where fromFLines::Lns->a
class FromFileLine   a where fromFLine ::Lin->a
class ToFileStr      a where toFStr    ::a  ->STR
class ToStrsForDecor a where toStrsD   ::a  ->Sts

type STR=String   --STRing 
type FPA=FilePath --File PAth
type BOO=Bool     --BOOlean
type MON=Monad    --MONad
type IOU=IO ()    --IO Unit
type IOS=IO STR   --IO String 
type IOB=IO BOO   --IO Boolean 
type IOF=IO FPA   --IO File path
type DAY=Int      --DAY
type MNT=Int      --MoNTh
type SIZ=Int      --SIZe
type SNa=STR      --Subject Name
type ENa=STR      -- 
type Lin=STR      -- 
type Dcr=Sts      -- 
type MEN=Maybe ENa-- 
type DEs=[DEx]    -- 
type TEs=[TEx]    -- 
type Sts=[STR]    -- 
type Sbs=[Sub]    -- 
type Lns=[Lin]    -- 
type Dat=(DAY,MNT)-- 

data DEx=DE MEN Int
data TEx=TE MEN Int Dat
data Sub=SU SNa DEs TEs
