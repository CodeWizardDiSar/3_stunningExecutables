{-# LANGUAGE ConstraintKinds #-} 
module Types where

--Hopefully SOme a:InDeeD a| NothiNG
data HSO a=Idd a|Nng deriving (Show)

--EXeRcise:DONe|MISsed|To DO
data EXR=Don EXD|Mis EXD|Tdo EXD DAT deriving (Show)

type STR=String  --STRing 
type LIN=STR     --LINe 
type LNS=[LIN]   --LiNeS
type WRD=STR     --WoRD
type WDS=[WRD]   --WorDS
type PAT=FilePath--File PAth
type INT=Int     --INTeger
type BOO=Bool    --BOOlean
type MON=Monad   --MONad
type IOU=IO ()   --IO Unit
type IOS=IO STR  --IO String 
type IOB=IO BOO  --IO Boolean 
type IOF=IO PAT  --IO File path
type FCS=STR     --File ContentS
type STS=[STR]   --STringS
type DCR=STS     --DeCoR

type DAY=INT          --DAY
type MNT=INT          --MoNTh
type YER=INT          --YEaR
type DAT=(DAY,MNT,YER)--DATe
type SNA=STR          --Subject NAme
type EXN=STR          --EXercise Number (In string cause why not)
type HEN=HSO STR      --Hopefully Exercise Name
type EXD=(SNA,EXN,HEN)
type EXS=[EXR]

class FST a where fst::STR->a--From STring
class TST a where tst::a->STR--To STring
