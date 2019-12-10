{-# LANGUAGE ConstraintKinds #-} 
module Types where
import Control.Monad.State

class FST a where fst::STR->a  --From STring
class FLS a where fls::LNS->a  --From LineS
class FLI a where fli::LIN->a  --From LIne
class TST a where tst::a  ->STR--To STring
class TSS a where tss::a  ->STS--To StringS
class TTS a where tts::a  ->STR--To To do String

--renaming
type INT=Int     --INTeger
type STR=String  --STRing 
type FPA=FilePath--File PAth
type BOO=Bool    --BOOlean
type MON=Monad   --MONad
type IOU=IO ()   --IO Unit
type IOS=IO STR  --IO String 
type IOB=IO BOO  --IO Boolean 
type IOF=IO FPA  --IO File path

--shorter
type T=TST

--specifying
type DAY=INT            --DAY
type MNT=INT            --MoNTh
type SIZ=INT            --SIZe
type SNA=STR            --Subject NAme
type ENA=STR            --Exercise NAme
type LIN=STR            --LINe 
type WRD=STR            --WoRD
type DCR=STS            --DeCoR
type HEN=HSO ENA        --Hopefully Exercise Name
data DEX=Dex HEN INT    --Done EXercise
type DES=[DEX]          --Done ExerciseS
data TEX=Tex HEN INT DAT--To do EXercise
type TES=[TEX]          --To do ExerciseS 
type STS=[STR]          --STringS
data SUB=Sub SNA DES TES--SUBject
type SBS=[SUB]          --SuBjectS 
type LNS=[LIN]          --LiNeS 
type WRS=[WRD]          --LiNeS 
type DAT=(DAY,MNT)      --DATe

--
data HSO a=Idd a|Nng    --Hopefully SOme a, InDeeD a| NothiNG
