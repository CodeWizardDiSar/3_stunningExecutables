{-# LANGUAGE LambdaCase #-} 
module Show where

--FOrmat String
--fos="\tSub Name |\tEx Number |\tEx Name |\tRemaining"
--SHOw
--sho=pst fos>>
















--import Prelude hiding (seq)
--import Control.Arrow
--import Control.Monad
--import Data.Function
--import Data.Time.Calendar
--import Data.Time.Clock
--import StrToExs
--import Types
--import General

--r3130=cnc $ repeat [31,30]
--r3031=(31:30:r3031)
--ok2=take 5 r3031
--ok1' y=31:take 7 (take 1 r3031 ++ [feb y] ++ drop 2 r3031)
--feb= (`mod` 4)>>>(==0)>>> \case True->29;_->28 
--
--date=getCurrentTime>>=(utctDay>>>toGregorian>>>wim)
--nim="Not Important"
--mtm="More than a month"
--seq=sequence
--mas f=map f>>>seq 
--
--std=nli>>pst fom>>nli>>ptd
--ptd=fts>>=mas tts>>=(cnc>>>pst)::IOU
--tdd=[", ",", ","\n"]
--
--instance TTS SUB where tts (Sub n _ t)=t&mas (tte n)>>=(cnc>>>wim)
--tte n=tes>>>seq>=>dew tdd>>>pnf n>>>wim--To To do Exercise
--pnf n=(cnc ["\t\t",n,", "]++)          --Put Name in Front
--tes (Tex na n d)                   --To do Exercise Strings
--  =map wim [cts n,case na of Nng->nim;Idd n->n]++[red d]
--
--red::DAT->IOS
--red (d,m,y)=date>>= \(y',m',d')->m'==m&(\case True->d'-d&cts;_->mtm)&wim
