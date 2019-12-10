{-# LANGUAGE LambdaCase,FlexibleInstances #-} 
module FileToSubs where
import System.Directory
import Prelude hiding (fst)
import Control.Arrow
import Control.Monad.State
import Data.Function
import Data.List.Split
import Types
import General

--strings
tdo="ToDo"           ::STR
don="Done"           ::STR
sub="Sub"            ::STR
sbe="SubEnd"         ::STR
wrg="Wrong "         ::STR
fif=" format in file"::STR

--renaming
sil=lines  ::STR->LNS      --Split In Lines
siw=words  ::STR->WRS      --Split In Words
nng=Nothing::Maybe a       --NothiNG

sll=(\l->splitOn [l])      ::LIN->LNS->[LNS]--Split Lines On Line
--sll but no [] if last line is split line
ell=(\l->endBy   [l])      ::LIN->LNS->[LNS]--End By Line
pem=(\s->error$wrg++s++fif)::STR->a         --Print Error Message
 
--Current Data Keeper CoNtentS > subjects From STring > Wrap In Monad
fts=cdk>>=cns>>=(fst>>>wim)::IO SBS--File To Subjects

instance FST SBS where fst=sil>>>ell sbe>>>map fls
instance FLS SUB where --n=Name es=ExerciseS d=Done t=To do
  fls= \(n:es)->case sll tdo es of [d,t]->Sub n (fls d) (fls t);_->pem sub
instance FLI a => FLS [a] where fls=map fli
instance FLI DEX where--na=NAme,n=Number
  fli=siw>>> \case[na,n]->Dex (fst na) (cfs n);_->pem don
instance FLI TEX where--na=NAme,n=Number,d=Day,m=Month
  fli=siw>>> \case[na,n,d,m]->Tex (fst na) (cfs n) (cfs d,cfs m);_->pem tdo
instance FST HEN where fst= \case"NoName"->Nng;x->Idd x
