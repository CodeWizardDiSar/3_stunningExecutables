{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-} 
{-# LANGUAGE ConstraintKinds #-}
module SubsToFile where
import Control.Arrow
import Types
import General

--Decors
sud=["\n","ToDo\n","SubEnd\n"]::DCR--Subject
dod=[" ","\n"]                ::DCR--Done
tdd=[" "," "," ","\n"]        ::DCR--To Do

stf::SBS->IOU--Subjects To File
stf=tst>>> \s->wdk>>= \w->w s
wdk::IO (STR->IOU)--Write Data Keeper
wdk=ndk>>=(wrf>>>wim)
tms::(T a,T b,T c,T d)=>(a,b,c,d)->STS--Tuple Map to String
tms (a,b,c,d)=[tst a,tst b,tst c,tst d]

instance TSS SUB        where tss (Sub n d t)     =[n,tst d,tst t]
instance TSS DEX        where tss (Dex m n)       =[tst m,tst n]
instance TSS TEX        where tss (Tex na n (d,m))=tms (na,n,d,m)
instance TST a=>TST [a] where tst= mco tst
instance TST SUB        where tst= tss>>>dew sud
instance TST DEX        where tst= tss>>>dew dod
instance TST TEX        where tst= tss>>>dew tdd
instance TST HEN        where tst= \case Nng->"NoName";Idd n->n 
instance TST INT        where tst= cts
