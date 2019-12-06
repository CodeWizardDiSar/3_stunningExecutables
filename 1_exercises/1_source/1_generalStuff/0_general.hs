{-# LANGUAGE LambdaCase #-} 

module General where
import Control.Monad
import Data.Function
import Control.Arrow
import System.Directory
import Types

--Capital letters in comments explain names

--paths
vek=(dkd++"/ver")   ::FPA--VErsion Keeper
dkd="../0_data"     ::FPA--Data Keeper Dir
dkp=(dkd++"/data")  ::FPA--Data Keeper file Prefix
tvk=(dkd++"/verTmp")::FPA--Temporary Version Keeper

--renaming
rdf=readFile     ::FPA->IOS     --ReaD File
cns=readFile     ::FPA->IOS     --CoNtentS
wrf=writeFile    ::FPA->STR->IOU--WRite File
pst=putStrLn     ::STR->IOU     --Print STring
wim=(return)     ::MON m=>a->m a--Wrap In Monad
exi=doesFileExist::FPA->IOB     --file EXIsts?

--had to be done
nli=pst ""                     ::IOU               --New LIne
nls=(\case 0->wim ()
           i->nli>>((i-1)&nls))::Int->IOU          --New LineS
wnl=(\p->nli>>p>>nli)          ::IOU->IOU          --Wrap In Newlines
aos=(read>>>(+1)>>>show)       ::STR->STR          --Add One to String
pss=mapM_ pst                  ::Sts->IOU          --Print StringS
mco=(\f->map f>>>concat)       ::(a->[b])->[a]->[b]--Map and Concat

--if there is a version keeper
--read the version from it 
--otherwise create one, write 0 to it and also wrap 0 in IO
ver=(vke>>=bca rvk ww0)::IOS         --VERsion
vke=vek&exi            ::IOB         --Version Keeper Exists?
bca=(\a b->
  \case True ->a;_->b) ::a->a->BOO->a--Bool CAse (if then else)
rvk=rdf vek            ::IOS         --Read Version Keeper
ww0=wwm "0"            ::IOS         --Write to vk and Wrap in monad "0"
wwm=(\s->wvk s>>wim s) ::STR->IOS    --Write to vk and Wrap in Monad
wvk=wrf vek            ::STR->IOU    --Write to Version Keeper

--get version
--add one to it
--write new version to version keeper
upv=a1v>>rev             ::IOU--UPdate Version
a1v=(ver>>=awt)          ::IOU--Add 1 to Version
awt=(aos>>>wtv)          ::STR->IOU--Add 1 and Write to Temp vk
wtv=wrf tvk              ::STR->IOU--Write to Temp Version keeper
rev=renameFile tvk vek   ::IOU--REname temp Vk to vk

cvf=(ver>>=apw)          ::IOF     --Current Version File path
apw=((dkp++)>>>wim)      ::STR->IOS--Append to Prefix and Wrap in monad

nvf=(ver>>=aaw)          ::IOF     --Next Version File path
aaw=(aos>>>apw)          ::STR->IOS--Add 1 Append to prefix and Wrap
