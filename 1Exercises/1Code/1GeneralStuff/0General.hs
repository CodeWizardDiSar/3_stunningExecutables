{-# LANGUAGE LambdaCase #-} 
module General where
import Control.Monad
import Data.Function
import Control.Arrow
import System.Directory
import Types

--Capital letters in comments explain names

--renaming
rdf=readFile     ::FPA->IOS     --ReaD File
cns=readFile     ::FPA->IOS     --CoNtentS
wrf=writeFile    ::FPA->STR->IOU--WRite File
pst=putStrLn     ::STR->IOU     --Print STring
wim=return       ::MON m=>a->m a--Wrap In Monad
exi=doesFileExist::FPA->IOB     --file EXIsts?
cnc=concat       ::[[a]]->[a]   --CoNCat
cfs=read         ::STR->INT     --Convert From String
cts=show         ::INT->STR     --Convert From String
err=error        ::STR->a       --Convert From String

--paths
vek=(dkd++"/ver")   ::FPA--VErsion Keeper
tvk=(dkd++"/verTmp")::FPA--Temporary Version Keeper
dkd="../0Data"      ::FPA--Data Keeper Dir
dkp=(dkd++"/data")  ::FPA--Data Keeper Prefix

--had to be done
nli=pst ""                     ::IOU               --New LIne
nls=(\case 0->wim ()
           i->nli>>((i-1)&nls))::INT->IOU          --New LineS
wnl=(\p->nli>>p>>nli)          ::IOU->IOU          --Wrap In Newlines
aos=cfs>>>(+1)>>>cts           ::STR->STR          --Add One to String
pss=mapM_ pst                  ::STS->IOU          --Print StringS
mco=(\f->map f>>>cnc)          ::(a->[b])->[a]->[b]--Map and Concat
pde=err "decoration failed"
dew::DCR->STS->STR--DEcorate With
dew d s=case (d,s) of (f:r,f':r')->cnc [f',f,dew r r'];([],[])->[];_->pde

--version keeper exists > read version
--doesn't > create one > write 0 to it > wrap 0 in IO
ver=vke>>=bca rvk ww0 ::IOS         --VERsion
vke=vek&exi           ::IOB         --Version Keeper Exists?
bca=(\a b->
  \case True ->a;_->b)::a->a->BOO->a--Bool CAse (if then else)
rvk=rdf vek           ::IOS         --Read Version Keeper
ww0=wwm "0"           ::IOS         --Write to vk and Wrap in monad "0"
wwm=(\s->wvk s>>wim s)::STR->IOS    --Write to vk and Wrap in Monad
wvk=wrf vek           ::STR->IOU    --Write to Version Keeper

--get version > add one to it > write new version to version keeper
upv=a1v>>rev          ::IOU     --UPdate Version
a1v=ver>>=awt         ::IOU     --Add 1 to Version
awt=aos>>>wtv         ::STR->IOU--Add 1 and Write to Temp vk
wtv=wrf tvk           ::STR->IOU--Write to Temp Version keeper
rev=renameFile tvk vek::IOU     --REname temp Vk to vk

cdk=ver>>=apw    ::IOF     --Current Data Keeper
apw=(dkp++)>>>wim::STR->IOF--Append to Prefix and Wrap in monad

ndk=ver>>=aaw::IOF     --Next Data Keeper
aaw=aos>>>apw::STR->IOS--Add 1 Append to prefix and Wrap
