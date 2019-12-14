{-# LANGUAGE LambdaCase #-} 
module General where
import Control.Monad
import Data.Function
import Control.Arrow
import System.Directory
import System.Process
import Types
import Paths
import Renaming
import Useful

--version keeper exists > read version
--doesn't > create one > write 0 to it > wrap 0 in IO
ver = vke>>= \case True->rvk;_->ww0 -- VERsion
vke = vek&fex                        -- Version Keeper Exists?
rvk = rdf vek                        -- Read Version Keeper
ww0 = wvk "0">>wim "0"               -- Write to vk and Wrap in monad "0"
wvk = wrf vek                        -- Write to Version Keeper
ver :: IOS     
vke :: IOB     
rvk :: IOS     
ww0 :: IOS     
wvk :: STR->IOU

--get version > add one > write to temp > rename temp
upv = ver>>=(aos>>>wtv)>>rnv -- UPdate Version
aos = cfs>>>(+1)>>>cts       -- Add One to String
wtv = wrf tvk                -- Write to Temp Version keeper
rnv = rfi tvk vek            -- REname temp Vk to vk
upv :: IOU     
aos :: STR->STR
wtv :: STR->IOU
rnv :: IOU     

cdk = ver>>=((dkp++)>>>wim)       -- Current Data Keeper
ndk = ver>>=(aos>>>(dkp++)>>>wim) -- Next Data Keeper
wnk = \s->ndk>>=(wrf>>> \f->f s)  -- Write to Next data Keeper
cdk :: IOF     
ndk :: IOF     
wnk :: STR->IOU
