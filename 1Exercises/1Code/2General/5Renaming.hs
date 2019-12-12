{-# LANGUAGE LambdaCase #-} 
module Renaming where
import System.Directory
import Types

rdf=readFile      -- ReaD File
cns=readFile      -- CoNtentS
wrf=writeFile     -- WRite File
rfi=renameFile    -- Rename FIle
pst=putStrLn      -- Print STring
wim=return        -- Wrap In Monad
cnc=concat        -- CoNCat
fex=doesFileExist -- File EXists?
cfs=read          -- Convert From String
cts=show          -- Convert To String
pem=error         -- Print Error Message
sin=lines
wrs=words
mmp=mapM_
rep=replicate

rdf::PAT->IOS     
cns::PAT->IOS     
wrf::PAT->STR->IOU
rfi::PAT->PAT->IOU
pst::STR->IOU     
wim::MON m=>a->m a
cnc::[[a]]->[a]   
fex::PAT->IOB     
cfs::STR->INT     
cts::INT->STR     
pem::STR->a       
sin::STR->LNS
wrs::STR->WDS
mmp::(Monad m, Foldable t)=>(a->m b)->t a->m ()
