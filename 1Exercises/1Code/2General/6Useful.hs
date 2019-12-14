{-# LANGUAGE LambdaCase #-} 
module Useful where
import Control.Arrow
import Data.Function
import Types
import Renaming

nli = pst ""                               -- New LIne
pss = mmp pst                              -- Print StringS
nls = \case 0->wim ();i->nli>>((i-1)&nls) -- New LineS
wnl = \p->nli>>p>>nli                      -- Wrap In Newlines
pws = pss>>>wnl                            -- Print Wrapped in NLs Strings
tbd = \i s->(rnt i '\t')++s                -- TaBbeD i times
msf = \x y->[tbd 1 x]++map (tbd 2) y       -- MessageS Format
pwmgl = \x->pws x >> getLine

nli :: IOU               
pws :: STS->IOU          
nls :: INT->IOU          
wnl :: IOU->IOU          
pss :: STS->IOU          
msf :: MSG->MGS->MGS
tbd :: INT->STR->STR
