{-# LANGUAGE LambdaCase #-} 
module Useful where
import Types
import Control.Arrow
import Data.Function
import Renaming

nli = pst ""                               -- New LIne
pss = mmp pst                              -- Print StringS
nls = \case 0 ->wim ();i->nli>>((i-1)&nls) -- New LineS
wnl = \p->nli>>p>>nli                      -- Wrap In Newlines
pws = pss>>>wnl                            -- Print Wrapped in NLs Strings
nli :: IOU               
pws :: STS->IOU          
nls :: INT->IOU          
wnl :: IOU->IOU          
pss :: STS->IOU          
