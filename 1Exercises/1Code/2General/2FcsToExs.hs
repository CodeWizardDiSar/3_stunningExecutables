{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-} 
module FcsToExs where
import Prelude hiding (fst,sin)
import Data.List.Split
import Control.Arrow
import Types 
import General
import Renaming

exs = cdk>>=rdf>>=(fte>>>wim)
fte = sin>>>map lte
lte = siw>>> \case ["d",sn,nu,en]   ->Don (sn,nu,fst en)
                   ["m",sn,nu,en]   ->Mis (sn,nu,fst en)
                   ["t",sn,nu,en,da]->Tdo (sn,nu,fst en)$fst da
                   _                ->pem "Line To Exercise"
fte :: FCS->EXS
lte :: LIN->EXR

--from string
instance FST HEN where fst= \case "_"->Nng;en->Idd en
instance FST DAT where
  fst=splitOn "/">>> \case [d,m,y]->(fst d,fst m,fst y);_->pem "Date"
instance FST INT where fst=cfs
