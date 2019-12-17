{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-} 
{-# LANGUAGE ConstraintKinds #-}
module ExsToFcs where
import Prelude hiding (Nothing,and)
import Types
import General
import Renaming

etf = forEach etl`and`glue
etl =
  \case Don (n,nu,e)   ->glue ["d ",n," ",nu," ",tst e,"\n"]
        Mis (n,nu,e)   ->glue ["m ",n," ",nu," ",tst e,"\n"]
        Tdo (n,nu,e) da->glue ["t ",n," ",nu," ",tst e," ",tst da,"\n"]
etf :: EXS->FCS
etl :: EXR->LIN

instance TST HEN where tst = \case Nothing->"_"; Indeed e->e 
instance TST DAT where tst = \(d,m,y)->glue [tst d,"/",tst m,"/",tst y]
instance TST INT where tst = convertToString
