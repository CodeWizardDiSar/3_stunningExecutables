{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-} 
{-# LANGUAGE ConstraintKinds #-}
module ExsToFcs where
import Control.Arrow
import Types
import General
import Renaming

etf = map etl >>> cnc
etl =
  \case Don (n,nu,e)    -> cnc ["d ",n," ",nu," ",tst e,"\n"]
        Mis (n,nu,e)    -> cnc ["m ",n," ",nu," ",tst e,"\n"]
        Tdo (n,nu,e) da -> cnc ["t ",n," ",nu," ",tst e," ",tst da,"\n"]
etf :: EXS -> FCS
etl :: EXR -> LIN

instance TST HEN where tst = \case Nng -> "_"; Idd e -> e 
instance TST DAT where tst = \(d,m,y) -> cnc [tst d,"/",tst m,"/",tst y]
instance TST INT where tst = cts
