{-# LANGUAGE StrictData #-}
module Teriyaki.Data where

import Teriyaki.Util

data KindF a
  = KTyp
  | KCon Name
  | KArr a a

data TypeF a
  = TUnit
