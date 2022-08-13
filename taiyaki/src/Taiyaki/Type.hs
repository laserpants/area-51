module Taiyaki.Type where

import Taiyaki.Data
import Data.Map.Strict (Map)

newtype Substitution
  = Substitution (Map MonoIndex MonoType)

