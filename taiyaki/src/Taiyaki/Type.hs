{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Taiyaki.Type where

import Control.Newtype.Generics (Newtype, over2, pack, unpack)
import Data.Map ((!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Taiyaki.Data
import Taiyaki.Data.Cons
import Taiyaki.Util

type SubMap = Map MonoIndex MonoType

newtype Substitution = Substitution SubMap

compose :: Substitution -> Substitution -> Substitution
compose = over2 Substitution fun
  where
    fun s1 s2 = apply (Substitution s1) s2 `Map.union` s1

mapsTo :: MonoIndex -> MonoType -> Substitution
mapsTo n = pack <<< Map.singleton n

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

substitute :: SubMap -> MonoType -> MonoType
substitute sub =
  cata
    ( \case
        TVar k n     -> fromMaybe (tVar k n) (sub !? n)
        TUnit        -> tUnit
        TBool        -> tBool
        TInt         -> tInt
        TBig         -> tBig
        TNat         -> tNat
        TFloat       -> tFloat
        TDouble      -> tDouble
        TChar        -> tChar
        TString      -> tString
        TVoid        -> tVoid
        TList t      -> tList t
        TCon k n     -> tCon k n
        TApp k t1 t2 -> tApp k t1 t2
        TArr t1 t2   -> tArr t1 t2
        TRec t       -> tRec t
        TNil         -> tNil
        TExt n t1 t2 -> tExt n t1 t2
    )

{- ORMOLU_ENABLE -}

class Substitutable a where
  apply :: Substitution -> a -> a

instance Substitutable MonoType where
  apply = substitute . unpack

instance (Substitutable a) => Substitutable [a] where
  apply = fmap . apply

instance (Substitutable a) => Substitutable (Map k a) where
  apply = fmap . apply

instance Substitutable Void where
  apply = const id

-------------------------------------------------------------------------------

-- Substitution
instance Semigroup Substitution where
  (<>) = compose

deriving instance Monoid Substitution

deriving instance Show Substitution

deriving instance Eq Substitution

deriving instance Ord Substitution

deriving instance Generic Substitution

instance Newtype Substitution
