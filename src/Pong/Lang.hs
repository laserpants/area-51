{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Pong.Lang
  ( module Pong.Data
  , module Pong.Util
  , FreeIn(..)
  , Typed
  , HasArity
  , unwindType
  , insertArgs
  , typeOf
  , foldType
  , isCon
  , isTCon
  , arity
  , returnTypeOf
  , funArgs
  , constructors
  , insertDefinition
  , emptyProgram
  , printProgram
  , tUnit
  , tBool
  , tInt32
  , tInt64
  , tFloat
  , tDouble
  , tVar
  , tArr
  , tData
  , (.->)
  , var
  , lit
  , if_
  , lam
  , let_
  , app
  , op2
  , case_
  , bVar
  , bLit
  , bIf
  , bCall
  , bOp2
  , bCase
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import Data.Tuple.Extra (first)
import Pong.Data
import Pong.Util
import qualified Pong.Util.Env as Env

class FreeIn a where
  free :: a -> [Name]

instance FreeIn (Expr t) where
  free =
    cata $ \case
      EVar (_, name) -> [name]
      ELit lit -> []
      EIf e1 e2 e3 -> e1 <> e2 <> e3
      ELam args expr -> expr `without` (snd <$> args)
      ELet bind e1 e2 -> (e1 <> e2) `without` [snd bind]
      EApp _ fun args -> fun <> concat args
      EOp2 op e1 e2 -> e1 <> e2
      ECase e1 cs -> e1 <> (cs >>= (free . uncurry Clause) . first (fmap snd))

instance FreeIn Body where
  free =
    cata $ \case
      BVar name -> [name]
      BLit lit -> []
      BIf e1 e2 e3 -> e1 <> e2 <> e3
      BCall fun args -> [fun] <> concat args
      BOp2 op e1 e2 -> e1 <> e2
      BCase e1 cs -> e1 <> (cs >>= free . uncurry Clause)

instance FreeIn (Clause Name) where
  free =
    \case
      Clause (_:vs) expr -> expr `without` vs

instance (FreeIn a) => FreeIn (Signature a) where
  free = free . snd . body

instance (FreeIn a) => FreeIn (Map Name (Signature a)) where
  free = concatMap free . Map.elems

class Typed a where
  typeOf :: a -> Type

instance Typed Type where
  typeOf = id

instance Typed Literal where
  typeOf =
    \case
      LBool {} -> tBool
      LInt32 {} -> tInt32
      LInt64 {} -> tInt64
      LFloat {} -> tFloat
      LDouble {} -> tDouble
      LUnit -> tUnit

instance Typed Op2 where
  typeOf =
    \case
      OEqInt32 -> tInt32 .-> tInt32 .-> tBool
      OAddInt32 -> tInt32 .-> tInt32 .-> tInt32
      OSubInt32 -> tInt32 .-> tInt32 .-> tInt32
      OMulInt32 -> tInt32 .-> tInt32 .-> tInt32
      OAddFloat -> tFloat .-> tFloat .-> tFloat
      OMulFloat -> tFloat .-> tFloat .-> tFloat
      OSubFloat -> tFloat .-> tFloat .-> tFloat
      ODivFloat -> tFloat .-> tFloat .-> tFloat
      OAddDouble -> tDouble .-> tDouble .-> tDouble
      OMulDouble -> tDouble .-> tDouble .-> tDouble
      OSubDouble -> tDouble .-> tDouble .-> tDouble
      ODivDouble -> tDouble .-> tDouble .-> tDouble

instance Typed Ast where
  typeOf =
    cata $ \case
      EVar (ty, _) -> ty
      ELit lit -> typeOf lit
      EIf _ _ e3 -> e3
      ELam args expr -> foldType expr (fst <$> args)
      ELet _ _ e2 -> e2
      EApp ty _ _ -> ty
      EOp2 op _ _ -> returnTypeOf op
      ECase _ [] -> error "Empty case statement"
      ECase _ cs -> head (snd <$> cs)

instance Typed (Definition a) where
  typeOf =
    \case
      Function Signature {..} -> foldType (fst body) (fst <$> arguments)
      External Signature {..} -> foldType (fst body) (fst <$> arguments)
      Constant lit -> typeOf lit
      Data name _ -> tData name

class HasArity a where
  arity :: a -> Int

instance HasArity (Definition a) where
  arity =
    \case
      Function Signature {..} -> length arguments
      External Signature {..} -> length arguments
      _ -> 0

instance HasArity Type where
  arity = pred <<< length <<< unwindType

isTCon :: TCon -> Type -> Bool
isTCon con =
  project >>> \case
    TArr {}
      | ArrT == con -> True
    TVar {}
      | VarT == con -> True
    _ -> False

isCon :: Con -> Expr t -> Bool
isCon con =
  project >>> \case
    EVar {}
      | VarE == con -> True
    ELit {}
      | LitE == con -> True
    ELam {}
      | LamE == con -> True
    _ -> False

unwindType :: Type -> [Type]
unwindType =
  para $ \case
    TArr (t, _) (_, u) -> t : u
    t -> [embed (fst <$> t)]

returnTypeOf :: (Typed t) => t -> Type
returnTypeOf = last <<< unwindType <<< typeOf

funArgs :: Definition a -> [(Type, Name)]
funArgs =
  \case
    Function Signature {..} -> arguments
    External Signature {..} -> arguments
    _ -> []

constructors :: Definition a -> [Constructor]
constructors =
  \case
    Data _ cs -> cs
    _ -> []

{-# INLINE foldType #-}
foldType :: Type -> [Type] -> Type
foldType = foldr tArr

{-# INLINE insertArgs #-}
insertArgs :: [(Type, Name)] -> TypeEnv -> TypeEnv
insertArgs = Env.inserts . (swap <$>)

{-# INLINE emptyProgram #-}
emptyProgram :: Program
emptyProgram = Program 0 mempty

insertDefinition :: Name -> Definition Body -> Program -> Program
insertDefinition name def =
  \case
    Program {definitions, ..} ->
      Program {definitions = Map.insert name def definitions, ..}

printProgram :: Program -> IO ()
printProgram Program {..} = sequence_ (Map.mapWithKey (curry print) definitions)

{-# INLINE tUnit #-}
tUnit :: Type
tUnit = embed TUnit

{-# INLINE tBool #-}
tBool :: Type
tBool = embed TBool

{-# INLINE tInt32 #-}
tInt32 :: Type
tInt32 = embed TInt32

{-# INLINE tInt64 #-}
tInt64 :: Type
tInt64 = embed TInt64

{-# INLINE tFloat #-}
tFloat :: Type
tFloat = embed TFloat

{-# INLINE tDouble #-}
tDouble :: Type
tDouble = embed TDouble

{-# INLINE tVar #-}
tVar :: Int -> Type
tVar = embed1 TVar

{-# INLINE tArr #-}
tArr :: Type -> Type -> Type
tArr = embed2 TArr

infixr 1 `tArr`

{-# INLINE (.->) #-}
(.->) = tArr

infixr 1 .->

{-# INLINE tData #-}
tData :: Name -> Type
tData = embed1 TData

{-# INLINE var #-}
var :: (t, Name) -> Expr t
var = embed1 EVar

{-# INLINE lit #-}
lit :: Literal -> Expr t
lit = embed1 ELit

{-# INLINE if_ #-}
if_ :: Expr t -> Expr t -> Expr t -> Expr t
if_ = embed3 EIf

{-# INLINE lam #-}
lam :: [(t, Name)] -> Expr t -> Expr t
lam = embed2 ELam

{-# INLINE let_ #-}
let_ :: (t, Name) -> Expr t -> Expr t -> Expr t
let_ = embed3 ELet

{-# INLINE app #-}
app :: t -> Expr t -> [Expr t] -> Expr t
app = embed3 EApp

{-# INLINE op2 #-}
op2 :: Op2 -> Expr t -> Expr t -> Expr t
op2 = embed3 EOp2

{-# INLINE case_ #-}
case_ :: Expr t -> [([(t, Name)], Expr t)] -> Expr t
case_ = embed2 ECase

{-# INLINE bVar #-}
bVar :: Name -> Body
bVar = embed1 BVar

{-# INLINE bLit #-}
bLit :: Literal -> Body
bLit = embed1 BLit

{-# INLINE bIf #-}
bIf :: Body -> Body -> Body -> Body
bIf = embed3 BIf

{-# INLINE bCall #-}
bCall :: Name -> [Body] -> Body
bCall = embed2 BCall

{-# INLINE bOp2 #-}
bOp2 :: Op2 -> Body -> Body -> Body
bOp2 = embed3 BOp2

{-# INLINE bCase #-}
bCase :: Body -> [(Names, Body)] -> Body
bCase = embed2 BCase
