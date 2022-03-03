{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE RecordWildCards #-}
module Pong.Lang where

import Control.Monad.State
--  ( module Pong.Data
--  , module Pong.Util
--  , FreeIn(..)
--  , Typed
--  , HasArity
--  , annotate
--  , unwindType
--  , insertArgs
--  , typeOf
--  , foldType
--  , isCon
--  , isTCon
--  , arity
--  , returnTypeOf
--  , argTypes
--  , funArgs
--  , constructors
--  , insertDefinition
--  , emptyProgram
----  , printProgram
--  , tUnit
--  , tBool
--  , tInt32
--  , tInt64
--  , tFloat
--  , tDouble
--  , tVar
--  , tArr
--  , tData
----  , tOpaque
--  , (~>)
--  , var
--  , lit
--  , if_
--  , lam
--  , let_
--  , app
--  , call_
--  , op2
--  , case_
--  ) where
--
--import Data.Map.Strict (Map)
--import Data.Tuple (swap)
--import Data.Tuple.Extra (first, dupe)
--import Data.Void
import Control.Newtype.Generics
import Data.List (nub)
import Pong.Data
import Pong.Util

import Data.Function ((&))
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import Debug.Trace
--import TextShow
--import qualified Data.Map.Strict as Map
import qualified Pong.Util.Env as Env

mapRow :: (a -> b) -> Row a v -> Row b v
mapRow f = 
  cata $ \case
    RNil -> rNil
    RVar v -> rVar v
    RExt name expr row -> rExt name (f expr) row

mapRowM :: (Monad m) => (a -> m b) -> Row a v -> m (Row b v)
mapRowM f = 
  cata $ \case
    RNil -> pure rNil
    RVar v -> pure (rVar v)
    RExt name expr row -> rExt name <$> f expr <*> row

class FreeIn a where
  free :: a -> [Int]

instance FreeIn (TypeT g) where
  free =
    nub .
    cata
      (\case
         TVar n -> [n]
         TCon _ ts -> concat ts
         TArr t1 t2 -> t1 <> t2
         TRow r -> free r
         _ -> [])

instance FreeIn (Row (TypeT g) a) where
  free =
    cata $ \case
      RExt _ expr r -> free expr <> r
      _ -> []

instance (FreeIn a) => FreeIn [a] where
  free = concatMap free

instance (Show t, Typed t) => FreeIn (Expr t t a1 a2) where
  free = free . typeOf

instance (FreeIn e) => FreeIn (Environment e) where
  free =
    \case
      Env env -> free (Map.elems env)

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
      LChar {} -> tChar
      LString {} -> tString
      LUnit -> tUnit

instance Typed Op2 where
  typeOf =
    \case
      OEqInt32 -> tInt32 ~> tInt32 ~> tBool
      OAddInt32 -> tInt32 ~> tInt32 ~> tInt32
      OSubInt32 -> tInt32 ~> tInt32 ~> tInt32
      OMulInt32 -> tInt32 ~> tInt32 ~> tInt32
      OAddFloat -> tFloat ~> tFloat ~> tFloat
      OMulFloat -> tFloat ~> tFloat ~> tFloat
      OSubFloat -> tFloat ~> tFloat ~> tFloat
      ODivFloat -> tFloat ~> tFloat ~> tFloat
      OAddDouble -> tDouble ~> tDouble ~> tDouble
      OMulDouble -> tDouble ~> tDouble ~> tDouble
      OSubDouble -> tDouble ~> tDouble ~> tDouble
      ODivDouble -> tDouble ~> tDouble ~> tDouble

instance (Show t, Typed t) => Typed (Row (Expr t t a1 a2) (Label t)) where
  typeOf =
    cata $ \case
      RNil -> tRow rNil
      RVar (t, _) -> typeOf t
      RExt name expr r ->
        let TRow row = project r
         in tRow (rExt name (typeOf expr) row)

instance (Show t, Typed t) => Typed (Expr t t a1 a2) where
  typeOf =
    cata $ \case
      EVar (t, _) -> typeOf t
      ECon (t, _) -> typeOf t
      ELit lit -> typeOf lit
      EIf _ _ e3 -> e3
      ELet _ _ e3 -> e3
      ELam _ args expr -> foldType expr (typeOf . fst <$> args)
      EApp t fun as -> typeOf t
      ECall _ (t, _) as -> foldType1 (drop (length as) (unwindType t))
      EOp2 op _ _ -> returnType op
      ECase _ [] -> error "Empty case statement"
      ECase _ cs -> head (snd <$> cs)
      ERow r -> typeOf r

instance (Typed t) => Typed (Definition (Label t) a) where
  typeOf =
    \case
      Function args (t, _) -> foldType t (typeOf . fst <$> toList args)
      External args (t, _) -> foldType t args
      Constant (t, _) -> typeOf t
      Data {} -> error "Implementation error" 

class HasArity a where
  arity :: a -> Int

instance HasArity Type where
  arity = pred <<< length <<< unwindType

instance (Show t, Typed t) => HasArity (Expr t t a1 a2) where
  arity = arity . typeOf

instance HasArity (Definition r a) where
  arity =
    \case
      Function args _ -> length args
      External args _ -> length args
      _ -> 0

isTCon :: TCon -> Type -> Bool
isTCon con =
  project >>> \case
    TArr {}
      | ArrT == con -> True
    TVar {}
      | VarT == con -> True
    _ -> False

isCon :: Con -> Expr t a0 a1 a2 -> Bool
isCon con =
  project >>> \case
    EVar {}
      | VarE == con -> True
    ELit {}
      | LitE == con -> True
    ELam {}
      | LamE == con -> True
    _ -> False

unwindType :: (Typed t) => t -> [Type]
unwindType =
  typeOf >>>
  para
    (\case
       TArr (t, _) (_, u) -> t : u
       t -> [embed (fst <$> t)])

{-# INLINE returnType #-}
returnType :: (Typed t) => t -> Type
returnType = last <<< unwindType

{-# INLINE argTypes #-}
argTypes :: (Typed t) => t -> [Type]
argTypes = init <<< unwindType

freeVars :: (Eq t) => Expr t a0 a1 a2 -> [Label t]
freeVars =
  cata $ \case
    EVar v -> [v]
    ECon _ -> []
    ELit _ -> []
    EIf e1 e2 e3 -> e1 <> e2 <> e3
    ELet bind e1 e2 -> (e1 <> e2) `without` [bind]
    ELam _ args expr -> expr `without` args
    EApp _ fun args -> fun <> concat args
    ECall _ fun args -> fun : concat args
    EOp2 op e1 e2 -> e1 <> e2
    ECase e1 cs -> e1 <> (cs >>= \(_:vs, expr) -> expr `without` vs)
    ERow row ->
      (`cata` row) $ \case
        RNil -> []
        RVar v -> [v]
        RExt _ expr r -> freeVars expr <> r

toPolyType :: Type -> PolyType
toPolyType =
  cata $ \case
    TUnit -> tUnit
    TBool -> tBool
    TInt32 -> tInt32
    TInt64 -> tInt64
    TFloat -> tFloat
    TDouble -> tDouble
    TChar -> tChar
    TString -> tString
    TCon con ts -> tCon con ts
    TArr t1 t2 -> tArr t1 t2
    TVar n -> tVar n
    TRow row -> tRow (mapRow toPolyType row)

fromPolyType :: [Type] -> PolyType -> Type
fromPolyType ts =
  cata $ \case
    TGen n -> ts !! n
    TUnit -> tUnit
    TBool -> tBool
    TInt32 -> tInt32
    TInt64 -> tInt64
    TFloat -> tFloat
    TDouble -> tDouble
    TChar -> tChar
    TString -> tString
    TCon con ts -> tCon con ts
    TArr t1 t2 -> tArr t1 t2
    TVar n -> tVar n
    TRow row -> tRow (mapRow (fromPolyType ts) row)

{-# INLINE foldType #-}
foldType :: Type -> [Type] -> Type
foldType = foldr tArr

{-# INLINE foldType1 #-}
foldType1 :: [Type] -> Type
foldType1 = foldr1 tArr

{-# INLINE insertArgs #-}
insertArgs :: [(PolyType, Name)] -> Environment PolyType -> Environment PolyType
insertArgs = Env.inserts . (swap <$>)

{-# INLINE emptyProgram #-}
emptyProgram :: Program a
emptyProgram = Program mempty

modifyProgram ::
     (MonadState (Program a) m)
  => (Map Name (Definition (Label Type) a) -> Map Name (Definition (Label Type) a))
  -> m ()
modifyProgram = modify . over Program

insertDef ::
     (MonadState (Program a) m) => Name -> Definition (Label Type) a -> m ()
insertDef = modifyProgram <$$> Map.insert

forEachDef ::
     (MonadState (Program a) m)
  => (Definition (Label Type) a -> m (Definition (Label Type) a))
  -> m ()
forEachDef run = do
  Program defs <- get
  forM_ (Map.keys defs) $ \key -> do
    def <- run (defs ! key)
    insertDef key def

lookupDef :: (MonadState (Program a) m) => Name -> m (Definition (Label Type) a)
lookupDef var = do
  Program defs <- get
  pure (defs ! var)

--{-# INLINE emptyProgram #-}
--emptyProgram :: Program
--emptyProgram = Program 0 mempty
--
--insertDefinition :: Name -> Definition Ast -> Program -> Program
--insertDefinition name def =
--  \case
--    Program {definitions, ..} ->
--      Program {definitions = Map.insert name def definitions, ..}
--
----printProgram :: Program -> IO ()
----printProgram Program {..} = sequence_ (Map.mapWithKey (curry print) definitions)
--{-# INLINE tVar #-}
--tVar :: Int -> Type
--tVar = embed1 TVar
--
--{-# INLINE tArr #-}
--tArr :: Type -> Type -> Type
--tArr = embed2 TArr
--
--infixr 1 `tArr`
--
--{-# INLINE (~>) #-}
--(~>) = tArr
--
--infixr 1 ~>
--
--{-# INLINE tData #-}
--tData :: Name -> Type
--tData = embed1 TData
--
----{-# INLINE tOpaque #-}
----tOpaque :: Type
----tOpaque = embed TOpaque
--
--{-# INLINE var #-}
--var :: Label t -> Expr t a0 a1 a2 a3
--var = embed1 EVar
--
--{-# INLINE lit #-}
--lit :: Literal -> Expr t a0 a1 a2 a3
--lit = embed1 ELit
--
--{-# INLINE if_ #-}
--if_ :: Expr t a0 a1 a2 a3 -> Expr t a0 a1 a2 a3 -> Expr t a0 a1 a2 a3 -> Expr t a0 a1 a2 a3
--if_ = embed3 EIf
--
--{-# INLINE lam #-}
--lam :: [Label t] -> Expr t a0 () a2 a3 -> Expr t a0 () a2 a3
--lam = embed3 ELam () 
--
--{-# INLINE let_ #-}
--let_ :: Label t -> Expr t () a1 a2 a3 -> Expr t () a1 a2 a3 -> Expr t () a1 a2 a3
--let_ = embed4 ELet ()
--
--{-# INLINE app #-}
--app :: Expr t a0 a1 () a3 -> [Expr t a0 a1 () a3] -> Expr t a0 a1 () a3
--app = embed3 EApp ()
--
--{-# INLINE call_ #-}
--call_ :: Label t -> [Expr t a0 a1 a2 ()] -> Expr t a0 a1 a2 ()
--call_ = embed3 ECall ()
--
--{-# INLINE op2 #-}
--op2 :: Op2 -> Expr t a0 a1 a2 a3 -> Expr t a0 a1 a2 a3 -> Expr t a0 a1 a2 a3
--op2 = embed3 EOp2
--
--{-# INLINE case_ #-}
--case_ :: Expr t a0 a1 a2 a3 -> [([Label t], Expr t a0 a1 a2 a3)] -> Expr t a0 a1 a2 a3
--case_ = embed2 ECase
{-# INLINE tUnit #-}
tUnit :: TypeT t
tUnit = embed TUnit

{-# INLINE tBool #-}
tBool :: TypeT t
tBool = embed TBool

{-# INLINE tInt32 #-}
tInt32 :: TypeT t
tInt32 = embed TInt32

{-# INLINE tInt64 #-}
tInt64 :: TypeT t
tInt64 = embed TInt64

{-# INLINE tFloat #-}
tFloat :: TypeT t
tFloat = embed TFloat

{-# INLINE tDouble #-}
tDouble :: TypeT t
tDouble = embed TDouble

{-# INLINE tArr #-}
tArr :: TypeT t -> TypeT t -> TypeT t
tArr = embed2 TArr

infixr 1 `tArr`

{-# INLINE (~>) #-}
(~>) = tArr

infixr 1 ~>

{-# INLINE tCon #-}
tCon :: Name -> [TypeT t] -> TypeT t
tCon = embed2 TCon

{-# INLINE tVar #-}
tVar :: Int -> TypeT t
tVar = embed1 TVar

{-# INLINE tRow #-}
tRow :: Row (TypeT t) Name -> TypeT t
tRow = embed1 TRow

{-# INLINE tChar #-}
tChar :: TypeT t
tChar = embed TChar

{-# INLINE tString #-}
tString :: TypeT t
tString = embed TString

{-# INLINE tGen #-}
tGen :: Int -> PolyType
tGen = embed1 TGen

{-# INLINE rNil #-}
rNil :: Row r v
rNil = embed RNil

{-# INLINE rVar #-}
rVar :: v -> Row r v
rVar = embed1 RVar

{-# INLINE rExt #-}
rExt :: Name -> r -> Row r v -> Row r v
rExt = embed3 RExt

{-# INLINE eOp2 #-}
eOp2 :: Op2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eOp2 = embed3 EOp2

{-# INLINE eVar #-}
eVar :: Label t -> Expr t a0 a1 a2
eVar = embed1 EVar

{-# INLINE eCon #-}
eCon :: Label t -> Expr t a0 a1 a2
eCon = embed1 ECon

{-# INLINE eLit #-}
eLit :: Literal -> Expr t a0 a1 a2
eLit = embed1 ELit

{-# INLINE eIf #-}
eIf :: Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eIf = embed3 EIf

{-# INLINE eLet #-}
eLet :: Label t -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eLet = embed3 ELet

{-# INLINE eLam #-}
eLam :: [Label t] -> Expr t a0 () a2 -> Expr t a0 () a2
eLam = embed3 ELam ()

{-# INLINE eLam_ #-}
eLam_ :: a1 -> [Label t] -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eLam_ = embed3 ELam

{-# INLINE eApp #-}
eApp :: t1 -> Expr t0 t1 a1 a2 -> [Expr t0 t1 a1 a2] -> Expr t0 t1 a1 a2
eApp = embed3 EApp

{-# INLINE eCall #-}
eCall :: Label t -> [Expr t a0 a1 ()] -> Expr t a0 a1 ()
eCall = embed3 ECall ()

{-# INLINE eCase #-}
eCase :: Expr t a0 a1 a2 -> [([Label t], Expr t a0 a1 a2)] -> Expr t a0 a1 a2
eCase = embed2 ECase

{-# INLINE eRow #-}
eRow :: Row (Expr t a0 a1 a2) (Label t) -> Expr t a0 a1 a2
eRow = embed1 ERow
