{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Pong.Lang where

import Control.Monad.State
import Control.Newtype.Generics
import Data.Function ((&))
import Data.List (nub)
import Debug.Trace
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import Pong.Data
import Pong.Util (Name, Map, (!), (<$$>), (<#>), (<<<), (>>>), cata, para, project, embed, without, embed, embed1, embed2, embed3, embed4)
import qualified Pong.Util.Env as Env

mapRow :: (e -> f) -> Row e r -> Row f r
mapRow f =
  cata $ \case
    RNil -> rNil
    RVar v -> rVar v
    RExt name expr row -> rExt name (f expr) row

mapRowM :: (Monad m) => (e -> m f) -> Row e r -> m (Row f r)
mapRowM f =
  cata $ \case
    RNil -> pure rNil
    RVar v -> pure (rVar v)
    RExt name expr row -> rExt name <$> f expr <*> row

canonRow :: Row e r -> Row e r
canonRow = uncurry (flip foldRow) . unwindRow

canonTypeRows :: TypeT t -> TypeT t
canonTypeRows =
  cata $ \case
    TRow r -> tRow (canonRow r)
    t -> embed t

foldRow :: Row e r -> Map Name [e] -> Row e r
foldRow = Map.foldrWithKey (flip . foldr . rExt)

unwindRow :: Row e r -> (Map Name [e], Row e r)
unwindRow row = (foldr (uncurry (Map.insertWith (<>))) mempty fields, leaf)
  where
    fields =
      (`para` row) $ \case
        RExt label ty (_, rest) -> (label, [ty]) : rest
        _ -> []
    leaf =
      (`cata` row) $ \case
        RExt _ _ r -> r
        t -> embed t

splitRow :: Name -> Row e r -> (e, Row e r)
splitRow a row =
  ( e
  , canonRow
      (foldRow k $
       case es of
         [] -> Map.delete a m
         _ -> Map.insert a es m))
  where
    Just (e:es) = Map.lookup a m
    (m, k) = unwindRow row

class FreeIn a where
  free :: a -> [Int]

instance FreeIn (TypeT t) where
  free =
    nub .
    cata
      (\case
         TVar n -> [n]
         TCon _ ts -> concat ts
         TArr t1 t2 -> t1 <> t2
         TRow r -> free r
         _ -> [])

instance FreeIn (Row (TypeT t) Int) where
  free =
    cata $ \case
      RVar v -> [v]
      RExt _ expr r -> free expr <> r
      _ -> []

instance (FreeIn a) => FreeIn [a] where
  free = concatMap free

instance (Typed t) => FreeIn (Expr t t a1 a2) where
  free = free . typeOf

instance (FreeIn e) => FreeIn (Environment e) where
  free =
    \case
      Environment env -> free (Map.elems env)

class Typed a where
  typeOf :: a -> Type

instance Typed Type where
  typeOf = id

instance Typed Prim where
  typeOf =
    \case
      PBool {} -> tBool
      PInt {} -> tInt
      PFloat {} -> tFloat
      PDouble {} -> tDouble
      PChar {} -> tChar
      PString {} -> tString
      PUnit -> tUnit

instance (Typed t) => Typed (Op2 t) where
  typeOf =
    \case
      Op2 _ t -> typeOf t

instance (Typed t) => Typed (Row (Expr t t a1 a2) (Label t)) where
  typeOf =
    cata $ \case
      RNil -> tRow rNil
      RVar (t, _) -> typeOf t
      RExt name expr r ->
        let TRow row = project r
         in tRow (rExt name (typeOf expr) row)

instance (Typed t) => Typed (Expr t t a1 a2) where
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
      EOp2 (Op2 _ t) _ _ -> returnType t
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

instance (Typed t) => HasArity t where
  arity = pred <<< length <<< unwindType

instance HasArity (Definition d a) where
  arity =
    \case
      Function args _ -> length args
      External args _ -> length args
      _ -> 0

leastFree :: (FreeIn t) => [t] -> Int
leastFree ts =
  case free =<< ts of
    [] -> 0
    vs -> succ (maximum vs)

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
    ERow {}
      | RowE == con -> True
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
    TInt -> tInt
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
    TInt -> tInt
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

programToTypeEnv :: Program p -> Environment PolyType
programToTypeEnv p = Env.fromList (toPolyType . typeOf <$$> Map.toList (unpack p))

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

modifyProgram ::
     (MonadState (Program a) m)
  => (Map Name (Definition (Label Type) a) -> Map Name (Definition (Label Type) a))
  -> m ()
modifyProgram = modify . over Program

modifyProgramM ::
     (MonadState (Program a) m)
  => (Map Name (Definition (Label Type) a) -> m (Map Name (Definition (Label Type) a)))
  -> m ()
modifyProgramM f = do
  Program p <- get
  x <- f p
  put (Program x)
  --p <- Control.Newtype.Generics.unpack <$> get
  --let zzz = modify f p
  -- modify . over Program

insertDef ::
     (MonadState (Program a) m) => Name -> Definition (Label Type) a -> m ()
insertDef = modifyProgram <$$> Map.insert

--updateDef ::
--     (MonadState (Program a) m)
--  => (Definition (Label Type) a -> Maybe (Definition (Label Type) a))
--  -> Name
--  -> m ()
--updateDef = modifyProgram <$$> Map.update 

forEachDef ::
     (MonadState (Program a) m)
  => (Definition (Label Type) a -> m (Definition (Label Type) a))
  -> m ()
forEachDef run = do
  Program defs <- get
  forM_ (Map.keys defs) $ \key -> do
    Program defs <- get
    def <- run (defs ! key)
    insertDef key def

lookupDef :: (MonadState (Program a) m) => Name -> m (Definition (Label Type) a)
lookupDef key = do
  Program defs <- get
  pure (defs ! key)

{-# INLINE tUnit #-}
tUnit :: TypeT t
tUnit = embed TUnit

{-# INLINE tBool #-}
tBool :: TypeT t
tBool = embed TBool

{-# INLINE tInt #-}
tInt :: TypeT t
tInt = embed TInt

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
tRow :: Row (TypeT t) Int -> TypeT t
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
rNil :: Row e r
rNil = embed RNil

{-# INLINE rVar #-}
rVar :: r -> Row e r
rVar = embed1 RVar

{-# INLINE rExt #-}
rExt :: Name -> e -> Row e r -> Row e r
rExt = embed3 RExt

{-# INLINE eOp2 #-}
eOp2 :: Op2 t -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eOp2 = embed3 EOp2

{-# INLINE eVar #-}
eVar :: Label t -> Expr t a0 a1 a2
eVar = embed1 EVar

{-# INLINE eCon #-}
eCon :: Label a0 -> Expr t a0 a1 a2
eCon = embed1 ECon

{-# INLINE eLit #-}
eLit :: Prim -> Expr t a0 a1 a2
eLit = embed1 ELit

{-# INLINE eIf #-}
eIf :: Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eIf = embed3 EIf

{-# INLINE eLet #-}
eLet :: Label t -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eLet = embed3 ELet

{-# INLINE eLam #-}
eLam :: a1 -> [Label t] -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eLam = embed3 ELam

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

{-# INLINE oAddInt #-}
oAddInt :: Op2 Type
oAddInt = Op2 OAdd (tInt ~> tInt ~> tInt)

{-# INLINE oSubInt #-}
oSubInt :: Op2 Type
oSubInt = Op2 OSub (tInt ~> tInt ~> tInt)

{-# INLINE oMulInt #-}
oMulInt :: Op2 Type
oMulInt = Op2 OMul (tInt ~> tInt ~> tInt)

{-# INLINE oEqInt #-}
oEqInt :: Op2 Type
oEqInt = Op2 OEq (tInt ~> tInt ~> tBool)
