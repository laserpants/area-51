{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Pong where

import Control.Arrow ((>>>))
import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Char (isAlphaNum, isUpper)
import Data.Fix
import Data.Foldable (foldMap', foldrM)
import Data.Function (on)
import Data.Int (Int32, Int64)
import Data.List (sortBy, groupBy, intersperse)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, (<|))
import Data.Map.Strict (Map, (!))
import Data.Maybe (fromMaybe)
import Data.Set (Set, member)
import Data.Text (Text)
import Data.Tuple.Extra
-- import Debug.Trace
import Prelude hiding (map)
import TextShow
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

type List1 = NonEmpty

type Name = Text

data Type
  = TCon !Name ![Type]
  | TVar !Int
  | RNil
  | RExt !Name ![Type] !Type
  deriving (Show, Eq, Ord, Read)

data Scheme = Forall !(Set Int) !Type
  deriving (Show, Eq, Ord, Read)

data Prim
  = PUnit
  | PBool   !Bool
  | PInt32  !Int32
  | PInt64  !Int64
  | PFloat  !Float
  | PDouble !Double
  | PChar   !Char
  | PString !Text
  deriving (Show, Eq, Ord, Read)

data Op1
  = ONot
  | ONeg
  deriving (Show, Eq, Ord, Read)

data Op2
  = OEq
  | ONEq
  | OLt
  | OGt
  | OLtE
  | OGtE
  | OAdd
  | OSub
  | OMul
  | ODiv
  | OOr
  | OAnd
  deriving (Show, Eq, Ord, Read)

data Label t = Label !t !Name
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data Clause t = Clause !(List1 (Label t)) !(Expr t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data Focus t = Focus !(Label t) !(Label t) !(Label t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data Expr t
  = EVar !(Label t)
  | ELit !Prim
  | EIf  !(Expr t) !(Expr t) !(Expr t)
  | ELet !(List1 (Label t, Expr t)) !(Expr t)
  | EApp !t !(Expr t) !(List1 (Expr t))
  | ELam !(List1 (Label t)) !(Expr t)
  | EOp1 !(t, Op1) !(Expr t)
  | EOp2 !(t, Op2) !(Expr t) !(Expr t)
  | EPat !(Expr t) !(List1 (Clause t))
  | ENil
  | EExt !Name !(Expr t) !(Expr t)
  | ERes !(Focus t) !(Expr t) !(Expr t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

newtype Substitution = Sub { unSub :: Map Int Type }
  deriving (Show, Eq, Ord)

instance Semigroup Substitution where
  s1 <> s2 = Sub (s2' <> unSub s1) where s2' = apply s1 (unSub s2)

instance Monoid Substitution where
  mempty = Sub mempty

data Constraint = Constraint !Type !Type
  deriving (Show, Eq, Ord)

type TypeEnv = Environment Scheme

class Typed t where
  typeOf :: t -> Type

instance Typed Type where
  typeOf = id

instance Typed Prim where
  typeOf =
    \case
      PBool{}   -> tBool
      PInt32{}  -> tInt32
      PInt64{}  -> tInt64
      PFloat{}  -> tFloat
      PDouble{} -> tDouble
      PChar{}   -> tChar
      PString{} -> tString
      PUnit     -> tUnit

instance (Typed t) => Typed (Label t) where
  typeOf (Label t _) = typeOf t

instance (Typed t) => Typed (Clause t) where
  typeOf (Clause _ expr) = typeOf expr

instance (Typed t) => Typed (Expr t) where
  typeOf =
    \case
      EVar var        -> typeOf var
      ELit prim       -> typeOf prim
      EIf _ _ e       -> typeOf e
      ELet _ e        -> typeOf e
      EApp t _ _      -> typeOf t
      ELam lls e      -> foldType (typeOf e) (typeOf <$> NonEmpty.toList lls)
      EOp1 (t, _) _   -> typeOf t
      EOp2 (t, _) _ _ -> typeOf t
      EPat _ (c:|_)   -> typeOf c
      ENil            -> RNil
      ERes _ _ e      -> typeOf e
      EExt n e1 e2    -> normalizeRow (RExt n [typeOf e1] (typeOf e2))

class Substitutable s where
  apply :: Substitution -> s -> s
  tvars :: s -> Set Int

instance (Substitutable s) => Substitutable [s] where
  apply = fmap . apply
  tvars = Set.unions . fmap tvars

instance (Substitutable s) => Substitutable (List1 s) where
  apply = fmap . apply
  tvars = Set.unions . fmap tvars

instance (Substitutable s, Substitutable t) => Substitutable (s, t) where
  apply sub (a, b) = (apply sub a, apply sub b)
  tvars (a, b) = tvars a <> tvars b

instance (Substitutable s) => Substitutable (Map k s) where
  apply = fmap . apply
  tvars = Set.unions . fmap tvars

instance (Substitutable s) => Substitutable (Environment s) where
  apply = inEnv . fmap . apply
  tvars = Set.unions . fmap tvars . unEnv

instance (Substitutable s) => Substitutable (Label s) where
  apply = fmap . apply
  tvars = Set.unions . fmap tvars

instance (Typed s, Substitutable s) => Substitutable (Clause s) where
  apply sub (Clause labels expr) =
    Clause (apply sub labels) (apply sub expr)
  tvars (Clause labels expr) =
    tvars labels <> tvars expr

instance Substitutable Type where
  apply sub =
    \case
      TCon name ts -> TCon name (apply sub ts)
      TVar t       -> fromMaybe (TVar t) (Map.lookup t (unSub sub))
      RNil         -> RNil
      RExt n ts t  -> RExt n (apply sub ts) (apply sub t)
  tvars =
    \case
      TCon _ ts    -> tvars ts
      TVar t       -> Set.singleton t
      RNil         -> mempty
      RExt _ ts t  -> tvars ts <> tvars t

instance Substitutable Scheme where
  apply sub (Forall vs t) =
    Forall vs (apply (foldr removeSub sub vs) t)
  tvars (Forall vs t) =
    tvars t `Set.difference` vs

instance Substitutable Constraint where
  apply sub (Constraint t1 t2) = Constraint (apply sub t1) (apply sub t2)
  tvars (Constraint t1 t2)     = tvars t1 `Set.union` tvars t2

instance (Typed s, Substitutable s) => Substitutable (Expr s) where
  apply = fmap . apply
  tvars = foldMap' (maybeSingleton . unTVar . typeOf)

mapsTo :: Int -> Type -> Substitution
mapsTo = Sub <$$> Map.singleton

substitutions :: [(Int, Type)] -> Substitution
substitutions = Sub . Map.fromList

zipSub :: [Int] -> [Type] -> Substitution
zipSub = substitutions <$$> zip

inSub :: (Map Int Type -> Map Int Type) -> Substitution -> Substitution
inSub f = Sub . f . unSub

removeSub :: Int -> Substitution -> Substitution
removeSub = inSub . Map.delete

class Bound b where
  bound :: b -> Set Name

instance Bound b => Bound [b] where
  bound = Set.unions . fmap bound

instance Bound b => Bound (List1 b) where
  bound = bound . NonEmpty.toList

instance Bound (Label t) where
  bound (Label _ var) = Set.singleton var

instance (Bound b) => Bound (b, Expr t) where
  bound (b, _) = bound b

instance Bound (Focus b) where
  bound (Focus _ l1 l2) = bound l1 <> bound l2

class FreeIn f t where
  free :: f -> Set (t, Name)

instance (Ord t, FreeIn f t) => FreeIn [f] t where
  free = Set.unions . fmap free

instance (Ord t, FreeIn f t) => FreeIn (List1 f) t where
  free = free . NonEmpty.toList

instance (Ord t) => FreeIn (Label t) t where
  free _ = mempty

withoutVars :: Set (t, Name) -> Set Name ->  Set (t, Name)
withoutVars s1 s2 = Set.filter (\(_, name) -> name `notElem` s2) s1

instance FreeIn (Expr f) t => FreeIn (Clause f) t where
  free (Clause lls e1) = free e1 `withoutVars` bound lls

instance (Ord t, FreeIn f t, FreeIn e t) => FreeIn (f, e) t where
  free (a, b) = free a <> free b

instance (Ord t) => FreeIn (Expr t) t where
  free =
    \case
      EVar (Label t v) -> Set.singleton (t, v)
      ELit _           -> mempty
      EIf e1 e2 e3     -> free e1 <> free e2 <> free e3
      ELet vs e1       -> (free e1 <> free vs) `withoutVars` bound vs
      EApp _ e1 es     -> free e1 <> free es
      ELam lls e1      -> free e1 `withoutVars` bound lls
      EOp1 _ e1        -> free e1
      EOp2 _ e1 e2     -> free e1 <> free e2
      EPat e1 cs       -> free e1 <> free cs
      ENil             -> mempty
      EExt _ e1 e2     -> free e1 <> free e2
      ERes f e1 e2     -> free e1 <> (free e2 `withoutVars` bound f)

op1Type :: Op1 -> Scheme
op1Type =
  \case
    ONot -> scheme [] (tBool ~> tBool)
    ONeg -> scheme [0] (TVar 0 ~> TVar 0)

op2Type :: Op2 -> Scheme
op2Type =
  \case
    OEq  -> scheme [0] (TVar 0 ~> TVar 0 ~> tBool)
    ONEq -> scheme [0] (TVar 0 ~> TVar 0 ~> tBool)
    OLt  -> scheme [0] (TVar 0 ~> TVar 0 ~> tBool)
    OGt  -> scheme [0] (TVar 0 ~> TVar 0 ~> tBool)
    OLtE -> scheme [0] (TVar 0 ~> TVar 0 ~> tBool)
    OGtE -> scheme [0] (TVar 0 ~> TVar 0 ~> tBool)
    OAdd -> scheme [0] (TVar 0 ~> TVar 0 ~> TVar 0)
    OSub -> scheme [0] (TVar 0 ~> TVar 0 ~> TVar 0)
    OMul -> scheme [0] (TVar 0 ~> TVar 0 ~> TVar 0)
    ODiv -> scheme [0] (TVar 0 ~> TVar 0 ~> TVar 0)
    OOr  -> scheme [] (tBool ~> tBool ~> tBool)
    OAnd -> scheme [] (tBool ~> tBool ~> tBool)

tUnit :: Type
tUnit = TCon "()" []

tInt32 :: Type
tInt32 = TCon "Int32" []

tInt64 :: Type
tInt64 = TCon "Int64" []

tBool :: Type
tBool = TCon "Bool" []

tFloat :: Type
tFloat = TCon "Float" []

tDouble :: Type
tDouble = TCon "Double" []

tChar :: Type
tChar = TCon "Char" []

tString :: Type
tString = TCon "String" []

tArr :: Type -> Type -> Type
tArr t1 t2 = TCon "->" [t1, t2]

infixr 1 `tArr`

scheme :: [Int] -> Type -> Scheme
scheme = Forall . Set.fromList

mono :: Type -> Scheme
mono = Forall mempty

foldType :: (Foldable f) => Type -> f Type -> Type
foldType = foldr tArr

foldRow :: Type -> Map Name [Type] -> Type
foldRow = Map.foldrWithKey RExt

unfoldRow :: Type -> (Type, Map Name [Type])
unfoldRow =
  \case
    RExt name ts t -> second (Map.insertWith (<>) name ts) (unfoldRow t)
    t              -> (t, mempty)

removeField :: Name -> Map Name [a] -> Map Name [a]
removeField = Map.update $ \case
  (_:a:as) -> Just (a:as)
  (_:_)    -> Nothing
  []       -> Nothing

normalizeRow :: Type -> Type
normalizeRow = uncurry foldRow . unfoldRow

normalizeAllRows :: Type -> Type
normalizeAllRows =
  \case
    TCon name ts   -> TCon name (normalizeAllRows <$> ts)
    TVar var       -> TVar var
    RNil           -> RNil
    RExt name ts t -> normalizeRow (RExt name (normalizeAllRows <$> ts) t)

unLabel :: Label t -> (Name, t)
unLabel (Label t name) = (name, t)

unfoldType :: Type -> List1 Type
unfoldType =
  \case
    TCon "->" [t1, t2] -> t1 <| unfoldType t2
    t                  -> NonEmpty.singleton t

arity :: Type -> Int
arity t = NonEmpty.length (unfoldType t) - 1

-- applyN :: Int -> Type -> Type
-- applyN n = foldl1' tArr . NonEmpty.drop n . unfoldType
--

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
{-# INLINE (<$$>) #-}
(<$$>) = fmap . fmap

infixr 8 <$$>

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
{-# INLINE (<$$$>) #-}
(<$$$>) = fmap . fmap . fmap

infixr 8 <$$$>

(~>) :: Type -> Type -> Type
{-# INLINE (~>) #-}
(~>) = tArr

infixr 1 ~>

--

newtype Environment a = Env { unEnv :: Map Name a }
  deriving (Show, Eq, Ord, Semigroup, Monoid)

inEnv :: (Map Name a -> Map Name b) -> Environment a -> Environment b
inEnv f = Env . f . unEnv

envEmpty :: Environment a
envEmpty = mempty

envInsert :: Name -> a -> Environment a -> Environment a
envInsert = inEnv <$$> Map.insert

envInserts :: (Foldable f) => f (Name, a) -> Environment a -> Environment a
envInserts = flip (foldr (uncurry envInsert))

envFromList :: (Foldable f) => f (Name, a) -> Environment a
envFromList = (`envInserts` mempty)

envLookup :: Name -> Environment a -> Maybe a
envLookup name = Map.lookup name . unEnv

envElems :: Environment a -> [a]
envElems = Map.elems . unEnv

insertArgs :: (Functor f, Foldable f) => f (Label Type) -> TypeEnv -> TypeEnv
insertArgs = envInserts . (unLabel <$>) . (mono <$$>)

unfoldRec :: Expr t -> (Expr t, Map Name [Expr t])
unfoldRec =
  \case
    EExt name es e -> second (Map.insertWith (<>) name [es]) (unfoldRec e)
    e              -> (e, mempty)

zeroIndexed :: Expr Type -> Expr Type
zeroIndexed expr = apply (zipSub vs ts) expr
  where
    vs = Set.toList (foldr (maybeInsert . unTVar) mempty expr)
    ts = TVar <$> [ 0 .. ]

data InferState = InferState { count :: !Int, substitution :: !Substitution }
  deriving (Show, Eq, Ord)

initialState :: InferState
initialState = InferState 0 mempty

overSubstitution :: (Substitution -> Substitution) -> InferState -> InferState
overSubstitution f InferState{..} = InferState { substitution = f substitution, .. }

overCount :: (Int -> Int) -> InferState -> InferState
overCount f InferState{..} = InferState { count = f count, .. }

newtype Infer a = Infer { unInfer :: ReaderT TypeEnv (StateT InferState (Except String)) a }
  deriving (Functor, Applicative, Monad, MonadReader TypeEnv, MonadState InferState, MonadError String)

modifySubstitution :: (Substitution -> Substitution) -> Infer ()
modifySubstitution = modify . overSubstitution

modifyCount :: (Int -> Int) -> Infer ()
modifyCount = modify . overCount

bind :: Int -> Type -> Infer Substitution
bind v t
  | TVar v == t        = pure mempty
  | v `member` tvars t = throwError "Infinite type"
  | otherwise          = pure (v `mapsTo` t)

bindType :: Int -> Type -> Infer ()
bindType v t = do
  sub <- bind v t
  modifySubstitution (sub <>)

unifyAll :: [Type] -> [Type] -> Infer ()
unifyAll [] [] = pure ()
unifyAll (t1 : ts1) (t2 : ts2) = do
  unifyTypes t1 t2
  sub <- gets substitution
  unifyAll (apply sub ts1) (apply sub ts2)
unifyAll _ _ = throwError "Implementation error"

unifyTypes :: Type -> Type -> Infer ()
unifyTypes (TVar t) t2 = bindType t t2
unifyTypes t1 (TVar t) = bindType t t1
unifyTypes (TCon c1 ts1) (TCon c2 ts2)
  | c1 == c2  = unifyAll ts1 ts2
  | otherwise = throwError "Cannot unify"
unifyTypes r1@RExt{} r2@RExt{} = unifyRows r1 r2
unifyTypes RNil RNil = pure ()
unifyTypes _ _       = throwError "Cannot unify"

unifyRows :: Type -> Type -> Infer ()
unifyRows (RExt n1 ts1 t1) r2@(RExt n2 ts2 t2)
  | n1 == n2  = unifyAll (t1:ts1) (t2:ts2)
  | t1 == t2  = throwError "Cannot unify"
  | otherwise = do
      let (s, m) = unfoldRow r2
      case Map.lookup n1 m of
        Nothing -> do
          v <- fresh
          unifyAll [s, t1] [RExt n1 ts1 v, foldRow v m]
        Just us ->
          unifyAll (t1:ts1) (foldRow s (removeField n1 m):us)
unifyRows _ _ = error "Implementation error"

unify :: (Typed a1, Typed a2) => a1 -> a2 -> Infer ()
unify a1 a2 = do
  sub <- gets substitution
  let t1 = apply sub (typeOf a1)
      t2 = apply sub (typeOf a2)
  unifyTypes t1 t2

nextIndex :: Infer Int
nextIndex = do
  s <- gets count
  modifyCount (+1)
  pure s

fresh :: Infer Type
fresh = TVar <$> nextIndex

tagExpr :: Expr () -> Infer (Expr Int)
tagExpr = traverse (const nextIndex)

lookupName :: Name -> Infer Type
lookupName name = do
  env <- ask
  case envLookup name env of
    Just ts -> instantiate ts
    Nothing -> throwError ("Name '" <> Text.unpack name <> "' not in scope")

inferExpr :: Expr Int -> Infer (Expr Type)
inferExpr =
  \case
    EVar var  -> EVar <$> inferName var
    ELit prim -> pure (ELit prim)
    EIf e1 e2 e3 -> do
      a1 <- inferExpr e1
      a2 <- inferExpr e2
      a3 <- inferExpr e3
      tBool `unify` a1
      a2 `unify` a3
      pure (EIf a1 a2 a3)
    EApp t e1 es -> do
      a1 <- inferExpr e1
      as <- traverse inferExpr es
      a1 `unify` foldType (TVar t) (typeOf <$> NonEmpty.toList as)
      pure (EApp (TVar t) a1 as)
    ELam lls e1 -> do
      let as = TVar <$$> lls
      a <- local (insertArgs as) (inferExpr e1)
      pure (ELam as a)
    ELet vs e1 -> do
      let lls = unLabel . fst <$> vs
      let vars = second (mono . TVar) <$> lls
      as <- traverse (local (envInserts vars) . inferExpr . snd) vs
      forM_ (NonEmpty.zip lls as) $ \((_, t), a) ->
        TVar t `unify` a
      sub <- gets substitution
      let ts = apply sub . TVar . snd <$> lls
      let names = fst <$> lls
      ss <- traverse generalize ts
      a1 <- local (envInserts (NonEmpty.zip names ss)) (inferExpr e1)
      pure (ELet (NonEmpty.zip (NonEmpty.zipWith Label ts names) as) a1)
    EOp1 (t, op) e1 -> do
      a1 <- inferExpr e1
      t1 <- instantiate (op1Type op)
      t1 `unify` (typeOf a1 ~> TVar t)
      pure (EOp1 (TVar t, op) a1)
    EOp2 (t, op) e1 e2 -> do
      a1 <- inferExpr e1
      a2 <- inferExpr e2
      t1 <- instantiate (op2Type op)
      t1 `unify` foldType (TVar t) (typeOf <$> [a1, a2])
      pure (EOp2 (TVar t, op) a1 a2)
    EPat e1 cs -> do
      a1 <- inferExpr e1
      h :| tl <- traverse (inferClause (typeOf a1)) cs
      forM_ tl (unify h)
      pure (EPat a1 (h :| tl))
    ENil -> pure ENil
    EExt n e1 e2 ->
      EExt n <$> inferExpr e1 <*> inferExpr e2
    ERes (Focus (Label _ n1) (Label t2 n2) (Label t3 n3)) e1 e2 -> do
      a1 <- inferExpr e1
      case focusField n1 a1 of
        Just (n, r) -> do
          n `unify` TVar t2
          r `unify` TVar t3
          sub <- gets substitution
          s2 <- generalize (apply sub (TVar t2))
          a2 <- local (envInserts [(n2, s2), (n3, mono (apply sub (TVar t3)))]) (inferExpr e2)
          pure (ERes (Focus (Label (TVar t2) n1) (Label (TVar t2) n2) (Label (TVar t3) n3)) a1 a2)
        Nothing ->
          throwError "Cannot unify"
  where
    inferName (Label t name) = do
      t1 <- lookupName name
      t1 `unify` TVar t
      pure (Label (TVar t) name)

    inferClause t (Clause (ll:|lls) e1) = do
      let as = TVar <$$> lls
      a2 <- inferName ll
      a2 `unify` foldType t (typeOf <$> as)
      a1 <- local (insertArgs as) (inferExpr e1)
      pure (Clause ((TVar <$> ll) :| as) a1)

focusField :: Name -> Expr t -> Maybe (Expr t, Expr t)
focusField name =
  \case
    r@EExt{} -> do
      let (e, m) = unfoldRec r
      case Map.lookup name m of
        Just (e1:_) ->
          Just (e1, Map.foldrWithKey (flip . foldr . EExt) e (removeField name m))
        _ -> Nothing
    _     -> Nothing

instantiate :: Scheme -> Infer Type
instantiate (Forall vs t) = do
  ts <- replicateM (Set.size vs) fresh
  pure (apply (zipSub (Set.toList vs) ts) t)

generalize :: Type -> Infer Scheme
generalize t = do
  env <- ask
  pure (Forall (tvars t `Set.difference` tvars env) t)

runInferCount :: Int -> TypeEnv -> Infer a -> Either String (a, InferState)
runInferCount count env val = runExcept (runStateT (runReaderT (unInfer val) env) initialState{ count = count })

runInfer :: TypeEnv -> Infer a -> Either String (a, InferState)
runInfer env val = runExcept (runStateT (runReaderT (unInfer val) env) initialState)

typeExpr :: TypeEnv -> Expr () -> Either String (Expr Type)
typeExpr env val = applySub <$> runInfer env (tagExpr val >>= inferExpr)
  where
    applySub (expr, InferState{..}) = apply substitution expr

unTVar :: Type -> Maybe Int
unTVar (TVar t) = Just t
unTVar _        = Nothing

isTVar :: Type -> Bool
isTVar TVar{} = True
isTVar _      = False

maybeSingleton :: (Ord s) => Maybe s -> Set s
maybeSingleton (Just a) = Set.singleton a
maybeSingleton _        = mempty

maybeInsert :: (Ord s) => Maybe s -> Set s -> Set s
maybeInsert (Just a) = Set.insert a
maybeInsert Nothing  = id

data ClauseF t a = ClauseF !(List1 (Label t)) !a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data ExprF t a
  = FVar !(Label t)
  | FLit !Prim
  | FIf  !a !a !a
  | FLet !(List1 (Label t, a)) !a
  | FApp !t !a !(List1 a)
  | FLam !(List1 (Label t)) !a
  | FOp1 !(t, Op1) !a
  | FOp2 !(t, Op2) !a !a
  | FPat !a !(List1 (ClauseF t a))
  | FNil
  | FRes !(Focus t) !a !a
  | FExt !Name !a !a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

toExprF :: Expr t -> Fix (ExprF t)
toExprF =
  unfoldFix $ \case
    EVar var      -> FVar var
    ELit prim     -> FLit prim
    EIf e1 e2 e3  -> FIf e1 e2 e3
    ELet vs e1    -> FLet vs e1
    EApp t e1 es  -> FApp t e1 es
    ELam lls e1   -> FLam lls e1
    EOp1 op e1    -> FOp1 op e1
    EOp2 op e1 e2 -> FOp2 op e1 e2
    EPat e1 cs    -> FPat e1 ((\(Clause lls e) -> ClauseF lls e) <$> cs)
    ENil          -> FNil
    ERes f e1 e2  -> FRes f e1 e2
    EExt n e1 e2  -> FExt n e1 e2

fromExprF :: Fix (ExprF t) -> Expr t
fromExprF =
  foldFix $ \case
    FVar var      -> EVar var
    FLit prim     -> ELit prim
    FIf e1 e2 e3  -> EIf e1 e2 e3
    FLet vs e1    -> ELet vs e1
    FApp t e1 es  -> EApp t e1 es
    FLam lls e1   -> ELam lls e1
    FOp1 op e1    -> EOp1 op e1
    FOp2 op e1 e2 -> EOp2 op e1 e2
    FPat e1 cs    -> EPat e1 ((\(ClauseF lls e) -> Clause lls e) <$> cs)
    FNil          -> ENil
    FRes f e1 e2  -> ERes f e1 e2
    FExt n e1 e2  -> EExt n e1 e2

foldExprF :: (ExprF t a -> a) -> Expr t -> a
foldExprF e = toExprF >>> foldFix e

go :: ExprF t (Expr t) -> Expr t
go = fromExprF . Fix . fmap toExprF

para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para f = rec where rec = f . fmap ((,) <*> rec) . unFix

paraM :: (Monad m, Traversable t) => (t (Fix t, a) -> m a) -> Fix t -> m a
paraM f = rec where rec = f <=< mapM ((\g z -> (z, ) <$> g z) rec) . unFix

substVars :: (Foldable f) => f (Name, Name) -> Expr t -> Expr t
substVars names expr = foldr (uncurry substVar) expr names

substVar :: Name -> Name -> Expr t -> Expr t
substVar var new = replaceVar var (\t -> EVar (Label t new))

replaceVarM :: (Monad m) => Name -> (t -> m (Expr t)) -> Expr t -> m (Expr t)
replaceVarM name f =
  toExprF >>> paraM
    (\case
      FVar (Label t var)
        | var == name -> f t
        | otherwise   -> pure $ EVar (Label t var)

      FLet vs e1
        | name `matchesAnyLabel` (fst <$> vs) -> pure $ ELet (second (fromExprF . fst) <$> vs) (fromExprF (fst e1))
        | otherwise                           -> pure $ ELet (second snd <$> vs) (snd e1)

      FLam lls e1
        | name `matchesAnyLabel` lls -> pure $ ELam lls (fromExprF (fst e1))
        | otherwise                  -> pure $ ELam lls (snd e1)

      FPat e1 cs ->
        pure $ EPat (snd e1) (modClause <$> cs)

      FRes s@(Focus _ ll1 ll2) e1 e2
        | name `matchesAnyLabel` [ll1, ll2] -> pure $ ERes s (fromExprF (fst e1)) (fromExprF (fst e2))
        | otherwise                         -> pure $ ERes s (snd e1) (snd e2)

      e -> pure $ go (snd <$> e))
  where
    modClause (ClauseF lls e)
      | name `matchesAnyLabel` lls = Clause lls (fromExprF (fst e))
      | otherwise                  = Clause lls (snd e)

replaceVar :: Name -> (t -> Expr t) -> Expr t -> Expr t
replaceVar name f = runIdentity . replaceVarM name (pure . f)

matchesLabel :: Name -> Label t -> Bool
matchesLabel name (Label _ label) = name == label

matchesAnyLabel :: (Foldable f) => Name -> f (Label t) -> Bool
matchesAnyLabel = any . matchesLabel

qualifyNames :: Expr t -> Expr t
qualifyNames e = evalState (suffixNames (toExprF e)) 0

suffixed :: (MonadState Int m) => Name -> m Name
suffixed prefix = do
  n <- get
  modify (+1)
  pure (prefix <> showt n)

suffixNames :: Fix (ExprF t) -> State Int (Expr t)
suffixNames =
  foldFix $ \case
    FLet vs e1 -> do
      let (lls, es) = NonEmpty.unzip vs
      sub <- mapping lls
      as <- sequence (substVars sub <$$> es)
      ELet (NonEmpty.zip (relabeled sub lls) as) . substVars sub <$> e1
    FLam lls e1 -> do
      sub <- mapping lls
      ELam (relabeled sub lls) . substVars sub <$> e1
    FRes f e1 e2 -> do
      (f', sub) <- suffixFocus f
      ERes f' <$> e1 <*> (substVars sub <$> e2)
    FPat e1 cs ->
      EPat <$> e1 <*> traverse suffixClause cs
    e ->
      go <$> sequence e
  where
    suffixFocus (Focus ll1 (Label t2 n2) (Label t3 n3)) = do
      sub <- mapping [Label t2 n2, Label t3 n3]
      case snd <$> sub of
        [n2', n3'] -> pure (Focus ll1 (Label t2 n2') (Label t3 n3'), sub)
        (_:_) -> error "Implementation error"
        []    -> error "Implementation error"

    suffixClause (ClauseF lls e) = do
      sub <- mapping lls
      Clause (relabeled sub lls) . substVars sub <$> e

    relabeled =
      NonEmpty.zipWith (\s (Label t _) -> Label t (snd s))

    mapping lls =
      forM lls (\(Label _ name) -> do
        if isConstructor name
          then pure (name, name)
          else (name ,) <$> suffixed (name <> "."))

isPolymorphic :: Type -> Bool
isPolymorphic = not . Set.null . tvars

data BindGroup = Binds
  { bindVars :: !(Map Name (Type, Expr Type))
  , bindExpr :: !(Expr Type)
  } deriving (Show, Eq, Ord, Read)

updateBindGroup :: (BindGroup -> BindGroup) -> List1 (Label Type, Expr Type) -> Expr Type -> Expr Type
updateBindGroup f = from . f <$$> to
  where
    to =
      Binds . foldr (\(Label t name, expr) -> Map.insert name (t, expr)) mempty
    from (Binds vars expr) =
      case nonEmpty (Map.toList vars) of
        Just vs -> ELet ((\(name, (t, e)) -> (Label t name, e)) <$> vs) expr
        Nothing -> error "Implementation error"

bindNames :: BindGroup -> [Name]
bindNames (Binds vs _) = Map.keys vs

mapBindsM :: (Monad m) => (Expr Type -> m (Expr Type)) -> BindGroup -> m BindGroup
mapBindsM f Binds{..} = Binds <$> traverse (secondM f) bindVars <*> f bindExpr

unifier :: Type -> Type -> Substitution
unifier t1 t2 =
  case runInfer mempty (unifyTypes t1 t2) of
    Left{} -> error "Implementation error"
    Right (_, InferState{..}) -> substitution

updateBindings :: Name -> BindGroup -> BindGroup
updateBindings key Binds{..}
  | isPolymorphic t = Binds (foldr fn (Map.delete key vs) names) e
  | otherwise       = Binds{..}
  where
    (t, expr) = bindVars ! key
    fn (n, u) = Map.insert n (apply (unifier t u) (u, substVar key n expr))
    (Binds vs e, names) = run (mapBindsM (updateRefs key) Binds{..})
    run a = evalState (runWriterT a) 0

monomorphize :: Expr Type -> Expr Type
monomorphize = foldExprF $
  \case
    FLet vs e1 -> updateBindGroup (\bg -> foldr updateBindings bg (bindNames bg)) vs e1
    e -> go e

updateRefs :: (MonadState Int m, MonadWriter [(Name, t)] m) => Name -> Expr t -> m (Expr t)
updateRefs name = replaceVarM name $ \t -> do
  new <- suffixed (name <> ".")
  tell [(new, t)]
  pure (EVar (Label t new))

type Dictionary t = Map Name ([Label t], Expr t)

class Tag t where
  funType :: Expr t -> [Label t] -> t

instance Tag Type where
  funType e lls = foldType (typeOf e) (typeOf <$> lls)

instance Tag () where
  funType _ _ = ()

dictionary :: (Tag t) => Expr t -> Dictionary t
dictionary expr = Map.fromList (("$fun._", ([], main)) : defs)
  where
    (main, defs) = evalState (runWriterT (toDef (toExprF expr))) 0
    toDef =
      foldFix $ \case
        FLam lls e -> do
          e1 <- e
          name <- suffixed "$fun."
          let labels = NonEmpty.toList lls
          tell [(name, (labels, e1))]
          pure (EVar (Label (funType e1 labels) name))
        e ->
          go <$> sequence e

simplifyLets :: Dictionary t -> Dictionary t
simplifyLets m = substVars vars <$$> m'
  where
    (m', vars) = runWriter (mapM (secondM (simplify . toExprF)) m)
    simplify = foldFixM $
      \case
        FLet vs e1 -> do
          binds <- foldrM fn [] (NonEmpty.toList vs)
          pure $ case binds of
            a:as -> ELet (a:|as) e1
            []   -> e1
          where
            fn (Label _ name, EVar (Label _ var)) ls = do
              tell [(name, var)]
              pure ls
            fn l ls = pure (l:ls)
        e ->
          pure (go e)

freeIn :: (Ord t) => Name -> Dictionary t -> Set (t, Name)
freeIn name m =
  let (lls, expr) = m ! name
   in free expr `withoutVars` Set.fromList (Map.keys m <> (fst . unLabel <$> lls))

closeDefs :: Dictionary Type -> Dictionary Type
closeDefs defs = uncurry app (runWriter (Map.traverseWithKey fn defs))
  where
    fn key (lls, expr) = do
      let extra = uncurry Label <$> Set.toList (freeIn key defs)
      tell [(key, extra)]
      pure (extra <> lls, expr)

    app m extra =
      case snd =<< extra of
        []    -> m
        (_:_) -> closeDefs (foldr (\(name, lls) -> (applyArgs name lls <$$>)) m extra)

applyArgs :: Name -> [Label Type] -> Expr Type -> Expr Type
applyArgs _ [] = id
applyArgs name (a:as) = flattenApps . fn
  where
    fn = foldExprF $ \case
      FVar (Label t var)
        | name == var ->
            let e1 = EVar (Label (foldType t (typeOf <$> (a:as))) var)
             in EApp t e1 (EVar <$> a:|as)
        | otherwise -> EVar (Label t var)
      e ->
        go e

flattenApps :: Expr t -> Expr t
flattenApps = foldExprF $
  \case
    FApp t (EApp _ e1 es1) es2 -> EApp t e1 (es1 <> es2)
    e -> go e

addImplicitArgs :: Dictionary Type -> Dictionary Type
addImplicitArgs = fmap fn
  where
    fn (lls, expr)
      | n > 0 = ( lls <> NonEmpty.toList (NonEmpty.zipWith Label (unfoldType t) extra)
                , applyExtra extra expr )
      | otherwise = (lls, expr)
      where
        extra = "$v.0" :| [ "$v." <> showt i | i <- [ 1 .. n - 1 ] ]
        n = arity t
        t = typeOf expr

applyExtra :: List1 Name -> Expr Type -> Expr Type
applyExtra args =
  \case
    ELam lls e ->
      let vars = NonEmpty.zipWith (\v (Label _ ll) -> (ll, v)) args lls
          e1   = substVars vars e
       in case NonEmpty.drop (length args) lls of
          []   -> e1
          a:as -> ELam (a:|as) e1

    EApp t e1 es -> app t e1 (NonEmpty.toList es)
    EVar var     -> app (typeOf var) (EVar var) []
    EIf e1 e2 e3 -> EIf (applyExtra args e1) (applyExtra args e2) (applyExtra args e3)
    EPat e1 cs   -> EPat e1 (applyClause <$> cs)
    ELet vs e1   -> ELet vs (applyExtra args e1)
    ERes f e1 e2 -> ERes f e1 (applyExtra args e2)
    ENil         -> error "Implementation error"
    EExt{}       -> error "Implementation error"
    ELit{}       -> error "Implementation error"
    EOp1{}       -> error "Implementation error"
    EOp2{}       -> error "Implementation error"
  where
    applyClause (Clause labels expr) = Clause labels (applyExtra args expr)
    app t e1 es =
      let ts  = unfoldType t
          es' = es `NonEmpty.prependList` NonEmpty.zipWith (EVar <$$> Label) ts args
       in case NonEmpty.drop (length args) ts of
          []     -> error "Implementation error"
          (u:us) -> EApp (foldType u us) e1 es'


-- typeExpr
-- zeroIndexed
-- monomorphize
-- qualifyNames
--
-- dictionary
-- simplifyLets
-- closeDefs
-- add implicit args


type ValueEnv = Environment (Either ([Label Type], Expr Type) Value)

newtype Eval a = Eval { unEval :: ReaderT ValueEnv IO a }
  deriving (Functor, Applicative, Monad, MonadReader ValueEnv, MonadIO)

data Value
  = VPrim !Prim
  | VData !Name ![Value]
  | VCall !(Label Type) ![Expr Type]
  | VNil
  | VExt  !Name !Value !Value
  deriving (Show, Eq, Ord, Read)

isConstructor :: Name -> Bool
isConstructor = isUpper . Text.head . Text.dropWhile (== '$')

eval :: Expr Type -> Eval Value
eval =
  \case
    ELit prim ->
      pure (VPrim prim)

    EVar (Label _ name) -> do
      env <- ask
      if isConstructor name
        then pure (VData name [])
        else case envLookup name env of
          Just (Left ([], e)) ->
            eval e
          Just (Left (lls, e)) ->
            let t = foldType (typeOf e) (typeOf <$> lls)
             in pure (VCall (Label t name) [])
          Just (Right value) ->
            pure value
          Nothing ->
            error ("Name '" <> Text.unpack name <> "' not in scope")

    EApp _ e1 es -> do
      val <- eval e1
      case val of
        VData con as -> do
          vs <- traverse eval es
          pure (VData con (as <> NonEmpty.toList vs))

        VCall (Label t name) as
          | arity t > length args ->
              pure (VCall (Label t name) args)
          | otherwise -> do
              env <- ask
              case envLookup name env of
                Just (Left fun) -> do
                  vs <- traverse eval args
                  evalFun fun vs
                _ -> error "Runtime error"
          where
            args = as <> NonEmpty.toList es

        _ -> error "Runtime error"

    ELet ((Label _ name, e1) :| []) e2 -> do
      val <- eval e1
      local (envInsert name (Right val)) (eval e2)

    ELet{} ->
      error "Runtime error"

    EIf e1 e2 e3 -> do
      val <- eval e1
      case val of
        VPrim (PBool True)  -> eval e2
        VPrim (PBool False) -> eval e3
        _ -> error "Runtime error"

    EOp1 (_, op) e1 -> do
      a1 <- eval e1
      case a1 of
        VPrim p1 -> pure (VPrim (evalOp1 op p1))
        _ -> error "Invalid operand"

    EOp2 (_, op) e1 e2 -> do
      a1 <- eval e1
      a2 <- eval e2
      case (a1, a2) of
        (VPrim p1, VPrim p2) -> pure (VPrim (evalOp2 op p1 p2))
        (_, _) -> error "Invalid operand"

    ELam{} -> error "Runtime error"

    EPat e1 cs -> do
      a1 <- eval e1
      evalPat a1 (NonEmpty.toList cs)

    ENil         -> pure VNil
    EExt n e1 e2 -> VExt n <$> eval e1 <*> eval e2

    ERes (Focus (Label _ name) (Label _ n) (Label _ r)) e1 e2 -> do
      case focusField name e1 of
        Just (a1, a2) -> do
          v1 <- eval a1
          v2 <- eval a2
          local (envInserts [(n, Right v1), (r, Right v2)]) (eval e2)
        Nothing ->
          error "Runtime error"

evalPat :: Value -> [Clause Type] -> Eval Value
evalPat _ [] = error "No match"
evalPat (VData con vs) ((Clause (Label _ name :| lls) expr) : cs)
  | con == name = local (envInserts (bindArgs lls vs)) (eval expr)
  | otherwise   = evalPat (VData con vs) cs
evalPat (VPrim (PBool isTrue)) ((Clause (Label _ val :| []) expr) : cs)
  | (val == "True" && isTrue) || (val == "False" && not isTrue) = eval expr
  | otherwise = evalPat (VPrim (PBool isTrue)) cs
evalPat _ _ = error "Not implemented"

evalOp1 :: Op1 -> Prim -> Prim
evalOp1 ONot (PBool b)   = PBool (not b)
evalOp1 ONeg (PFloat p)  = PFloat (negate p)
evalOp1 ONeg (PDouble p) = PDouble (negate p)
evalOp1 ONeg (PInt32 n)  = PInt32 (negate n)
evalOp1 ONeg (PInt64 n)  = PInt64 (negate n)
evalOp1 _ _              = error "Not implemented"

evalOp2 :: Op2 -> Prim -> Prim -> Prim
evalOp2 OAdd (PFloat p) (PFloat q) = PFloat (p + q)
evalOp2 OMul (PFloat p) (PFloat q) = PFloat (p * q)
evalOp2 OSub (PFloat p) (PFloat q) = PFloat (p - q)
evalOp2 ODiv (PFloat p) (PFloat q) = PFloat (p / q)
evalOp2 OLt  (PFloat p) (PFloat q) = PBool (p < q)
evalOp2 OGt  (PFloat p) (PFloat q) = PBool (p > q)
evalOp2 OLtE (PFloat p) (PFloat q) = PBool (p <= q)
evalOp2 OGtE (PFloat p) (PFloat q) = PBool (p >= q)
evalOp2 OAdd (PDouble p) (PDouble q) = PDouble (p + q)
evalOp2 OMul (PDouble p) (PDouble q) = PDouble (p * q)
evalOp2 OSub (PDouble p) (PDouble q) = PDouble (p - q)
evalOp2 ODiv (PDouble p) (PDouble q) = PDouble (p / q)
evalOp2 OLt  (PDouble p) (PDouble q) = PBool (p < q)
evalOp2 OGt  (PDouble p) (PDouble q) = PBool (p > q)
evalOp2 OLtE (PDouble p) (PDouble q) = PBool (p <= q)
evalOp2 OGtE (PDouble p) (PDouble q) = PBool (p >= q)
evalOp2 OEq  (PInt32 m) (PInt32 n) = PBool (m == n)
evalOp2 ONEq (PInt32 m) (PInt32 n) = PBool (m /= n)
evalOp2 OEq  (PInt64 m) (PInt64 n) = PBool (m == n)
evalOp2 ONEq (PInt64 m) (PInt64 n) = PBool (m /= n)
evalOp2 OEq  (PBool m) (PBool n) = PBool (m == n)
evalOp2 ONEq (PBool m) (PBool n) = PBool (m /= n)
evalOp2 OEq  (PFloat m) (PFloat n) = PBool (m == n)
evalOp2 ONEq (PFloat m) (PFloat n) = PBool (m /= n)
evalOp2 OEq  (PDouble m) (PDouble n) = PBool (m == n)
evalOp2 ONEq (PDouble m) (PDouble n) = PBool (m /= n)
evalOp2 OAdd (PInt32 m) (PInt32 n) = PInt32 (m + n)
evalOp2 OSub (PInt32 m) (PInt32 n) = PInt32 (m - n)
evalOp2 OMul (PInt32 m) (PInt32 n) = PInt32 (m * n)
evalOp2 OLt  (PInt32 p) (PInt32 q) = PBool (p < q)
evalOp2 OGt  (PInt32 p) (PInt32 q) = PBool (p > q)
evalOp2 OLtE (PInt32 p) (PInt32 q) = PBool (p <= q)
evalOp2 OGtE (PInt32 p) (PInt32 q) = PBool (p >= q)
evalOp2 OAdd (PInt64 m) (PInt64 n) = PInt64 (m + n)
evalOp2 OSub (PInt64 m) (PInt64 n) = PInt64 (m - n)
evalOp2 OMul (PInt64 m) (PInt64 n) = PInt64 (m * n)
evalOp2 OLt  (PInt64 p) (PInt64 q) = PBool (p < q)
evalOp2 OGt  (PInt64 p) (PInt64 q) = PBool (p > q)
evalOp2 OLtE (PInt64 p) (PInt64 q) = PBool (p <= q)
evalOp2 OGtE (PInt64 p) (PInt64 q) = PBool (p >= q)
evalOp2 _ _ _ = error "Not implemented"

evalFun :: ([Label Type], Expr Type) -> [Value] -> Eval Value
evalFun (vars, expr) args = local (envInserts (bindArgs vars args)) (eval expr)

bindArgs :: [Label Type] -> [a] -> [(Name, Either e a)]
bindArgs lls vs = zip (fst . unLabel <$> lls) (Right <$> vs)

runEval :: ValueEnv -> Eval a -> IO a
runEval env input = runReaderT (unEval input) env

dictToEnv :: Dictionary Type -> ValueEnv
dictToEnv = Map.foldrWithKey (\k -> envInsert k . Left) envEmpty

runDict :: Dictionary Type -> IO Value
runDict dict = runEval (dictToEnv dict) (eval (snd (dict ! "$fun._")))

--

class Match a where
  replace :: Name -> Name -> a -> a

instance Match a => Match [a] where
  replace = fmap <$$> replace

instance Match a => Match (MClause a) where
  replace var new (MClause con vs e) =
    MClause con vs (replace var new e)

instance Match a => Match (MExpr a) where
  replace _ _ Fail = Fail
  replace var new (Expr e)    = Expr (replace var new e)
  replace var new (Case v cs) = Case v (replace var new cs)

instance Match (Expr t) where
  replace = substVar

data MPattern
  = MCon !Name ![MPattern]
  | MVar !Name
  deriving (Show, Eq)

data MClause a = MClause !Name ![Name] !(MExpr a)
  deriving (Show, Eq)

data MExpr a
  = Fail
  | Expr !a
  | Case !Name ![MClause a]
  deriving (Show, Eq)

type MatchEq a = ([MPattern], MExpr a)

fstPat :: (MPattern -> a) -> MatchEq e -> a
fstPat f (p:_, _) = f p
fstPat _ ([], _)  = error "Empty list"

conName :: MPattern -> Name
conName (MCon name _) = name
conName MVar{} = ""

isCon :: MPattern -> Bool
isCon MCon{} = True
isCon MVar{} = False

isVar :: MPattern -> Bool
isVar MVar{} = True
isVar MCon{} = False

patPredicate :: MPattern -> MPattern -> Bool
patPredicate MVar{} = isVar
patPredicate MCon{} = isCon

-- TODO: rename
next :: State Int Int
next = do
  s <- get
  modify (+1)
  pure s

match :: (Match a) => [Name] -> [MatchEq a] -> MExpr a -> MExpr a
match us0 qs0 e0 = evalState (matchM us0 qs0 e0) 0 where
  -- Empty rule
  matchM [] [([], e)] _ = pure e
  matchM [] _ _ = error "Implementation error"
  matchM (u:us) qs e
    -- Variable rule
    | all (fstPat isVar) qs = matchM us (varRule <$> qs) e
    -- Constructor rule
    | all (fstPat isCon) qs = Case u <$> traverse conRule (groupByConstructor qs)
    -- Mixed rule
    | otherwise = matchM us qs2 e >>= matchM us qs1
    where
      (qs1, qs2) = partitionEqs qs

      varRule (MVar var:ps, e1) = (ps, replace var u e1)
      varRule (_, _) = error "Implementation error"

      conRule gs@((MCon con ps:_, _):_) = do
        vs <- (\f -> "$v." <> showt f) <$$> replicateM (length ps) next
        MClause con vs <$> matchM (vs <> us) (ps' <$> gs) e
      conRule _ = error "Implementation error"

      ps' (MCon _ ps:ds, e1) = (ps <> ds, e1)
      ps' (_, _)             = error "Implementation error"

partitionEqs :: [MatchEq a] -> ([MatchEq a], [MatchEq a])
partitionEqs [] = error "Implementation error"
partitionEqs (eq:eqs) = span (fstPat (fstPat patPredicate eq)) (eq:eqs)

groupByConstructor :: [MatchEq a] -> [[MatchEq a]]
groupByConstructor = grouped . sorted
  where
    grouped = groupBy (on (==) (fstPat conName))
    sorted  = sortBy (compare `on` fstPat conName)

compileMExpr :: MExpr (Expr ()) -> Expr ()
compileMExpr =
  \case
    Fail -> error "Pattern matching failure"
    Expr expr -> expr
    Case name cs ->
      EPat (EVar (Label () name)) (clauseList cs)
  where
    compileMClause (MClause con vs e) =
      Clause (Label () <$> (con:|vs)) (compileMExpr e)
    clauseList cs =
      case compileMClause <$> cs of
        d:ds -> d:|ds
        []   -> error "Empty list"

-----------------
-- LLVM IR
-----------------

(<+>) :: Text -> Text -> Text
a <+> b = a <> " " <> b

infixr 6 <+>

data IRType
  = TInt1
  | TInt8
  | TInt32
  | TInt64
  | TFloat
  | TDouble
  | TVoid
  | TFun !IRType ![IRType]
  | TPtr !IRType
  | TStruct ![IRType]
  | TName !Name !IRType
  deriving (Show, Eq, Ord, Read)

i1 :: IRType
i1 = TInt1

i8 :: IRType
i8 = TInt8

i32 :: IRType
i32 = TInt32

i64 :: IRType
i64 = TInt64

ptr :: IRType -> IRType
ptr = TPtr

fun :: IRType -> [IRType] -> IRType
fun = TFun

struct :: [IRType] -> IRType
struct = TStruct

data IRValue
  = Local  !IRType !Name
  | Global !IRType !Name
  | I1     !Bool
  | I32    !Int32
  | I64    !Int64
  | Float  !Float
  | Double !Double
  deriving (Show, Eq, Ord, Read)

irTypeOf :: IRValue -> IRType
irTypeOf =
  \case
    Local  t _ -> t
    Global t _ -> t
    I1     _   -> TInt1
    I32    _   -> TInt32
    I64    _   -> TInt64
    Float  _   -> TFloat
    Double _   -> TDouble

data IRInstrF v t a
  = IAdd   !t !v !v    !(v -> a)
  | ISub   !t !v !v    !(v -> a)
  | IMul   !t !v !v    !(v -> a)
  | IRet   !t !v       !(v -> a)
  | ILoad  !t !v       !(v -> a)
  | IStore !v !v       !(v -> a)
  | IGep   !t !v !v !v !(v -> a)
  | IAlloc !t          !(v -> a)
  | IBCast !v !t       !(v -> a)
  | ICall  !t !v ![v]  !(v -> a)
  deriving (Functor)

data IRConstruct a
  = CDefine  !IRType ![(IRType, Name)] a
  | CDeclare !IRType ![IRType]
  | CType    !IRType
  deriving (Functor)

data IRState = IRState
  { globalCount :: !Int
  , definitions :: !(Map Name (IRConstruct (IRCode IRState)))
  }

initialIRState :: IRState
initialIRState = IRState
  { globalCount = 1
  , definitions = mempty
  }

type IRInstr v t = Free (IRInstrF v t)

add :: (MonadFree (IRInstrF v t) m) => t -> v -> v -> m v
add t v w = wrap (IAdd t v w pure)

sub :: (MonadFree (IRInstrF v t) m) => t -> v -> v -> m v
sub t v w = wrap (ISub t v w pure)

mul :: (MonadFree (IRInstrF v t) m) => t -> v -> v -> m v
mul t v w = wrap (IMul t v w pure)

alloca :: (MonadFree (IRInstrF v t) m) => t -> m v
alloca t = wrap (IAlloc t pure)

load :: (MonadFree (IRInstrF v t) m) => t -> v -> m v
load t v = wrap (ILoad t v pure)

ret :: (MonadFree (IRInstrF v t) m) => t -> v -> m v
ret t v = wrap (IRet t v pure)

store :: (MonadFree (IRInstrF v t) m) => v -> v -> m v
store v w = wrap (IStore v w pure)

getelementptr :: (MonadFree (IRInstrF v t) m) => t -> v -> v -> v -> m v
getelementptr t u v w = wrap (IGep t u v w pure)

call :: (MonadFree (IRInstrF v t) m) => t -> v -> [v] -> m v
call t v vs = wrap (ICall t v vs pure)

bitcast :: (MonadFree (IRInstrF v t) m) => v -> t -> m v
bitcast v t = wrap (IBCast v t pure)

irPrim :: Prim -> IRValue
irPrim =
  \case
    PBool b    -> I1 b
    PInt32 i32 -> I32 i32
    PInt64 i64 -> I64 i64
    PFloat f   -> Float f
    PDouble d  -> Double d

class IR a where
  encode :: a -> Text

instance (IR a) => IR [a] where
  encode = Text.concat . intersperse ", " . (encode <$>)

instance IR IRValue where
  encode =
    \case
      Local  _ name  -> "%" <> enquote name
      Global _ name  -> "@" <> enquote name
      I1     False   -> "0"
      I1     True    -> "1"
      I32    n -> showt n
      I64    n -> showt n
      Float  f -> showt f
      Double d -> showt d

instance IR IRType where
  encode =
    \case
      TInt1     -> "i1"
      TInt8     -> "i8"
      TInt32    -> "i32"
      TInt64    -> "i64"
      TFloat    -> "float"
      TDouble   -> "double"
      TVoid     -> "void"
      TFun t ts -> encode t <+> "(" <> encode ts <> ")" <> "*"
      TPtr t    -> encode t <> "*"
      TStruct t -> "{" <+> encode t <+> "}"
      TName n _ -> "%" <> enquote n

enquote :: Text -> Text
enquote n
  | Text.all isAlphaNum n = n
  | otherwise = "\"" <> n <> "\""

newtype IRTyped v = IRTyped v
  deriving (Show, Eq, Ord, Read)

instance IR (IRTyped IRValue) where
  encode (IRTyped v) = encode (irTypeOf v) <+> encode v

instance IR (IRCode IRState) where
  encode code = Text.unlines (indent 2 <$> snd (runCodegen (runIRCode code)))

indent :: Int -> Text -> Text
indent level = (spaces <>) where spaces = Text.replicate level " "

instance IR (IRType, Name) where
  encode = encode . IRTyped . uncurry Local

data IRNamed = IRNamed !Name !(IRConstruct (IRCode IRState))

instance IR IRNamed where
  encode (IRNamed name ir) =
    case ir of
      CDefine t as e ->
        let signature = encode t <+> "@" <> enquote name <> "(" <> encode as <> ")"
            body = "{\n" <> encode e <> "}\n"
         in "define" <+> signature <+> body
      CDeclare t ts ->
        "declare" <+> encode t <+> "@" <> enquote name <> "(" <> encode ts <> ")"
      CType t -> do
        let body = "{\n" <> encodeStruct t <> "\n}\n"
        "%" <> enquote name <+> "=" <+> "type" <+> body
        where
          encodeStruct = \case
            TStruct ts -> Text.intercalate ",\n" (indent 2 . encode <$> ts)
            t -> encode t

irFunType :: Type -> (IRType, [IRType])
irFunType =
  \case
    TCon "->" [t1, t2] -> second (toIRType t1 :) (irFunType t2)
    t                  -> (toIRType t, [])

irFunTypeOf :: (Typed t) => t -> (IRType, [IRType])
irFunTypeOf = irFunType . typeOf

toIRType :: Type -> IRType
toIRType =
  \case
    TCon "Bool"   [] -> i1
    TCon "Int32"  [] -> i32
    TCon "Int64"  [] -> i64
    TCon "Float"  [] -> TFloat
    TCon "Double" [] -> TDouble
    TCon "Char"   [] -> i8
    TCon "->"     _  -> ptr i8

type Codegen = WriterT [Text] (State Int)

runCodegen :: Codegen a -> (a, [Text])
runCodegen a = evalState (runWriterT a) 1

execCodegen :: Codegen a -> a
execCodegen = fst . runCodegen

type IRCode = IRInstr IRValue IRType

runIRCode :: IRCode a -> Codegen a
runIRCode = iterM interpreter

newtype IREval a = IREval { unIREval :: ReaderT (Environment IRValue) (StateT IRState IRCode) a }
  deriving (Functor, Applicative, Monad, MonadReader (Environment IRValue), MonadState IRState, MonadFree (IRInstrF IRValue IRType))

runIREval :: Environment IRValue -> IREval a -> IRCode (a, IRState)
runIREval env val = runStateT (runReaderT (unIREval val) env) initialIRState

instruction :: IRType -> Text -> (IRValue -> Codegen a) -> Codegen a
instruction t s next = do
  var <- gets (Local t . showt)
  modify (+1)
  tell [encode var <+> "=" <+> s]
  next var

interpreter :: IRInstrF IRValue IRType (Codegen a) -> Codegen a
interpreter =
  \case
    IAdd t v w next ->
      instruction t ("add" <+> encode t <+> encode v <> "," <+> encode w) next
    ISub t v w next ->
      instruction t ("sub" <+> encode t <+> encode v <> "," <+> encode w) next
    IMul t v w next ->
      instruction t ("mul" <+> encode t <+> encode v <> "," <+> encode w) next
    IAlloc t next ->
      instruction (TPtr t) ("alloca" <+> encode t) next
    ILoad t v next ->
      instruction t ("load" <+> encode t <> "," <+> encode (IRTyped v)) next
    IBCast v t next ->
      instruction t ("bitcast" <+> encode (IRTyped v) <+> "to" <+> encode t) next
    IGep t u v w next ->
      instruction (ptr (member t w)) ("getelementptr" <+> encode t <> "," <+> encode (IRTyped u) <> "," <+> encode (IRTyped v) <> "," <+> encode (IRTyped w)) next
    ICall t v vs next ->
      instruction t ("call" <+> encode t <+> encode v <> "(" <> encode (IRTyped <$> vs) <> ")") next
    IStore v w next -> do
      tell ["store" <+> encode (IRTyped v) <> "," <+> encode (IRTyped w)]
      next v
    IRet t v next -> do
      tell ["ret" <+> encode t <+> encode v]
      next v
    _ ->
      error "Not implemented"
  where
    member (TName _ t) n        = member t n
    member (TStruct ts) (I32 n) = ts !! fromIntegral n

