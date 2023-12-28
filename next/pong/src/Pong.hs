{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Pong where

import Control.Arrow ((>>>))
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Fix
import Data.Foldable (foldMap')
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, (<|))
import Data.Map.Strict (Map, (!))
import Data.Maybe (fromMaybe)
import Data.Set (Set, member)
import Data.Text (Text)
import Data.Tuple.Extra
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
  deriving (Show, Eq, Ord, Read)

data Scheme = Forall !(Set Int) !Type
  deriving (Show, Eq, Ord, Read)

data Prim
  = PUnit
  | PBool   !Bool
  | PInt    !Int
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
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data LExpr
  = LVar !(Label Type)
  | LCon !(Label Type) ![LExpr]
  | LLit !Prim
  | LLet !(Label Type) !LExpr !LExpr
  | LCal !(Label Type) ![LExpr]
  | LOp1 !(Type, Op1) !LExpr
  | LOp2 !(Type, Op2) !LExpr !LExpr
  | LPat !LExpr !(List1 ([Label Type], LExpr))
  deriving (Show, Eq, Ord, Read)

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
      PInt{}    -> tInt
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
  tvars =
    \case
      TCon _ ts    -> tvars ts
      TVar t       -> Set.singleton t

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

class Free f where
  free :: f -> Set Name

instance Free f => Free [f] where
  free = Set.unions . fmap free

instance Free f => Free (List1 f) where
  free = free . NonEmpty.toList

instance Free (Label t) where
  free _ = mempty

instance Free (Clause f) where
  free (Clause lls e1) = free e1 `Set.difference` bound lls

instance (Free f, Free e) => Free (f, e) where
  free (a, b) = free a <> free b

instance Free (Expr t) where
  free =
    \case
      EVar (Label _ v) -> Set.singleton v
      ELit _           -> mempty
      EIf e1 e2 e3     -> free e1 <> free e2 <> free e3
      ELet vs e1       -> (free e1 <> free vs) `Set.difference` bound vs
      EApp _ e1 es     -> free e1 <> free es
      ELam lls e1      -> free e1 `Set.difference` bound lls
      EOp1 _ e1        -> free e1
      EOp2 _ e1 e2     -> free e1 <> free e2
      EPat e1 cs       -> free e1 <> free cs

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

tInt :: Type
tInt = TCon "Int" []

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

foldType :: Type -> [Type] -> Type
foldType = foldr tArr

unLabel :: Label t -> (Name, t)
unLabel (Label t name) = (name, t)

--unClause :: Clause t -> ([Label t], Expr t)
--unClause (Clause labels expr) = (labels, expr)

arity :: Type -> Int
arity =
  \case
    TCon "->" [_, t] -> 1 + arity t
    TCon{} -> 0
    TVar{} -> 0

unfoldType :: Type -> List1 Type
unfoldType =
  \case
    TCon "->" [t1, t2] -> t1 <| unfoldType t2
    t                  -> NonEmpty.singleton t

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

--

canonicalExpr :: Expr Type -> Expr Type
canonicalExpr expr = apply (zipSub vs ts) expr
  where
    vs = Set.toList (foldr (maybeInsert . unTVar) mempty expr)
    ts = TVar <$> [ 0 .. ]

data InferState = InferState { count :: !Int, substitution :: !Substitution }
  deriving (Show, Eq, Ord)

initialInferState :: InferState
initialInferState = InferState 0 mempty

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
      let lbls = unLabel . fst <$> vs
      let vars = second (mono . TVar) <$> lbls
      as <- traverse (local (envInserts vars) . inferExpr . snd) vs
      forM_ (NonEmpty.zip lbls as) $ \((_, t), a) ->
        TVar t `unify` a
      sub <- gets substitution
      let ts = apply sub . TVar . snd <$> lbls
      let names = fst <$> lbls
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

instantiate :: Scheme -> Infer Type
instantiate (Forall vs t) = do
  ts <- replicateM (Set.size vs) fresh
  pure (apply (zipSub (Set.toList vs) ts) t)

generalize :: Type -> Infer Scheme
generalize t = do
  env <- ask
  pure (Forall (tvars t `Set.difference` tvars env) t)

runInfer :: TypeEnv -> Infer a -> Either String (a, InferState)
runInfer env val = runExcept (runStateT (runReaderT (unInfer val) env) initialInferState)

typeExpr :: TypeEnv -> Expr () -> Either String (Expr Type)
typeExpr env val = extract <$> runInfer env (tagExpr val >>= inferExpr)
  where
    extract (expr, InferState{..}) = apply substitution expr

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

para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para f = go where go = f . fmap ((,) <*> go) . unFix

paraM :: (Monad m, Traversable t) => (t (Fix t, a) -> m a) -> Fix t -> m a
paraM f = go where go = f <=< mapM ((\g x -> (x, ) <$> g x) go) . unFix

substVars :: (Foldable f) => f (Name, Name) -> Expr () -> Expr ()
substVars names expr = foldr (uncurry substVar) expr names

substVar :: Name -> Name -> Expr () -> Expr ()
substVar var new = relabel var (const (EVar (Label () new)))

relabelM :: (Monad m) => Name -> (t -> m (Expr t)) -> Expr t -> m (Expr t)
relabelM name f = modExpr . toExprF
  where
    modExpr =
      paraM $ \case
        FVar (Label t var)
          | var == name -> f t
          | otherwise   -> pure $ EVar (Label t var)

        FLet vs e1
          | name `matchesAnyLabel` (fst <$> vs) -> pure $ ELet (second (fromExprF . fst) <$> vs) (fromExprF (fst e1))
          | otherwise                           -> pure $ ELet (second snd <$> vs) (snd e1)

        FLam lls e1
          | name `matchesAnyLabel` lls -> pure $ ELam lls (fromExprF (fst e1))
          | otherwise                  -> pure $ ELam lls (snd e1)

        FPat e1 cs    -> pure $ EPat (snd e1) (modClause <$> cs)
        FLit prim     -> pure $ ELit prim
        FIf e1 e2 e3  -> pure $ EIf (snd e1) (snd e2) (snd e3)
        FApp t e1 es  -> pure $ EApp t (snd e1) (snd <$> es)
        FOp1 op e1    -> pure $ EOp1 op (snd e1)
        FOp2 op e1 e2 -> pure $ EOp2 op (snd e1) (snd e2)

    modClause (ClauseF lls e)
      | name `matchesAnyLabel` lls = Clause lls (fromExprF (fst e))
      | otherwise                  = Clause lls (snd e)

relabel :: Name -> (t -> Expr t) -> Expr t -> Expr t
relabel name f = runIdentity . relabelM name (pure . f)

matchesLabel :: Name -> Label t -> Bool
matchesLabel name (Label _ label) = name == label

matchesAnyLabel :: (Foldable f) => Name -> f (Label t) -> Bool
matchesAnyLabel = any . matchesLabel

qualifyNames :: Expr () -> Expr ()
qualifyNames e = evalState (suffixNames (toExprF e)) 0

suffixed :: (MonadState Int m) => Name -> m Name
suffixed prefix = do
  n <- get
  modify (+1)
  pure (prefix <> showt n)

suffixNames :: Fix (ExprF ()) -> State Int (Expr ())
suffixNames =
  foldFix $ \case
    FLet vs e1 -> do
      let (lls, es) = NonEmpty.unzip vs
      sub <- mapping lls
      exprs <- sequence (substVars sub <$$> es)
      ELet (NonEmpty.zip (relabeled sub lls) exprs) . substVars sub <$> e1
    FLam lls e1 -> do
      sub <- mapping lls
      ELam (relabeled sub lls) . substVars sub <$> e1
    FPat e1 cs    -> EPat <$> e1 <*> traverse suffixClause cs
    FVar var      -> pure $ EVar var
    FLit prim     -> pure $ ELit prim
    FIf  e1 e2 e3 -> EIf <$> e1 <*> e2 <*> e3
    FApp t e1 es  -> EApp t <$> e1 <*> sequence es
    FOp1 op e1    -> EOp1 op <$> e1
    FOp2 op e1 e2 -> EOp2 op <$> e1 <*> e2
  where
    suffixClause (ClauseF lls e) = do
      sub <- mapping lls
      Clause (relabeled sub lls) . substVars sub <$> e

    relabeled = NonEmpty.zipWith (\s (Label t _) -> Label t (snd s))
    mapping lls = forM lls (\(Label _ name) -> (name ,) <$> suffixed (name <> "."))

isPolymorphic :: Type -> Bool
isPolymorphic = not . Set.null . tvars

data BindGroup = Binds 
  { bindVars :: !(Map Name (Type, Expr Type))
  , bindExpr :: !(Expr Type)
  } deriving (Show, Eq, Ord, Read)

bgFromExpr :: List1 (Label Type, Expr Type) -> Expr Type -> BindGroup
bgFromExpr = Binds . foldr (\(Label t name, expr) -> Map.insert name (t, expr)) mempty

bgToExpr :: BindGroup -> Expr Type
bgToExpr (Binds vars expr) = 
  case nonEmpty (Map.toList vars) of
    Just vs -> ELet ((\(name, (t, e)) -> (Label t name, e)) <$> vs) expr
    Nothing -> error "Implementation error"

bgKeys :: BindGroup -> [Name]
bgKeys (Binds vs _) = Map.keys vs

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
    fn (n, u) = Map.insert n (apply (unifier t u) (u, expr))
    (Binds vs e, names) = run (mapBindsM (updateRefs key) Binds{..})
    run a = evalState (runWriterT a) 0

monomorphize :: Expr Type -> Expr Type
monomorphize = toExprF >>> 
  foldFix (\case
    FLet vs e1 -> 
      let compile bg = foldr updateBindings bg (bgKeys bg)
       in bgToExpr (compile (bgFromExpr vs e1))

    FVar var      -> EVar var
    FLit prim     -> ELit prim
    FIf  e1 e2 e3 -> EIf e1 e2 e3
    FApp t e1 es  -> EApp t e1 es
    FLam lls e1   -> ELam lls e1
    FOp1 op e1    -> EOp1 op e1
    FOp2 op e1 e2 -> EOp2 op e1 e2
    FPat e1 cs    -> EPat e1 (fmap (\(ClauseF lls e) -> Clause lls e) cs))

updateRefs :: (MonadState Int m, MonadWriter [(Name, t)] m) => Name -> Expr t -> m (Expr t)
updateRefs name = relabelM name $ \t -> do
  new <- suffixed (name <> ".")
  tell [(new, t)]
  pure (EVar (Label t new))

type DefMap t = Map Name ([Label t], Expr t)

class Tag t where
  lamTag :: Expr t -> [Label t] -> t

instance Tag Type where
  lamTag e lls = foldType (typeOf e) (typeOf <$> lls)

instance Tag () where
  lamTag _ _ = ()

defMap :: (Tag t) => Expr t -> DefMap t
defMap expr = Map.fromList (("$fun._", ([], main)) : defs)
  where
    (main, defs) = evalState (runWriterT (toDef (toExprF expr))) 0
    toDef =
      foldFix $ \case
        FLam lls e -> do
          e1 <- e
          name <- suffixed "$fun."
          let labels = NonEmpty.toList lls
          tell [(name, (labels, e1))]
          pure (EVar (Label (lamTag e1 labels) name))

        FVar var        -> pure (EVar var)
        FLit prim       -> pure (ELit prim)
        FIf e1 e2 e3    -> EIf <$> e1 <*> e2 <*> e3
        FLet vs e1      -> ELet <$> traverse sequence vs <*> e1
        FApp t e1 es    -> EApp t <$> e1 <*> sequence es
        FOp1 op e1      -> EOp1 op <$> e1
        FOp2 op e1 e2   -> EOp2 op <$> e1 <*> e2
        FPat e1 cs      -> EPat <$> e1 <*> traverse (\(ClauseF lls e) -> Clause lls <$> e) cs




-- qualifyNames
-- typeExpr
-- canonicalExpr
-- monomorphize
-- defMap
--
-- closeExprs ???
-- add missing args



--
--
--
--



--
-- Tree transf.
--

-- overExprM :: (Monad m) => (Expr t -> m (Expr t)) -> Expr t -> m (Expr t)
-- overExprM f =
--   \case
--     EVar var        -> pure (EVar var)
--     ELit prim       -> pure (ELit prim)
--     EIf e1 e2 e3    -> EIf <$> f e1 <*> f e2 <*> f e3
--     ELet vs e1      -> ELet <$> traverse (secondM f) vs <*> f e1
--     EApp t e1 es    -> EApp t <$> f e1 <*> traverse f es
--     ELam lls e      -> ELam lls <$> f e
--     EOp1 op e1      -> EOp1 op <$> f e1
--     EOp2 op e1 e2   -> EOp2 op <$> f e1 <*> f e2
--     EPat e1 cs      -> EPat <$> f e1 <*> traverse (\(Clause lls e) -> Clause lls <$> f e) cs
--
-- overExpr :: (Expr t -> Expr t) -> Expr t -> Expr t
-- overExpr f = runIdentity . overExprM (pure . f)
--
-- --
--
-- toLExpr :: Expr Type -> LExpr
-- toLExpr =
--   \case
--     ELam{}            -> error "Implementation error"
--     ELet _ _          -> error "TODO"
--     ELit prim         -> LLit prim
--     EOp1 op e1        -> LOp1 op (toLExpr e1)
--     EOp2 op e1 e2     -> LOp2 op (toLExpr e1) (toLExpr e2)
--     EPat e1 cs        -> LPat (toLExpr e1) (toLExpr <$$> unClause <$> cs)
--     EIf e1 e2 e3      -> LPat (toLExpr e1) (clause "True" e2 :| [clause "False" e3])
--     EVar (Label t con)
--       | isDataCon con -> LCon (Label t con) []
--       | otherwise     -> LVar (Label t con)
--     EApp t (EVar (Label _ f)) es
--       | isDataCon f   -> LCon (Label t f) (toLExpr <$> NonEmpty.toList es)
--       | otherwise     -> LCal (Label t f) (toLExpr <$> NonEmpty.toList es)
--     EApp{}            -> error "Implementation error"
--   where
--     clause con expr = ([Label tBool con], toLExpr expr)
--
-- isDataCon :: Text -> Bool
-- isDataCon name
--   | Text.null name = error "Implementation error"
--   | '$' == c       = isDataCon (Text.tail name)
--   | otherwise      = isUpper c
--   where
--     c = Text.head name
--
-- --
--
-- -- (x) close lambdas
-- -- (x) flatten
-- --
-- -- (x) type
-- -- (x) monomorphize
-- --
-- -- (x) add implicit arguments
-- -- (x) flatten
-- --
-- -- (x) lift lambdas
-- --
-- -- (x) convert to LExpr
--
-- --
--
-- flattenLambdas :: Expr t -> Expr t
-- flattenLambdas =
--   \case
--     ELam lls1 (ELam lls2 e1) -> flattenLambdas (ELam (lls1 <> lls2) e1)
--     e                        -> overExpr flattenLambdas e
--
-- flattenApps :: Expr t -> Expr t
-- flattenApps =
--   \case
--     EApp t (EApp _ e1 es1) es2 -> flattenApps (EApp t e1 (es1 <> es2))
--     e                          -> overExpr flattenApps e
--
-- flatten :: Expr t -> Expr t
-- flatten = flattenApps . flattenLambdas
--
-- --
--
-- isPolymorphic :: Type -> Bool
-- isPolymorphic = not . Set.null . tvars
--
-- unifier :: Type -> Type -> Substitution
-- unifier t1 t2 =
--   case runInfer mempty (unifyTypes t1 t2) of
--     Left{} -> error "Implementation error"
--     Right (_, InferState{..}) -> substitution
--
-- monomorphize :: Expr Type -> Expr Type
-- monomorphize =
--   \case
--     ELet vs e1 ->
--       let letMap = toMap (typeOf e1, e1) vs
--           mmized = foldr compile letMap (Map.keys letMap \\ ["_"])
--        in case fromMap mmized of
--          ([], _)   -> error "Implementation error"
--          (a:as, e) -> overExpr monomorphize (ELet (a:|as) e)
--       where
--         compile key map
--           | isPolymorphic t = foldr fn (Map.delete key m) ns
--           | otherwise       = map
--           where
--             fn (n, u) = Map.insert n (apply (unifier t u) (u, e))
--             (m, ns) = evalState (runWriterT (traverse (secondM (updateRefs key)) map)) 0
--             (t, e) = map ! key
--
--         toMap = foldr insert . Map.singleton "_"
--
--         insert (Label t name, expr) =
--           Map.insert name (t, expr)
--
--         fromMap map =
--           ( binding <$> Map.toList (Map.delete "_" map)
--           , snd (map ! "_") )
--
--         binding (name, (t, expr)) = (Label t name, expr)
--
--     e -> overExpr monomorphize e
--
-- updateRefs :: (MonadState Int m, MonadWriter [(Name, Type)] m) => Name -> Expr Type -> m (Expr Type)
-- updateRefs name = individualizeRefs
--   where
--     individualizeRefs =
--       \case
--         EVar var
--           | name `matchesLabel` var -> individualizeName var
--           | otherwise               -> pure (EVar var)
--
--         ELit prim -> pure (ELit prim)
--
--         EIf e1 e2 e3 -> EIf <$> individualizeRefs e1
--                             <*> individualizeRefs e2
--                             <*> individualizeRefs e3
--
--         EApp t e1 es -> EApp t <$> individualizeRefs e1
--                                <*> traverse individualizeRefs es
--
--         EOp1 op e1 -> EOp1 op <$> individualizeRefs e1
--
--         EOp2 op e1 e2 -> EOp2 op <$> individualizeRefs e1
--                                  <*> individualizeRefs e2
--
--         EPat e1 cs -> EPat <$> individualizeRefs e1
--                            <*> traverse individualizeClause cs
--
--         ELet vs e1
--           | name `matchesAnyLabel` (fst <$> vs) -> pure (ELet vs e1)
--           | otherwise                           -> ELet <$> traverse individualizeBinding vs
--                                                         <*> individualizeRefs e1
--         ELam lls e
--           | name `matchesAnyLabel` lls -> pure (ELam lls e)
--           | otherwise                  -> ELam lls <$> individualizeRefs e
--
--     individualizeClause (Clause lls e)
--       | name `matchesAnyLabel` lls = pure (Clause lls e)
--       | otherwise                  = Clause lls <$> individualizeRefs e
--
--     individualizeName (Label t var) = do
--       un <- suffixed var
--       tell [(un, t)]
--       pure (EVar (Label t un))
--
--     individualizeBinding (ll, expr) = (ll, ) <$> individualizeRefs expr
--
-- matchesLabel :: Name -> Label t -> Bool
-- matchesLabel name (Label _ label) = name == label
--
-- matchesAnyLabel :: (Foldable f) => Name -> f (Label t) -> Bool
-- matchesAnyLabel = any . matchesLabel
--
-- suffixed :: (MonadState Int m) => Name -> m Name
-- suffixed var = do
--   i <- get
--   modify (+1)
--   pure (var <> "." <> showt i)
--
-- --
--
-- convertClosures :: Expr () -> Expr ()
-- convertClosures = flatten . closeExpr
--   where
--     closeExpr =
--       \case
--         ELet vs e1 ->
--           let letMap = ([], ) . closeExpr <$> toMap e1 vs
--               closed = addArgs <$> foldr compile letMap (Map.keys letMap \\ ["_"])
--            in case fromMap closed of
--              ([], _)   -> error "Implementation error"
--              (a:as, e) -> ELet (a:|as) e
--         e ->
--           overExpr closeExpr e
--
--     addArgs =
--       \case
--         ([], expr)     -> expr
--         (ll:lls, expr) -> ELam (ll:|lls) expr
--
--     toMap e =
--       foldr insert (Map.singleton "_" e)
--
--     insert (Label _ name, expr) =
--       Map.insert name expr
--
--     fromMap map =
--       ( first (Label ()) <$> Map.toList (Map.delete "_" map)
--       , map ! "_" )
--
--     compile key map
--       | null fvars = map
--       | otherwise =
--           let e1' = substVars vars (app (labels snd) e1)
--               e2' = app (labels fst) e2
--            in Map.insert "_" ([], e2') (Map.insert key (labels snd, e1') map)
--       where
--         fvars = Set.toList (free e1)
--         vars = zip fvars (("$" <>) <$> fvars)
--         labels f = Label () . f <$> vars
--         e1 = snd (map ! key)
--         e2 = snd (map ! "_")
--
--         app (ll:lls) = applyArgs key (EVar <$> (ll:|lls)) . convertClosures
--         app _        = error "Implementation error"
--
-- substVars :: (Foldable f) => f (Name, Name) -> Expr () -> Expr ()
-- substVars names expr = foldr (uncurry substVar) expr names
--
-- substVar :: Name -> Name -> Expr () -> Expr ()
-- substVar old new = relabel old (const (EVar (Label () new)))
--
-- applyArgs :: Name -> NonEmpty (Expr ()) -> Expr () -> Expr ()
-- applyArgs name args = relabel name (flip (EApp ()) args)
--
-- relabel :: Name -> (Expr t -> Expr t) -> Expr t -> Expr t
-- relabel name f = modExpr
--   where
--     modExpr =
--       \case
--         e@(EVar (Label _ var))
--           | var == name -> f e
--           | otherwise   -> e
--
--         ELet vs e1
--           | name `matchesAnyLabel` (fst <$> vs) -> ELet vs e1
--           | otherwise                           -> ELet (second modExpr <$> vs) (modExpr e1)
--
--         ELam lls e1
--           | name `matchesAnyLabel` lls -> ELam lls e1
--           | otherwise                  -> ELam lls (modExpr e1)
--
--         ELit prim     -> ELit prim
--         EIf e1 e2 e3  -> EIf (modExpr e1) (modExpr e2) (modExpr e3)
--         EApp t e1 es  -> EApp t (modExpr e1) (modExpr <$> es)
--         EOp1 op e1    -> EOp1 op (modExpr e1)
--         EOp2 op e1 e2 -> EOp2 op (modExpr e1) (modExpr e2)
--         EPat e1 cs    -> EPat (modExpr e1) (modClause <$> cs)
--
--     modClause (Clause lls e)
--       | name `matchesAnyLabel` lls = Clause lls e
--       | otherwise                  = Clause lls (modExpr e)
--
-- --
--
-- insertImplicitArgs :: Expr Type -> Expr Type
-- insertImplicitArgs = flatten . addArgs
--   where
--     addArgs =
--       \case
--         EApp t e1 es | arity t > 0 ->
--           let ts = unfoldType t
--               vs = zip (NonEmpty.init ts) [ 0 :: Int .. ]
--            in case nonEmpty [ Label u ("$." <> showt i) | (u, i) <- vs ] of
--                 Nothing -> error "Implementation error"
--                 Just lls -> ELam lls (EApp (NonEmpty.last ts) (addArgs e1) (es <> (EVar <$> lls)))
--         e -> overExpr addArgs e
--
-- --
--
-- flattenLets :: Expr t -> Expr t
-- flattenLets =
--   \case
--     ELet ((Label _ name, e1@EVar{}) :| []) e2 ->
--       flattenLets (relabel name (const e1) e2)
--     e ->
--       overExpr flattenLets e
--
-- data Definition a = Def !Name ![Label Type] !a
--   deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
--
-- liftLambdas :: Expr Type -> [Definition (Expr Type)]
-- liftLambdas expr = Def "_" [] (flattenLets main) : flattenLets <$$> fs
--   where
--     (main, fs) = evalState (runWriterT (runLift expr)) 0
--     runLift =
--       \case
--         e@(ELam lls e1) -> do
--           name <- suffixed "$fun"
--           tell [Def name (NonEmpty.toList lls) e1]
--           pure (EVar (Label (typeOf e) name))
--         e ->
--           overExprM runLift e
--
-- toLExprs :: [Definition (Expr Type)] -> [Definition LExpr]
-- toLExprs = (toLExpr <$$>)
--
-- -- --------------------
-- -- -- Eval
-- -- --------------------
--
-- type ValueEnv = Environment (Either ([Label Type], LExpr) Value)
--
-- newtype Eval a = Eval { unEval :: ReaderT ValueEnv IO a }
--   deriving (Functor, Applicative, Monad, MonadReader ValueEnv, MonadIO)
--
-- data Value
--   = VPrim !Prim
--   | VData !Name ![Value]
--   | VFunc !Name
--   deriving (Show, Eq, Ord, Read)
--
-- eval :: LExpr -> Eval Value
-- eval =
--   \case
--     LVar (Label _ name) -> do
--       env <- ask
--       case envLookup name env of
--         Just (Left{})    -> pure (VFunc name)
--         Just (Right val) -> pure val
--         Nothing          -> error ("Not in scope: " <> Text.unpack name)
--     LLit prim ->
--       pure (VPrim prim)
--     LLet (Label _ name) e1 e2 -> do
--       val <- eval e1
--       local (envInsert name (Right val)) (eval e2)
--     LCon (Label _ con) es ->
--       VData con <$> traverse eval es
--     LCal (Label _ fun) es -> do
--       as <- traverse eval es
--       env <- ask
--       case envLookup fun env of
--         Just (Left def) -> evalFun def as
--         Just (Right (VFunc val)) ->
--           case envLookup val env of
--             Just (Left def) -> evalFun def as
--             _ -> error "TODO"
--         _ -> error "Implementation error"
--     LOp1 (_, op) e1 -> do
--       a1 <- eval e1
--       case a1 of
--         VPrim p1 -> pure (VPrim (evalOp1 op p1))
--         _        -> error "Invalid operand"
--     LOp2 (_, op) e1 e2 -> do
--       a1 <- eval e1
--       a2 <- eval e2
--       case (a1, a2) of
--         (VPrim p1, VPrim p2) -> pure (VPrim (evalOp2 op p1 p2))
--         _                    -> error "Invalid operand"
--     LPat e1 cs -> do
--       a1 <- eval e1
--       evalPat a1 (NonEmpty.toList cs)
--
-- evalFun :: ([Label t], LExpr) -> [Value] -> Eval Value
-- evalFun (vars, expr) args = local (envInserts (bindVars vars args)) (eval expr)
--
-- evalPat :: Value -> [([Label Type], LExpr)] -> Eval Value
-- evalPat _ [] = error "No match"
-- evalPat d@(VData con vs) ((Label _ name : lls, expr) : cs)
--   | con == name = local (envInserts (bindVars lls vs)) (eval expr)
--   | otherwise   = evalPat d cs
-- evalPat p@(VPrim (PBool bool)) (([Label _ val], expr) : cs)
--   | (val == "True" && bool) || (val == "False" && not bool) = eval expr
--   | otherwise = evalPat p cs
-- evalPat _ _ = error "TODO"
--
-- bindVars :: [Label t] -> [a] -> [(Name, Either e a)]
-- bindVars lls vs = zip (fst . unLabel <$> lls) (Right <$> vs)
--
-- evalOp1 :: Op1 -> Prim -> Prim
-- evalOp1 ONot (PBool b)   = PBool (not b)
-- evalOp1 ONeg (PFloat p)  = PFloat (negate p)
-- evalOp1 ONeg (PDouble p) = PDouble (negate p)
-- evalOp1 ONeg (PInt n)    = PInt (negate n)
-- evalOp1 _ _              = error "Not implemented"
--
-- evalOp2 :: Op2 -> Prim -> Prim -> Prim
-- evalOp2 OAdd (PFloat p) (PFloat q) = PFloat (p + q)
-- evalOp2 OMul (PFloat p) (PFloat q) = PFloat (p * q)
-- evalOp2 OSub (PFloat p) (PFloat q) = PFloat (p - q)
-- evalOp2 ODiv (PFloat p) (PFloat q) = PFloat (p / q)
-- evalOp2 OLt  (PFloat p) (PFloat q) = PBool (p < q)
-- evalOp2 OGt  (PFloat p) (PFloat q) = PBool (p > q)
-- evalOp2 OLtE (PFloat p) (PFloat q) = PBool (p <= q)
-- evalOp2 OGtE (PFloat p) (PFloat q) = PBool (p >= q)
-- evalOp2 OAdd (PDouble p) (PDouble q) = PDouble (p + q)
-- evalOp2 OMul (PDouble p) (PDouble q) = PDouble (p * q)
-- evalOp2 OSub (PDouble p) (PDouble q) = PDouble (p - q)
-- evalOp2 ODiv (PDouble p) (PDouble q) = PDouble (p / q)
-- evalOp2 OLt  (PDouble p) (PDouble q) = PBool (p < q)
-- evalOp2 OGt  (PDouble p) (PDouble q) = PBool (p > q)
-- evalOp2 OLtE (PDouble p) (PDouble q) = PBool (p <= q)
-- evalOp2 OGtE (PDouble p) (PDouble q) = PBool (p >= q)
-- evalOp2 OEq  (PInt m) (PInt n) = PBool (m == n)
-- evalOp2 ONEq (PInt m) (PInt n) = PBool (m /= n)
-- evalOp2 OEq  (PBool m) (PBool n) = PBool (m == n)
-- evalOp2 ONEq (PBool m) (PBool n) = PBool (m /= n)
-- evalOp2 OEq  (PFloat m) (PFloat n) = PBool (m == n)
-- evalOp2 ONEq (PFloat m) (PFloat n) = PBool (m /= n)
-- evalOp2 OEq  (PDouble m) (PDouble n) = PBool (m == n)
-- evalOp2 ONEq (PDouble m) (PDouble n) = PBool (m /= n)
-- evalOp2 OAdd (PInt m) (PInt n) = PInt (m + n)
-- evalOp2 OSub (PInt m) (PInt n) = PInt (m - n)
-- evalOp2 OMul (PInt m) (PInt n) = PInt (m * n)
-- evalOp2 OLt  (PInt p) (PInt q) = PBool (p < q)
-- evalOp2 OGt  (PInt p) (PInt q) = PBool (p > q)
-- evalOp2 OLtE (PInt p) (PInt q) = PBool (p <= q)
-- evalOp2 OGtE (PInt p) (PInt q) = PBool (p >= q)
-- evalOp2 _ _ _ = error "Not implemented"
--
-- runEval :: ValueEnv -> Eval a -> IO a
-- runEval env value = runReaderT (unEval value) env
