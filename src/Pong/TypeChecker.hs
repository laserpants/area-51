{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Pong.TypeChecker
  ( runCheck
  ) where

import Control.Arrow ((>>>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (first, firstM)
import Pong.Lang
import qualified Pong.Util.Env as Env

class Substitutable a where
  apply :: Substitution -> a -> a

instance Substitutable Type where
  apply sub =
    cata $ \case
      TArr t1 t2 -> tArr t1 t2
      TVar n -> fromMaybe (tVar n) (getSubstitution sub !? n)
      t -> embed t

instance Substitutable Expr where
  apply sub =
    cata $ \case
      EVar t name -> var (apply sub t) name
      ELam args expr -> lam (first (apply sub) <$> args) expr
      ELet (t, name) expr1 expr2 -> let_ (apply sub t, name) expr1 expr2
      EApp t fun args -> app (apply sub t) fun args
      e -> embed e

instance (Functor f, Substitutable a) => Substitutable (f a) where
  apply = fmap . apply

instance Semigroup Substitution where
  (<>) = compose

instance Monoid Substitution where
  mempty = Substitution mempty

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 =
  Substitution
    (fmap (apply s1) (getSubstitution s2) `Map.union` getSubstitution s1)

mapsTo :: Int -> Type -> Substitution
mapsTo = Substitution <$$> Map.singleton

tagExpr :: Ast a -> Ast Int
tagExpr =
  flip evalState 1 .
  cata
    (\case
       EVar _ name -> var <$> tag <*> pure name
       ELit prim -> pure (lit prim)
       EIf e1 e2 e3 -> if_ <$> e1 <*> e2 <*> e3
       ELam args expr -> do
         ts <- replicateM (length args) tag
         lam (zip ts (snd <$> args)) <$> expr
       ELet (_, name) e1 e2 -> do
         t <- tag
         let_ (t, name) <$> e1 <*> e2
       EApp _ fun args -> app <$> tag <*> fun <*> sequence args
       EOp2 op e1 e2 -> op2 op <$> e1 <*> e2
       ECase e1 cs -> case_ <$> e1 <*> traverse sequence cs)
  where
    tag :: State Int Int
    tag = do
      s <- get
      put (succ s)
      pure s

unify :: (MonadError TypeError m) => Type -> Type -> m Substitution
unify t1 t2 =
  case (project t1, project t2) of
    (TVar n, t) -> pure (n `mapsTo` embed t)
    (t, TVar n) -> pure (n `mapsTo` embed t)
    (TArr t1 t2, TArr u1 u2) -> do
      sub1 <- unify t1 u1
      sub2 <- unify (apply sub1 t2) (apply sub1 u2)
      pure (sub2 <> sub1)
    _
      | t1 == t2 -> pure mempty
      | otherwise -> throwError UnificationError

applySubstitution :: (MonadState Substitution m, Substitutable a) => a -> m a
applySubstitution a = gets apply <*> pure a

unifyM ::
     ( MonadError TypeError m
     , MonadState Substitution m
     , Substitutable a
     , Substitutable b
     , Typed a
     , Typed b
     )
  => a
  -> b
  -> m ()
unifyM a b = do
  sub <- get
  sub1 <- unify (typeOf (apply sub a)) (typeOf (apply sub b))
  modify (sub1 <>)

runCheck :: TypeEnv -> Ast () -> Either TypeError Expr
runCheck symtab ast = apply sub <$> res
  where
    (res, sub) = runMonad (check (tagExpr ast))
    runMonad m =
      runState (runReaderT (runExceptT (getTypeChecker m)) symtab) mempty

check :: Ast Int -> TypeChecker Expr
check =
  cata $ \case
    ELet (t, name) expr1 expr2 -> do
      let insertBound e = do
            ty <- applySubstitution (tVar t)
            local (Env.insert name ty) e
      e1 <- insertBound expr1
      unifyM (tVar t) e1
      e2 <- insertBound expr2
      ty <- applySubstitution (tVar t)
      pure (let_ (ty, name) e1 e2)
    EApp t fun args -> do
      f <- fun
      xs <- sequence args
      unifyM f (foldType (tVar t) (typeOf <$> xs))
      ty <- applySubstitution (tVar t)
      pure (app ty f xs)
    ELam args expr -> do
      e <- local (insertArgs (first tVar <$> args)) expr
      xs <- traverse (firstM (applySubstitution . tVar)) args
      pure (lam xs e)
    EVar t name -> do
      Env env <- ask
      case env !? name of
        Just ty -> do
          unifyM (tVar t) ty
          pure (var ty name)
        _ -- /
         -> throwError (NotInScope name)
    ECase _ [] -- /
     -> throwError EmptyCaseStatement
    ECase expr clauses -> do
      e <- expr
      cs <- traverse (sequence <=< applySubstitution) clauses
      let t:ts = snd <$> cs
      forM_ ts (unifyM t)
      pure (case_ e cs)
    ELit prim -- / 
     -> pure (lit prim)
    EIf expr1 expr2 expr3 -> do
      e1 <- expr1
      e2 <- expr2
      e3 <- expr3
      unifyM e1 tBool
      unifyM e2 e3
      pure (if_ e1 e2 e3)
    EOp2 op expr1 expr2 -> do
      e1 <- expr1
      e2 <- expr2
      let [t1, t2, _] = unwindType (typeOf op)
      unifyM e1 t1
      unifyM e2 t2
      pure (op2 op e1 e2)
