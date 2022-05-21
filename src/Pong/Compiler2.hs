{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Pong.Compiler2 where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Newtype.Generics
import Data.Function (on)
import Data.List (nubBy)
import Data.List.NonEmpty (fromList, toList)
import Data.Tuple.Extra (first, second, swap)
import Pong.Data
import Pong.Lang
import Pong.TypeChecker
import Pong.Util hiding (unpack)
import Pong.Util.Env (Environment(..))
import TextShow (showt)
import qualified Data.Map.Strict as Map

canonical :: MonoType -> MonoType
canonical t = apply (Substitution map) t
  where
    map = Map.fromList (free t `zip` (tVar <$> [0 ..]))

isomorphic :: MonoType -> MonoType -> Bool
isomorphic t0 t1 = canonical t0 == canonical t1

-- TODO: keep?
--combineLambdas :: Expr t a0 a1 a2 -> Expr t a0 a1 a2
--combineLambdas =
--  cata $
--    \case
--      ELam t xs (Fix (ELam _ ys expr)) ->
--        eLam t (xs <> ys) expr
--      e ->
--        embed e

appArgs :: [Ast] -> Ast -> Ast
appArgs [] =
  id
appArgs xs =
  project
    >>> ( \case
            ECall _ f ys ->
              eCall f (ys <> xs)
            EVar v ->
              eCall v xs
            e ->
              error "Implementation error"
        )

uniqueName :: (MonadState (Int, a) m) => Name -> m Name
uniqueName prefix = do
  (n, _) <- getAndModify (first succ)
  pure (prefix <> showt n)

extra :: MonoType -> [Label MonoType]
extra = tail . varSequence "$v" . argTypes

programDefs :: (MonadState (Int, Program Scheme t a) m) => m [Name]
programDefs = snd <$$> gets (Map.keys . unpack . snd)

liftDef ::
  (MonadState (Int, Program Scheme MonoType Ast) m) =>
  Name ->
  [Label MonoType] ->
  [Label MonoType] ->
  Ast ->
  m Ast
liftDef name vs args expr = do
  let ty = foldType t ts
  insertDef (toScheme "a" (free ty) ty, name) def
  pure (eCall (typeOf def, name) (eVar <$> vs))
  where
    t = typeOf expr
    ts = fst <$> as
    as = vs <> args `without` [(t, name)]
    def = Function (fromList as) (t, expr)

makeDef ::
  (MonadState (Int, Program Scheme MonoType Ast) m) =>
  Name ->
  Ast ->
  ((Ast -> Ast) -> Ast) ->
  m Ast
makeDef name expr f =
  if isConT ArrT t
    then do
      defs <- programDefs
      ndef <- uniqueName name
      let vs = freeVars expr `exclude` defs 
          ys = extra t
      liftDef ndef vs ys (f $ appArgs (eVar <$> ys))
    else 
      pure expr
  where
    t = typeOf expr

-- --specializeFun 
-- --  :: (MonadState (Int, a) m) 
-- --  => Label (Type Int Name)
-- --  -> [Label (Type Int Name)]
-- --  -> (Type Int Name, TypedExpr)
-- --  -> m TypedExpr
-- --specializeFun =
-- --  undefined
-- 
-- instantiateDef 
--   :: (MonadState (Int, a) m) 
--   => (ProgKey (Type Int Name), Definition (Type Int Name) SourceExpr)
--   -> m (ProgKey MonoType, Definition MonoType SourceExpr)
-- instantiateDef = 
--   \case
--     (_, Function args (_, body)) -> do
-- --      ts <- traverse (\n -> tag >>= \v -> pure (n, tVar v)) (Set.toList (boundVars t))
-- --      let sub = toMonoType (Map.fromList ts)
--       undefined
-- 
-- specializeDef
--   :: (MonadState (Int, a) m) 
--   => Label (Type Int Name)
--   -> Definition (Type Int Name) (Expr (Type Int Name) (Type Int Name) () Void)
--   -> TypedExpr 
--   -> m TypedExpr
-- specializeDef (t, name) def expr = do
--   ts <- traverse (\n -> tag >>= \v -> pure (n, tVar v)) (Set.toList (boundVars t))
--   let sub = toMonoType (Map.fromList ts)
--   case def of
--     Function args (_, body) -> do
--       let e1 = eLam () (toList (first sub <$> args)) (mapTypes sub body)
--           t1 = sub t
--       (e, binds) <- runWriterT (replaceTemplates t1 name e1 expr)
--       --traceShowM "***"
--       --traceShowM (unique binds)
--       --traceShowM "***"
--       --pure (foldr (uncurry eLet) e (((t1, name), e1) : unique binds))
--       pure (foldr (uncurry eLet) e (unique binds))
-- 
-- --      specializeDef     
-- --        (sub t, name)
-- --        (toList (first sub <$> args))
-- --        (sub t0, mapTypes sub body)
-- --        expr

specializeDef 
  :: (MonadState (Int, a) m) 
  => Label MonoType
  -> [Label MonoType] 
  -> (MonoType, TypedExpr) 
  -> TypedExpr 
  -> m TypedExpr
specializeDef (t, name) args (_, body) e2 = do
  (e, binds) <- runWriterT (xreplaceTemplates t name e1 e2)
  pure (foldr (uncurry eLet) e binds)
  where 
    e1 = eLam () args body

-- | Predicate to test if the type contains at least one type variable
isTemplate :: Type v s -> Bool
isTemplate =
  cata $
    \case
      TVar {} -> True
      TArr t1 t2 -> t1 || t2
      TCon _ ts -> or ts
      TRow row ->
        ( (`cata` row) $
            \case
              RExt _ t r -> isTemplate t || r
              _ -> False
        )
      _ -> 
        False

xspecializeLets :: (MonadState (Int, a) m) => TypedExpr -> m TypedExpr
xspecializeLets =
  cata
    ( \case
        ELet (t, var) expr1 expr2 | isTemplate t -> do
          e1 <- expr1
          e2 <- expr2
          (e, binds) <- runWriterT (xreplaceTemplates t var e1 e2)
          pure (foldr (uncurry eLet) e (((t, var), e1) : unique binds))
        expr ->
          embed <$> sequence expr
    )

unique :: [((MonoType, a1), a2)] -> [((MonoType, a1), a2)]
unique = nubBy (on (==) (fst . fst))

xreplaceTemplates ::
  (MonadState (Int, a) m, MonadWriter [(Label MonoType, TypedExpr)] m) =>
  MonoType ->
  Name ->
  TypedExpr ->
  TypedExpr ->
  m TypedExpr
xreplaceTemplates t name e1 =
  para
    ( \case
        ELet (t0, var) (expr1, _) (expr2, _) | var == name ->
          pure (eLet (t0, var) expr1 expr2)
        EVar (t0, var) | var == name && not (isomorphic t t0) ->
          case getSubst t t0 of
            Right sub -> do
              newVar <- uniqueName ("$var_" <> var <> "_")
              tell [((t0, newVar), apply sub e1)]
              pure (eVar (t0, newVar))
            _ ->
              error "Implementation error"
        expr -> 
          embed <$> sequence (expr <&> snd)
    )
  where
    getSubst t1 t2 = 
      xrunTypeChecker'' (freeIndex [t1, t2]) mempty (unify t1 t2)

xrunTypeChecker'' :: Int -> TypeEnv -> TypeChecker a -> Either TypeError a
xrunTypeChecker'' n env m = evalState (runReaderT (runExceptT (unpack m)) env) (n, mempty)

--runTypeChecker'' :: Int -> Environment (Type Int Name) -> TypeChecker a -> Either TypeError a
--runTypeChecker'' n env m = evalState (runReaderT (runExceptT (unpack m)) env) (n, mempty)

--basfddd t1 t2 = runTypeChecker'' (freeIndex [t1, t2]) mempty (unify t1 t2)

exclude :: [Label s] -> [Name] -> [Label s]
exclude = foldr (\label -> filter ((/=) label . snd)) 

compile 
  :: (MonadState (Int, Program Scheme MonoType Ast) m) 
  => TypedExpr 
  -> m Ast
compile =
  cata $ \case
    ELam t args expr1 -> do
      e1 <- expr1
      defs <- programDefs
      ndef <- uniqueName "$lam"
      let vs = freeVars e1 `exclude` ((snd <$> args) <> defs)
          ys = extra (typeOf e1)
      liftDef ndef vs (args <> ys) (appArgs (eVar <$> ys) e1)
    ECase expr1 clauses -> do
      e1 <- expr1
      cs <- traverse sequence clauses
      makeDef "$match" (eCase e1 cs) (\app -> eCase e1 (second app <$> cs))
    EIf expr1 expr2 expr3 -> do
      e1 <- expr1
      e2 <- expr2
      e3 <- expr3
      makeDef "$if" (eIf e1 e2 e3) (\app -> eIf e1 (app e2) (app e3))
    EApp t fun args -> do
      f <- fun
      xs <- sequence args
      pure
        ( case project f of
            ECall _ g ys -> eCall g (ys <> xs)
            EVar v -> eCall v xs
        )
    ELet var expr1 expr2 -> do
      e1 <- expr1
      e2 <- expr2
      makeDef "$let" (eLet var e1 e2) (\app -> eLet var e1 (app e2))
    EField fs expr1 expr2 -> do
      e1 <- expr1
      e2 <- expr2
      makeDef "$field" (eField fs e1 e2) (\app -> eField fs e1 (app e2))
    ECon con ->
      pure (eCall con [])
    EOp1 op a1 ->
      eOp1 op <$> a1
    EOp2 op a1 a2 ->
      eOp2 op <$> a1 <*> a2
    EVar v -> pure (eVar v)
    ELit l -> pure (eLit l)
    ERow row ->
      eRow <$> mapRowM compile row
