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
import Data.Tuple.Extra (first, second, swap, secondM)
import Debug.Trace
import Pong.Data
import Pong.Lang
import Pong.Type
import Pong.Util hiding (unpack)
import Pong.Util.Env (Environment(..))
import TextShow (showt)
import qualified Pong.Util.Env as Env
import qualified Data.Map.Strict as Map

-- from:
--   foo = lam(a) => lam(b) => b
--   baz(x) = lam(a) => lam(b) => b
--
-- to:
--   foo(a, b) = b
--   baz(x, a, b) = b
hoistTopLambdas 
  :: Definition MonoType (Expr MonoType a0 a1 a2) 
  -> Definition MonoType (Expr MonoType a0 a1 a2)
hoistTopLambdas =
  \case
    Function args (t, expr)
      | isConE LamE expr -> combine t (toList args) expr
    Constant (t, expr)
      | isConE LamE expr -> combine t [] expr
    def -> def
  where
    combine t as =
      combineLambdas >>> project >>> 
        \case
          ELam _ bs expr -> Function (fromList (as <> bs)) (returnType t, expr)
          _ -> error "Implementation error"

appArgs :: [Ast] -> Ast -> Ast
appArgs [] = id
appArgs xs =
  para 
    ( \case
        EVar f ->
          eCall f xs
        EIf (e1, _) (_, e2) (_, e3) ->
          eIf e1 e2 e3
        ELet var (e1, _) (_, e2) ->
          eLet var e1 e2
        ECall _ f ys ->
          eCall f ((fst <$> ys) <> xs)
        EPat (e1, _) cs ->
          ePat e1 (snd <$$> cs)
        ERes fs (e1, _) (_, e2) ->
          eRes fs e1 e2
        e ->
          error "Implementation error"
    )

-- from : def f(x : int) : int -> int = add(x)
--
-- to   : def f(x : int, v_0 : int) : int = add(x, v_0)
--
normalizeDefs :: Definition MonoType Ast -> Definition MonoType Ast
normalizeDefs = \case
  Function args (t, expr) | isConT ArrT t ->
    fun t (toList args) expr 
  Constant (t, expr) | isConT ArrT t ->
    fun t [] expr 
  def ->
    def
  where
    fun t xs expr = 
      let 
        ys = extra t
      in
        Function (fromList (xs <> ys)) (returnType t, appArgs (eVar <$> ys) expr)

normalizeProgramDefs :: Program MonoType Ast -> Program MonoType Ast 
normalizeProgramDefs = over Program (Map.map normalizeDefs) 

canonical :: MonoType -> MonoType
canonical t = apply (Substitution map) t
  where
    map = Map.fromList (free t `zip` (tVar <$> [0 ..]))

isIsomorphicTo :: MonoType -> MonoType -> Bool
isIsomorphicTo t0 t1 = canonical t0 == canonical t1

combineLambdas :: Expr t a0 a1 a2 -> Expr t a0 a1 a2
combineLambdas =
  cata $
    \case
      ELam t xs (Fix (ELam _ ys expr)) ->
        eLam t (xs <> ys) expr
      e ->
        embed e

--appArgs :: [Ast] -> Ast -> Ast
--appArgs [] =
--  id
--appArgs xs =
--  project
--    >>> ( \case
--            ECall _ f ys ->
--              eCall f (ys <> xs)
--            EVar f ->
--              eCall f xs
--            e ->
--              error "Implementation error"
--        )

uniqueName :: (MonadState (Int, a) m) => Name -> m Name
uniqueName prefix = do
  (n, _) <- getAndModify (first succ)
  pure (prefix <> showt n)

extra :: MonoType -> [Label MonoType]
extra t  
  | null ts = []
-- | otherwise = tail (varSequence "$v" ts)
  | otherwise = varSequence "$v" ts
  where
    ts = argTypes t

programDefs :: (MonadReader TypeEnv m, MonadState (Int, Program t a) m) => m [Name]
programDefs = do
  xx <- gets (Map.keys . Map.mapKeys snd . unpack . snd)
  yy <- ask
  pure (xx <> (fst <$> Env.toList yy))

liftDef ::
  (MonadState (Int, Program MonoType Ast) m) =>
  Name ->
  [Label MonoType] ->
  [Label MonoType] ->
  Ast ->
  m Ast
liftDef name vs args expr = do
  let ty = foldType t ts
  insertDef (toScheme "a" (free ty) ty, name) def
  --pure (eCall (typeOf def, name) (eVar <$> vs))
  --pure (eCall (typeOf def, name) (eVar <$> as))
  pure (eVar (ty, name))
  where
    t = typeOf expr
    ts = fst <$> as
    as = vs <> args `without` [(t, name)]
    def = Function (fromList as) (t, expr)

makeDef ::
  (MonadReader TypeEnv m, MonadState (Int, Program MonoType Ast) m) =>
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

specializeDef 
  :: (MonadState (Int, a) m) 
  => Label MonoType
  -> [Label MonoType] 
  -> (MonoType, TypedExpr) 
  -> TypedExpr 
  -> m TypedExpr
specializeDef (t, name) args (_, body) e2 = do
  (e, binds) <- runWriterT (xreplacePolymorphics t name (eLam () args body) e2)
  pure (foldr (uncurry eLet) e binds)

-- | Predicate to test if the type contains at least one type variable
isPolymorphic :: Type v s -> Bool
isPolymorphic =
  cata $
    \case
      TVar {} -> True
      TArr t1 t2 -> t1 || t2
      TCon _ ts -> or ts
      TRow row ->
        ( (`cata` row) $
            \case
              RExt _ t r -> isPolymorphic t || r
              _ -> False
        )
      _ -> 
        False

monomorphizeLets :: (MonadState (Int, a) m) => TypedExpr -> m TypedExpr
monomorphizeLets =
  cata
    ( \case
        ELet (t, var) expr1 expr2 | isPolymorphic t -> do
          e1 <- expr1
          e2 <- expr2
          (e, binds) <- runWriterT (xreplacePolymorphics t var e1 e2)
          -- pure (foldr (uncurry eLet) e (((t, var), e1) : unique binds))
          pure (foldr (uncurry eLet) e (unique binds))
        expr ->
          embed <$> sequence expr
    )

unique :: [((MonoType, a1), a2)] -> [((MonoType, a1), a2)]
unique = nubBy (on (==) (fst . fst))

xreplacePolymorphics ::
  (MonadState (Int, a) m, MonadWriter [(Label MonoType, TypedExpr)] m) =>
  MonoType ->
  Name ->
  TypedExpr ->
  TypedExpr ->
  m TypedExpr
xreplacePolymorphics t name e1 =
  para
    ( \case
        ELet (t0, var) (expr1, _) (expr2, _) | var == name ->
          pure (eLet (t0, var) expr1 expr2)
        EVar (t0, var) | var == name && not (t `isIsomorphicTo` t0) ->
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
      evalTypeChecker (freeIndex [t1, t2]) mempty (unifyTypes t1 t2)

exclude :: [Label s] -> [Name] -> [Label s]
exclude = foldr (\label -> filter ((/=) label . snd)) 

compile 
  :: (MonadReader TypeEnv m, MonadState (Int, Program MonoType Ast) m) 
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
    EPat expr1 clauses -> do
      e1 <- expr1
      cs <- traverse sequence clauses
      makeDef "$match" (ePat e1 cs) (\app -> ePat e1 (second app <$> cs))
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
    ERes fs expr1 expr2 -> do
      e1 <- expr1
      e2 <- expr2
      makeDef "$res" (eRes fs e1 e2) (\app -> eRes fs e1 (app e2))
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

compileProgram :: Program MonoType TypedExpr -> Program MonoType Ast
compileProgram p = evalState run (1, emptyProgram)
  where
    compileDefs = (`programForM` const (traverse compile))
    run = do
      q <- runReaderT (compileDefs p) (programEnv p)
      (_, r) <- get
      pure (q <> r)
