{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Pong.Compiler where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Newtype.Generics
import Data.Either.Extra (mapLeft)
import Data.Function (on)
import Data.List (nubBy)
import Data.List.NonEmpty (fromList, toList)
import Data.Tuple.Extra (first, second)
import Pong.Data
import Pong.Lang
import Pong.Parser
import Pong.TypeChecker
import Pong.Util hiding (unpack)
import TextShow (showt)
import qualified Data.Map.Strict as Map
import qualified Pong.Util.Env as Env

foo_canonical :: MonoType -> MonoType
foo_canonical t = apply (Substitution map) t
  where
    map = Map.fromList (free t `zip` (tVar <$> [0 ..]))

foo_isIsomorphicTo :: MonoType -> MonoType -> Bool
foo_isIsomorphicTo t0 t1 = foo_canonical t0 == foo_canonical t1

-- | Predicate to test if the type contains at least one type variable
foo_isPolymorphic :: Type v s -> Bool
foo_isPolymorphic =
  cata
    ( \case
        TVar{} -> True
        TArr t1 t2 -> t1 || t2
        TCon _ ts -> or ts
        TRow row ->
          ( (`cata` row) $
              \case
                RExt _ t r -> foo_isPolymorphic t || r
                _ -> False
          )
        _ ->
          False
    )

monomorphize ::
  (MonadState (Int, a) m, MonadWriter [(Label MonoType, TypedExpr)] m) =>
  MonoType ->
  Name ->
  TypedExpr ->
  TypedExpr ->
  m TypedExpr
monomorphize t name e1 =
  para
    ( \case
        ELet (t0, var) (expr1, _) (expr2, _) | var == name ->
          pure (eLet (t0, var) expr1 expr2)
        EVar (t0, var) | var == name && not (t `foo_isIsomorphicTo` t0) ->
          case getSubst t t0 of
            Right sub -> do
              newVar <- foo_uniqueName ("$var_" <> var <> "_")
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

foo_monomorphizeDef 
  :: (MonadState (Int, a) m) 
  => Label MonoType
  -> [Label MonoType] 
  -> (MonoType, TypedExpr) 
  -> TypedExpr 
  -> m TypedExpr
foo_monomorphizeDef (t, name) args (_, body) expr = do
  (e, binds) <- runWriterT (monomorphize t name (eLam () args body) expr)
  pure (foldr (uncurry eLet) e binds)

foo_monomorphizeLets :: (MonadState (Int, a) m) => TypedExpr -> m TypedExpr
foo_monomorphizeLets =
  cata
    ( \case
        ELet (t, var) expr1 expr2 | foo_isPolymorphic t -> do
          e1 <- expr1
          e2 <- expr2
          (e, binds) <- runWriterT (monomorphize t var e1 e2)
          pure (foldr (uncurry eLet) e (unique binds))
        expr ->
          embed <$> sequence expr
    )
  where
    unique = nubBy (on (==) (fst . fst))

foo_appArgs :: [Ast] -> Ast -> Ast
foo_appArgs [] = id
foo_appArgs xs =
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
        _ ->
          error "Implementation error"
    )

foo_exclude :: [Label s] -> [Name] -> [Label s]
foo_exclude = foldr (\label -> filter ((/=) label . snd))

foo_extra :: MonoType -> [Label MonoType]
foo_extra t
  | null ts = []
  | otherwise = varSequence "$v" ts
  where
    ts = argTypes t

foo_programDefs :: (MonadReader TypeEnv m, MonadState (Int, Program t a) m) => m [Name]
foo_programDefs = do
  ns <- gets (Map.keys . Map.mapKeys snd . unpack . snd)
  env <- ask
  pure (ns <> (fst <$> Env.toList env))

foo_uniqueName :: (MonadState (Int, a) m) => Name -> m Name
foo_uniqueName prefix = do
  n <- getAndModify (first succ) <&> fst
  pure (prefix <> showt n)

foo_liftDef ::
  (MonadState (Int, Program MonoType Ast) m) =>
  Name ->
  [Label MonoType] ->
  [Label MonoType] ->
  Ast ->
  m Ast
foo_liftDef name vs args expr = do
  let ty = foldType t ts
  insertDef (toScheme "a" (free ty) ty, name) def
  pure (eVar (ty, name))
  where
    t = typeOf expr
    ts = fst <$> as
    as = vs <> args `without` [(t, name)]
    def = Function (fromList as) (t, expr)

foo_makeDef ::
  (MonadReader TypeEnv m, MonadState (Int, Program MonoType Ast) m) =>
  Name ->
  Ast ->
  ((Ast -> Ast) -> Ast) ->
  m Ast
foo_makeDef name expr f =
  if isConT ArrT t
    then do
      defs <- foo_programDefs
      ndef <- foo_uniqueName name
      let vs = freeVars expr `foo_exclude` defs
          ys = foo_extra t
      foo_liftDef ndef vs ys (f $ foo_appArgs (eVar <$> ys))
    else 
      pure expr
  where
    t = typeOf expr

foo_compile ::
  (MonadReader TypeEnv m, MonadState (Int, Program MonoType Ast) m) =>
  TypedExpr ->
  m Ast
foo_compile =
  cata $ \case
    ELam t args expr1 -> do
      e1 <- expr1
      defs <- foo_programDefs
      ndef <- foo_uniqueName "$lam"
      let vs = freeVars e1 `foo_exclude` ((snd <$> args) <> defs)
          ys = foo_extra (typeOf e1)
      foo_liftDef ndef vs (args <> ys) (foo_appArgs (eVar <$> ys) e1)
    EPat expr1 clauses -> do
      e1 <- expr1
      cs <- traverse sequence clauses
      foo_makeDef "$match" (ePat e1 cs) (\app -> ePat e1 (second app <$> cs))
    EIf expr1 expr2 expr3 -> do
      e1 <- expr1
      e2 <- expr2
      e3 <- expr3
      foo_makeDef "$if" (eIf e1 e2 e3) (\app -> eIf e1 (app e2) (app e3))
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
      foo_makeDef "$let" (eLet var e1 e2) (\app -> eLet var e1 (app e2))
    ERes fs expr1 expr2 -> do
      e1 <- expr1
      e2 <- expr2
      foo_makeDef "$res" (eRes fs e1 e2) (\app -> eRes fs e1 (app e2))
    ECon con ->
      pure (eCall con [])
    EOp1 op a1 ->
      eOp1 op <$> a1
    EOp2 op a1 a2 ->
      eOp2 op <$> a1 <*> a2
    EVar v -> pure (eVar v)
    ELit l -> pure (eLit l)
    ERow row ->
      eRow <$> mapRowM foo_compile row

foo_compileProgram :: Program MonoType TypedExpr -> Program MonoType Ast
foo_compileProgram p = evalState run (1, emptyProgram)
  where
    compileDefs = (`programForM` const (traverse foo_compile))
    run = do
      q <- runReaderT (compileDefs p) (programEnv p)
      (_, r) <- get
      pure (q <> r)

foo_combineLambdas :: Expr t a0 a1 a2 -> Expr t a0 a1 a2
foo_combineLambdas =
  cata $
    \case
      ELam t xs (Fix (ELam _ ys expr)) ->
        eLam t (xs <> ys) expr
      e ->
        embed e

foo_hoistTopLambdas ::
  Definition MonoType (Expr MonoType a0 a1 a2) ->
  Definition MonoType (Expr MonoType a0 a1 a2)
foo_hoistTopLambdas =
  \case
    Function args (t, expr)
      | isConE LamE expr -> combine t (toList args) expr
    Constant (t, expr)
      | isConE LamE expr -> combine t [] expr
    def -> def
  where
    combine t as =
      foo_combineLambdas 
        >>> project
          >>> \case
            ELam _ bs expr -> 
              Function (fromList (as <> bs)) (returnType t, expr)
            _ -> error "Implementation error"

foo_normalizeDefs :: Definition MonoType Ast -> Definition MonoType Ast
foo_normalizeDefs = \case
  Function args (t, expr) | isConT ArrT t ->
    fun t (toList args) expr 
  Constant (t, expr) | isConT ArrT t ->
    fun t [] expr 
  def ->
    def
  where
    fun t xs expr = 
      let 
        ys = foo_extra t
      in
        Function (fromList (xs <> ys)) (returnType t, foo_appArgs (eVar <$> ys) expr)

foo_normalizeProgramDefs :: Program MonoType Ast -> Program MonoType Ast 
foo_normalizeProgramDefs = over Program (Map.map foo_normalizeDefs) 

runTransform :: State (Int, ()) a -> a
runTransform = flip evalState (1, ())

transform1 :: Program MonoType TypedExpr -> Program MonoType TypedExpr
transform1 p = runTransform (programForM p (const (traverse foo_monomorphizeLets . foo_hoistTopLambdas)))

transform2 :: Program MonoType TypedExpr -> Program MonoType TypedExpr
transform2 p = runTransform (programForM p (const (traverse (flip (foldDefsM go) p))))
  where
    go ((_, name), def) e =
      case def of
        Function args body ->
          let as = toList args
           in foo_monomorphizeDef (foldType (fst body) (fst <$> as), name) as body e
        _ ->
          pure e

data CompilerError 
  = ParserError ParserError
  | TypeError TypeError

foo_compileSource :: Text -> Program MonoType Ast
foo_compileSource input = 
  case parseAndAnnotate input of
    Left e -> error "TODO"
    Right p ->
      foo_normalizeProgramDefs (foo_compileProgram (transform2 (transform1 p)))
  where
    parseAndAnnotate =
      mapLeft ParserError . parseProgram 
         >=> mapLeft TypeError . runInferProgram
