{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Pong.Tree where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Newtype.Generics (over, unpack)
import Data.Char (isUpper)
import Data.Either.Extra (mapLeft)
import Data.List.NonEmpty (fromList, toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra (first, second)
import Pong.Data
import Pong.Lang
import Pong.Read (ParserError, parseProgram)
import Pong.Type
import Pong.Util hiding (unpack)
import qualified Pong.Util.Env as Env
import TextShow (showt)

canonical :: (Substitutable a, Free a) => a -> a
canonical t = apply (Substitution map_) t
  where
    map_ = Map.fromList (free t `zip` (tVar <$> [0 ..]))

isIsomorphicTo :: (Eq a, Substitutable a, Free a) => a -> a -> Bool
isIsomorphicTo t0 t1 = canonical t0 == canonical t1

-- | Predicate to test if the type contains at least one type variable
isPolymorphic :: Type v -> Bool
isPolymorphic =
  cata
    ( \case
        TVar{} -> True
        TArr t1 t2 -> t1 || t2
        TCon _ ts -> or ts
        TRec row ->
          ( (`cata` row) $
              \case
                RExt _ t r -> isPolymorphic t || r
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
        ELet (t0, var) (expr1, _) (expr2, _)
          | var == name ->
              pure (eLet (t0, var) expr1 expr2)
        ERes r@[_, (_, var), _] (expr1, _) (expr2, _)
          | var == name ->
              pure (eRes r expr1 expr2)
        EPat (_, expr1) cs ->
          let updateClause (ps, e)
                | name `elem` (snd <$> ps) = pure (ps, fst e)
                | otherwise = (,) ps . snd <$> sequence e
           in ePat <$> expr1 <*> traverse updateClause cs
        ECon (t0, con)
          | con == name && not (t `isIsomorphicTo` t0) ->
              eCon <$> runSubst "C$" con e1 t t0
        EVar (t0, var)
          | var == name && not (t `isIsomorphicTo` t0) ->
              eVar <$> runSubst "v$" var e1 t t0
        expr ->
          embed <$> sequence (expr <&> snd)
    )
  where
    runSubst pfix var expr t1 t2 =
      case evalTypeChecker (freeIndex [t1, t2]) mempty (unifyTypes t1 t2) of
        Right sub -> do
          newVar <- uniqueName (pfix <> "-" <> var <> "-")
          tell [((t2, newVar), apply sub expr)]
          pure (t2, newVar)
        _ ->
          error "Implementation error"

monomorphizeDef ::
  (MonadState (Int, a) m) =>
  Label MonoType ->
  [Label MonoType] ->
  (MonoType, TypedExpr) ->
  TypedExpr ->
  m TypedExpr
monomorphizeDef (t, name) args (_, body) expr = do
  (e, binds) <- runWriterT (monomorphize t name (eLam () args body) expr)
  pure (foldr (uncurry eLet) e binds)

monomorphizeLets :: (MonadState (Int, a) m) => TypedExpr -> m TypedExpr
monomorphizeLets =
  cata
    ( \case
        ELet (t, var) expr1 expr2 | isPolymorphic t -> do
          e1 <- expr1
          e2 <- expr2
          (e, binds) <- runWriterT (monomorphize t var e1 e2)
          pure (foldr (uncurry eLet) (eLet (t, var) e1 e) binds)
        expr ->
          embed <$> sequence expr
    )

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
        _ ->
          error "Implementation error"
    )

exclude :: [Label t] -> [Name] -> [Label t]
exclude = flip withoutLabels

extra :: MonoType -> [Label MonoType]
extra t
  | null ts = []
  | otherwise = varSequence "$v" ts
  where
    ts = argTypes t

programDefs :: (MonadReader TypeEnv m, MonadState (Int, Program t a) m) => m [Name]
programDefs = do
  ns <- gets (Map.keys . Map.mapKeys snd . unpack . snd)
  env <- ask
  pure (ns <> (fst <$> Env.toList env))

uniqueName :: (MonadState (Int, a) m) => Name -> m Name
uniqueName prefix = do
  n <- getAndModify (first succ) <&> fst
  pure (prefix <> showt n)

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
  pure (appArgs (eVar <$> vs) (eVar (ty, name)))
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
    else pure expr
  where
    t = typeOf expr

compile ::
  (MonadReader TypeEnv m, MonadState (Int, Program MonoType Ast) m) =>
  TypedExpr ->
  m Ast
compile =
  cata $ \case
    ELam _ args expr1 -> do
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
    EApp _ fun args -> do
      f <- fun
      xs <- sequence args
      pure
        ( case project f of
            ECall _ g ys -> eCall g (ys <> xs)
            EVar v -> if null xs then eVar v else eCall v xs
            _ -> error "Implementation error"
        )
    ELet var expr1 expr2 -> do
      e1 <- expr1
      e2 <- expr2
      case project e1 of
        EVar (_, rep) -> do
          let name_ = snd var
          rep_ <-
            if isUpper (Text.head name_)
              then do
                modify (second (renameDef rep name_))
                pure name_
              else pure rep
          let body = substituteVar name_ rep_ e2
          makeDef "$let" body ($ body)
        _ ->
          makeDef "$let" (eLet var e1 e2) (\app -> eLet var e1 (app e2))
    ERes fs expr1 expr2 -> do
      e1 <- expr1
      e2 <- expr2
      makeDef "$res" (eRes fs e1 e2) (\app -> eRes fs e1 (app e2))
    ECon con ->
      pure (eVar con)
    EOp1 op a1 ->
      eOp1 op <$> a1
    EOp2 op a1 a2 ->
      eOp2 op <$> a1 <*> a2
    EVar v -> pure (eVar v)
    ELit l -> pure (eLit l)
    ERec row ->
      eRec <$> mapRowM compile row

compileProgram :: Program MonoType TypedExpr -> Program MonoType Ast
compileProgram p = evalState run (1, emptyProgram)
  where
    compileDefs = (`programForM` const (traverse compile))
    run = do
      q <- runReaderT (compileDefs p) (programEnv p)
      (_, r) <- get
      pure (q <> r)

combineLambdas :: Expr t a0 a1 a2 -> Expr t a0 a1 a2
combineLambdas =
  cata $
    \case
      ELam t xs (Fix (ELam _ ys expr)) ->
        eLam t (xs <> ys) expr
      e ->
        embed e

hoistTopLambdas ::
  Definition MonoType (Expr MonoType a0 a1 a2) ->
  Definition MonoType (Expr MonoType a0 a1 a2)
hoistTopLambdas =
  fmap combineLambdas
    >>> \case
      Function args (t, expr)
        | isConE LamE expr -> combine t (toList args) expr
      Constant (t, expr)
        | isConE LamE expr -> combine t [] expr
      def -> def
  where
    combine t as (Fix (ELam _ bs expr)) =
      Function (fromList (as <> bs)) (returnType t, expr)
    combine _ _ _ = error "Implementation error"

normalizeDef :: Definition MonoType Ast -> Definition MonoType Ast
normalizeDef = \case
  Function args (t, expr)
    | isConT ArrT t ->
        fun t (toList args) expr
  Constant (t, expr)
    | isConT ArrT t ->
        fun t [] expr
  def ->
    def
  where
    fun t xs expr =
      let ys = extra t
       in Function (fromList (xs <> ys)) (returnType t, appArgs (eVar <$> ys) expr)

normalizeProgramDefs :: Program MonoType Ast -> Program MonoType Ast
normalizeProgramDefs = over Program (Map.map normalizeDef)

runTransform :: State (Int, ()) a -> a
runTransform = flip evalState (1, ())

transform1 :: Program MonoType TypedExpr -> Program MonoType TypedExpr
transform1 p = runTransform (programForM p (const (traverse monomorphizeLets . hoistTopLambdas)))

transform2 :: Program MonoType TypedExpr -> Program MonoType TypedExpr
transform2 p = runTransform (programForM p (const (traverse (flip (foldDefsM go) p))))
  where
    go ((_, name), def) e =
      case def of
        Function args body ->
          let as = toList args
           in monomorphizeDef (foldType (fst body) (fst <$> as), name) as body e
        _ ->
          pure e

insertConstructors ::
  Program MonoType (Expr MonoType a0 a1 a2) ->
  Program MonoType (Expr MonoType a0 a1 a2)
insertConstructors (Program p) = Program (p <> Map.fromList (concat extra_))
  where
    extra_ =
      Map.toList p
        <&> \((Scheme s, _), def) ->
          case def of
            Data _ cons ->
              cons <&> \(Constructor con fs) ->
                let t = foldType s fs
                    names = Map.fromList (Set.toList (boundVars t) `zip` [0 ..])
                    t0 = toMonoType names t
                    body = (returnType t0, eVar (t0, "{{data}}"))
                 in ( (Scheme t, con)
                    , if null fs
                        then Constant body
                        else Function (fromList (varSequence "d" (argTypes t0))) body
                    )
            _ ->
              []

data CompilerError
  = ParserError ParserError
  | TypeError TypeError

postProcess :: Program MonoType Ast -> Program MonoType Ast
postProcess p =
  over Program (Map.filterWithKey (\s _ -> snd s `elem` names)) p
  where
    names = "main" : (snd <$> (freeVars p :: [Label MonoType]))

transformProgram :: Program MonoType TypedExpr -> Program MonoType Ast
transformProgram =
  insertConstructors
    >>> transform1
    >>> transform2
    >>> compileProgram
    >>> normalizeProgramDefs
    >>> postProcess

parseAndAnnotate :: Text -> Either CompilerError (Program MonoType TypedExpr)
parseAndAnnotate =
  mapLeft ParserError . parseProgram >=> mapLeft TypeError . runInferProgram

compileSource :: Text -> Program MonoType Ast
compileSource input =
  case parseAndAnnotate input of
    Left e -> error (show e)
    Right p -> transformProgram p

-------------------------------------------------------------------------------
-- Typeclass instances
-------------------------------------------------------------------------------

deriving instance Show CompilerError

deriving instance Eq CompilerError
