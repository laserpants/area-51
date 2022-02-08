{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Pong.Compiler where

import Control.Applicative ((<|>))
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra (first, second, secondM)
import Data.Void
import Debug.Trace
import Pong.Lang
import Pong.TypeChecker
import qualified Pong.Util.Env as Env
import TextShow (showt)

combineLambdas :: TypedExpr -> TypedExpr
combineLambdas =
  cata $ \case
    ELam _ xs (Fix (ELam _ ys expr)) -> lam (xs <> ys) expr
    e -> embed e

convertLetBindings :: TypedExpr -> PreAst
convertLetBindings =
  cata $ \case
    ELet _ (_, name) e1 e2
      | isCon VarE e1 || isCon LitE e1 || isCon LamE e1 ->
        let alg =
              \case
                EVar (_, var)
                  | name == var -> e1
                e -> embed e
         in cata alg e2
    ELet _ bind e1 e2 -> app (lam [bind] e2) [e1]
    EVar v -> var v
    ELit prim -> lit prim
    EIf e1 e2 e3 -> if_ e1 e2 e3
    ELam _ args expr -> lam args expr
    EApp _ fun args -> app fun args
    EOp2 op e1 e2 -> op2 op e1 e2
    ECase e1 cs -> case_ e1 cs

convertClosures :: (MonadReader TypeEnv m) => PreAst -> m PreAst
convertClosures =
  cata $ \case
    EVar v -> pure (var v)
    ELit prim -> pure (lit prim)
    EIf e1 e2 e3 -> if_ <$> e1 <*> e2 <*> e3
    EApp _ fun args -> do
      fun >>=
        (project >>> \case
           EApp _ g as1 -> do
             as <- sequence args
             pure (app g (as1 <> as))
           _ -> app <$> fun <*> sequence args)
    EOp2 op e1 e2 -> op2 op <$> e1 <*> e2
    ECase e1 cs ->
      let insertNames (n:vs, expr) = do
            e <- local (insertArgs vs) expr
            pure (n : vs, e)
       in case_ <$> e1 <*> traverse insertNames cs
    ELam _ args expr -> do
      body <- local (insertArgs args) expr
      let extra = free body `without` args
          lambda = lam (extra <> args) body
      pure $
        case extra of
          [] -> lambda
          _ -> app lambda (var <$> extra)

preprocess :: (MonadReader TypeEnv m) => TypedExpr -> m PreAst
preprocess = combineLambdas >>> convertLetBindings >>> convertClosures

typeCheck :: SourceExpr t -> Compiler (Either TypeError TypedExpr)
typeCheck ast = asks (`runCheck` ast)

runCompiler :: Compiler a -> TypeEnv -> (a, Program)
runCompiler comp env = runState (getCompilerState comp env) emptyProgram

execCompiler :: Compiler a -> TypeEnv -> Program
execCompiler comp env = execState (getCompilerState comp env) emptyProgram

evalCompiler :: Compiler a -> TypeEnv -> a
evalCompiler comp env = evalState (getCompilerState comp env) emptyProgram

getCompilerState :: Compiler a -> TypeEnv -> State Program a
getCompilerState = runReaderT . getCompiler

programNames :: (MonadState Program m) => m Names
programNames = gets definitions <#> Map.keys

mapDefinitionsM :: (Definition Ast -> Compiler (Definition Ast)) -> Compiler ()
mapDefinitionsM f = mapM_ updateDefinition =<< programNames
  where
    updateDefinition name = do
      defs <- gets definitions
      f (defs ! name) >>= modify . insertDefinition name

uniqueName :: Name -> Compiler Name
uniqueName name = do
  Program {..} <- get
  put Program {count = succ count, ..}
  pure (name <> "_" <> showt count)

compileAst :: PreAst -> Compiler Ast
compileAst =
  cata $ \case
    EIf e1 e2 e3 -> if_ <$> e1 <*> e2 <*> e3
    EOp2 op a b -> op2 op <$> a <*> b
    ELam _ args expr -> do
      name <- uniqueName "def"
      body <- local (insertArgs args) expr
      let signature = Function (Signature args (typeOf body, body))
      modify (insertDefinition name signature)
      pure (var (typeOf signature, name))
    EVar v -> pure (var v)
    ELit prim -> pure (lit prim)
    ECase e1 cs -> case_ <$> e1 <*> traverse clauses cs
    EApp _ expr args -> do
      e <- expr
      as <- sequence args
      case project e of
        EVar (t1, name) -> pure (call_ (t1, name) as)
        ECall _ fun as1 -> do
          pure (call_ fun (as1 <> as))
        e -> error (show e)

clauses :: ([Label Type], Compiler Ast) -> Compiler ([Label Type], Ast)
clauses (pairs, expr) = (pairs, ) <$> local (insertArgs pairs) expr

fillParams :: Definition Ast -> Compiler (Definition Ast)
fillParams (Function (Signature arguments (ty, body)))
  | isTCon ArrT ty = do
    let applyTo xs =
          project >>> \case
            ECall _ fun args -> pure (call_ fun (args <> xs))
            EVar name -> do
              names <- gets definitions
              pure (call_ name xs)
            expr -> pure (embed expr)
    vars <- replicateM (arity ty) (uniqueName "v")
    let extra = argTypes ty `zip` vars
    newBody <- applyTo (var <$> extra) body
    pure
      (Function
         (Signature
            {arguments = arguments <> extra, body = (returnTypeOf ty, newBody)}))
fillParams def = pure def

consTypes :: (Name, Definition (Expr t a0 a1 a2 a3)) -> [(Name, Type)]
consTypes (name, def) =
  constructors def <#> \Constructor {..} ->
    (consName, foldr tArr (tData name) (typeOf <$> consFields))

getEnv :: [(Name, Definition (Expr t a0 a1 a2 a3))] -> Environment Type
getEnv ds = Env.fromList $ (typeOf <$$> ds) <> (consTypes =<< ds)

toProgram ::
     Source (Expr t a0 a1 a2 a3)
  => [(Name, Definition (Expr t a0 a1 a2 a3))]
  -> Program
toProgram ds = execCompiler (compileDefinitions ds) (getEnv ds)

instance Source Ast where
  compileFunction name = pure . Function

instance Source TypedExpr where
  compileFunction name (Signature args (ty, body)) = do
    expr <- asks (runReader (preprocess body))
    let self = (foldType ty (args <#> fst), name)
    main <- local (insertArgs (self : args)) (compileAst expr)
    pure (Function (Signature args (ty, main)))

compileDefinitions :: (Source a) => [(Name, Definition a)] -> Compiler ()
compileDefinitions defs =
  forM_ defs $ \(name, def) -> do
    modify . insertDefinition name =<<
      case def of
        Function sig -> fillParams =<< compileFunction name sig
        External sig -> pure (External sig)
        Constant lit -> pure (Constant lit)
        Data name ctrs -> pure (Data name ctrs)

compileSource :: [(Name, Definition (SourceExpr ()))] -> Compiler ()
compileSource defs
  | null ls = do
    forM_ rs $ \a -> do
      traceShowM a
      traceShowM "////////"
      traceShowM "////////"
    compileDefinitions rs
  | otherwise = error (show ls) -- TODO
  where
    (ls, rs) = partitionDefs (sequence <$$> second typecheckDef <$> defs)
    partitionDefs = partitionEithers . (uncurry (\a -> bimap (a, ) (a, )) <$>)
    typecheckDef def = runCheck (insertArgs (funArgs def) (getEnv defs)) <$> def
