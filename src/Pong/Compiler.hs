{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Tuple.Extra (first, second)
import Debug.Trace
import Pong.Lang
import Pong.TypeChecker
import qualified Pong.Util.Env as Env
import TextShow (showt)

convertLetBindings :: Ast -> Ast
convertLetBindings =
  cata $ \case
    ELet (_, name) e1 e2
      | isCon VarE e1 || isCon LitE e1 || isCon LamE e1 ->
        let alg =
              \case
                EVar (_, var)
                  | name == var -> e1
                e -> embed e
         in cata alg e2
    ELet bind e1 e2 -- /
     -> app (typeOf e2) (lam [bind] e2) [e1]
    e -> embed e

combineLambdas :: Ast -> Ast
combineLambdas =
  cata $ \case
    ELam xs (Fix (ELam ys expr)) -- /
     -> lam (xs <> ys) expr
    e -> embed e

convertClosures :: (MonadReader TypeEnv m) => Ast -> m Ast
convertClosures =
  cata $ \case
    EVar (ty, name) -> pure (var (ty, name))
    ELit prim -> pure (lit prim)
    EIf e1 e2 e3 -> if_ <$> e1 <*> e2 <*> e3
    ELet {} ->
      error
        "Implementation error: Let bindings should be removed prior to this."
    EApp ty fun args -> app ty <$> fun <*> sequence args
    EOp2 op e1 e2 -> op2 op <$> e1 <*> e2
    ECase e1 cs ->
      let insertNames (n:vs, expr) = do
            e <- local (insertArgs vs) expr
            pure (n : vs, e)
       in case_ <$> e1 <*> traverse insertNames cs
    ELam args expr -> do
      body <- local (insertArgs args) expr
      let names = free body `without` (args <#> snd)
      extra <- (`zip` names) <$> traverse lookupNameUnsafe names
      let lambda = lam (extra <> args) body
      pure $
        case extra of
          [] -> lambda
          _ -> do
            app (foldType (typeOf body) (args <#> fst)) lambda (var <$> extra)

lookupNameUnsafe :: (MonadReader TypeEnv m) => Name -> m Type
lookupNameUnsafe = asks . (fromJust <$$> Env.lookup)

preprocess :: (MonadReader TypeEnv m) => Ast -> m Ast
preprocess = combineLambdas >>> convertLetBindings >>> convertClosures

typeCheck :: Expr () -> Compiler (Either TypeError Ast)
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

mapDefinitionsM ::
     (Definition Body -> Compiler (Definition Body)) -> Compiler ()
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

compileFunction :: Name -> Signature Ast -> Compiler (Definition Body)
compileFunction name (Signature args (ty, body)) = do
  expr <- asks (runReader (preprocess body))
  let self = (foldType ty (args <#> fst), name)
  main <- local (insertArgs (self : args)) (compileAst expr)
  pure (Function (Signature args (ty, main)))

compileAst :: Ast -> Compiler Body
compileAst =
  para $ \case
    ELet {} -- /
     -> error "Implementation error"
    ELam args expr -> do
      anon <- uniqueName "def"
      body <- local (insertArgs args) (snd expr)
      modify
        (insertDefinition
           anon
           (Function (Signature args (typeOf (fst expr), body))))
      pure (bVar anon)
    expr ->
      snd <$> expr & \case
        EVar (_, name) -> pure (bVar name)
        ELit lit -> pure (bLit lit)
        EIf e1 e2 e3 -> bIf <$> e1 <*> e2 <*> e3
        EOp2 op e1 e2 -> bOp2 op <$> e1 <*> e2
        ECase e1 cs -> bCase <$> e1 <*> traverse clauses cs
        EApp _ expr args -> do
          as <- sequence args
          expr >>=
            para
              (\case
                 BVar name -> pure (bCall name as)
                 BCall fun ys -> pure (bCall fun ((fst <$> ys) <> as))
                 BIf expr1 expr2 expr3 ->
                   bIf <$> (fst <$> sequence expr1) <*> (fst <$> sequence expr2) <*>
                   (fst <$> sequence expr3)
                 BCase expr clauses ->
                   bCase <$> (fst <$> sequence expr) <*>
                   traverse sequence (snd <$$> clauses)
                 e -> pure (embed (fst <$> e)))

clauses :: ([TyId Type], Compiler Body) -> Compiler (Names, Body)
clauses (pairs, body) = (,) (pairs <#> snd) <$> local (insertArgs pairs) body

fillParams :: Definition Body -> Compiler (Definition Body)
fillParams (Function (Signature arguments (ty, body)))
  | isTCon ArrT ty = do
    let tys = init (unwindType ty)
        applyTo xs =
          project >>> \case
            BCall fun args -- /
             -> pure (bCall fun (args <> xs))
            BVar name -> do
              names <- gets definitions
              pure (bCall name xs)
            expr -- /
             -> pure (embed expr)
    vars <- replicateM (length tys) (uniqueName "v")
    newBody <- applyTo (bVar <$> vars) body
    pure
      (Function
         (Signature
            { arguments = arguments <> zip tys vars
            , body = (returnTypeOf ty, newBody)
            }))
fillParams def = pure def

consTypes :: (Name, Definition (Expr t)) -> [(Name, Type)]
consTypes (name, def) =
  constructors def <#> \Constructor {..} ->
    (consName, foldr tArr (tData name) (typeOf <$> consFields))

compileDefinitions :: [(Name, Definition Ast)] -> Compiler ()
compileDefinitions ds =
  forM_ ds $ \(name, def) -> do
    newDef <-
      case def of
        Function sig -> fillParams =<< compileFunction name sig
        External sig -> pure (External sig)
        Constant lit -> pure (Constant lit)
        Data name css -> pure (Data name css)
    modify (insertDefinition name newDef)

getEnv :: [(Name, Definition (Expr t))] -> Environment Type
getEnv ds = Env.fromList $ (typeOf <$$> ds) <> (consTypes =<< ds)

instance Source Ast where
  toProgram ds = execCompiler (compileDefinitions ds) (getEnv ds)

instance Source (Expr ()) where
  toProgram ds
    | null ls = execCompiler (compileDefinitions rs) env
    | otherwise = error (show ls)
    where
      env = getEnv ds
      (ls, rs) = partitionDefs (sequence <$$> second typecheckDef <$> ds)
      typecheckDef def = runCheck (insertArgs (funArgs def) env) <$> def
      partitionDefs = partitionEithers . (uncurry (\a -> bimap (a, ) (a, )) <$>)
