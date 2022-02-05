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
import Data.Maybe (fromJust, fromMaybe, maybeToList)
import Data.Tuple.Extra (first, second, secondM)
import Pong.Lang
import Pong.TypeChecker
import TextShow (showt)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pong.Util.Env as Env

combineLambdas :: TypedExpr -> TypedExpr
combineLambdas =
  cata $ \case
    ELam _ xs (Fix (ELam _ ys expr)) -> lam (xs <> ys) expr
    e -> embed e

convertLetBindings :: TypedExpr -> TypedExpr
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
    e -> embed e

convertClosures :: (MonadReader TypeEnv m) => TypedExpr -> m PreAst
convertClosures =
  cata $ \case
    EVar v -> pure (var v)
    ELit prim -> pure (lit prim)
    EIf e1 e2 e3 -> if_ <$> e1 <*> e2 <*> e3
    EApp _ fun args -> do
      fun >>= (project >>> \case
        EApp _ g as1 -> do
          as <- sequence args
          pure (app g (as1 <> as))
        _ ->
          app <$> fun <*> sequence args)
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

--foo2 :: (MonadReader TypeEnv m) => Name -> m Type
--foo2 name = do
--  Env env <- ask
--  case env !? name of
--    Nothing ->
--      error ("-- Not found: " <> show name)
--    Just x ->
--      pure x

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

compileFunction :: Name -> Signature TypedExpr -> Compiler (Definition Ast)
compileFunction name (Signature args (ty, body)) = do
  expr <- asks (runReader (preprocess body))
  let self = (foldType ty (args <#> fst), name)
  main <- local (insertArgs (self : args)) (compileAst expr)
  pure (Function (Signature args (ty, main)))

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
        EVar (t1, name) ->
          pure (call_ (t1, name) as)
        ECall _ fun as1 -> do
          pure (call_ fun (as1 <> as))
        e -> error (show e)

clauses :: ([Label Type], Compiler Ast) -> Compiler ([Label Type], Ast)
clauses (pairs, expr) = (pairs, ) <$> local (insertArgs pairs) expr

----compileAst :: Ast -> Compiler Body
----compileAst =
----  para $ \case -- cata!
----    ELet {} -- /
----     -> error "Implementation error"
----    expr ->
----      snd <$> expr & \case
----        ELam args expr -> do
----          name <- uniqueName "def"
----          body <- local (insertArgs args) expr
----          let signature = Function (Signature args (typeOf body, body))
----          modify
----            (insertDefinition name signature)
----          pure (var (typeOf signature, name))
------        EVar v -> pure (var v)
------        ELit prim -> pure (lit prim)
------        EIf e1 e2 e3 -> if_ <$> e1 <*> e2 <*> e3
------        EOp2 op e1 e2 -> op2 op <$> e1 <*> e2
----        ECase e1 cs -> case_ <$> e1 <*> traverse clauses cs
----        e -> embed <$> sequence e
------        EApp t expr args -> do
------          as <- sequence args
------          expr >>= 
------            para (\case
------                 EApp _ expr1 args1 -> do
------                   e1 <- expr
------                   as1 <- sequence (snd <$> args1)
------                   pure (app t e1 (as1 <> as))
------                 EIf expr1 expr2 expr3 ->
------                   if_ <$> (fst <$> sequence expr1)  
------                       <*> (fst <$> sequence expr2) 
------                       <*> (fst <$> sequence expr3)
------                 ECase expr clauses ->
------                   case_ <$> (fst <$> sequence expr) 
------                         <*> traverse sequence (snd <$$> clauses)
------                 e -> pure (app t (embed (fst <$> e)) as)
------                 )
--------          expr >>=
--------            para
--------              (\case
--------                 EVar name -> pure (app t (var name) as)
--------                 EApp t1 fun ys -> undefined -- pure (bCall fun ((fst <$> ys) <> as))
--------              )
----------                 BCall fun ys -> pure (bCall fun ((fst <$> ys) <> as))
----------                 BIf expr1 expr2 expr3 ->
----------                   bIf <$> (fst <$> sequence expr1) <*> (fst <$> sequence expr2) <*>
----------                   (fst <$> sequence expr3)
----------                 BCase expr clauses ->
----------                   bCase <$> (fst <$> sequence expr) <*>
----------                   traverse sequence (snd <$$> clauses)
----------                 e -> pure (embed (fst <$> e)))
--
--clauses :: ([Label Type], Compiler Body) -> Compiler ([Label Type], Body)
----clauses (pairs, body) = (,) (pairs <#> snd) <$> local (insertArgs pairs) body
--clauses = undefined

fillParams :: Definition Ast -> Compiler (Definition Ast)
fillParams (Function (Signature arguments (ty, body)))
  | isTCon ArrT ty = do
    let applyTo xs =
          project >>> \case
            ECall _ fun args -> 
              pure (call_ fun (args <> xs))
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
            { arguments = arguments <> extra
            , body = (returnTypeOf ty, newBody)
            }))
fillParams def = pure def

--fillParams :: Definition Body -> Compiler (Definition Body)
--fillParams = undefined
---- TODO
----fillParams (Function (Signature arguments (ty, body)))
----  | isTCon ArrT ty = do
----    let tys = init (unwindType ty)
----        applyTo xs =
----          project >>> \case
----            BCall fun args -- /
----             -> pure (bCall fun (args <> xs))
----            BVar name -> do
----              names <- gets definitions
----              pure (bCall name xs)
----            expr -- /
----             -> pure (embed expr)
----    vars <- replicateM (length tys) (uniqueName "v")
----    newBody <- applyTo (bVar <$> vars) body
----    pure
----      (Function
----         (Signature
----            { arguments = arguments <> zip tys vars
----            , body = (returnTypeOf ty, newBody)
----            }))
----fillParams def = pure def

consTypes :: (Name, Definition (Expr t a0 a1 a2 a3)) -> [(Name, Type)]
consTypes (name, def) =
  constructors def <#> \Constructor {..} ->
    (consName, foldr tArr (tData name) (typeOf <$> consFields))

getEnv :: [(Name, Definition (Expr t a0 a1 a2 a3))] -> Environment Type
getEnv ds = Env.fromList $ (typeOf <$$> ds) <> (consTypes =<< ds)

toProgram :: Source (Expr t a0 a1 a2 a3) => [(Name, Definition (Expr t a0 a1 a2 a3))] -> Program
toProgram ds = execCompiler (compileDefinitions ds) (getEnv ds)

instance Source TypedExpr where
  compileDefinitions = 
    compileDefinitions <=<
      mapM (\(name, def) -> do
        (name,) <$> (case def of
          Function sig -> fillParams =<< compileFunction name sig
          --Function sig -> compileFunction name sig
          External sig -> pure (External sig)
          Constant lit -> pure (Constant lit)
          Data name css -> pure (Data name css)))

instance Source Ast where
  compileDefinitions = mapM_ (modify . uncurry insertDefinition) 

instance Source (SourceExpr ()) where
  compileDefinitions ds 
    | null ls = compileDefinitions rs
    | otherwise = error (show ls)  -- TODO
    where
      (ls, rs) = partitionDefs (sequence <$$> second typecheckDef <$> ds)
      typecheckDef def = runCheck (insertArgs (funArgs def) (getEnv ds)) <$> def
      partitionDefs = partitionEithers . (uncurry (\a -> bimap (a, ) (a, )) <$>)
