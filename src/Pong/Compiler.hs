{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pong.Compiler where

import Control.Monad.State
import Control.Monad.Writer
import qualified Control.Newtype.Generics as N
import Data.Function ((&))
import Data.List (partition)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty, (!!), fromList, toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (first)
import Data.Void (Void)
import Debug.Trace
import Pong.Data
import Pong.Lang
import Pong.TypeChecker (Substitution, apply, unify)
import Pong.Util
import Prelude hiding ((!!))
import TextShow (showt)

-- from:
--   lam(a) => lam(b) => b
--
-- to: 
--   lamb[a, b] => b
combineLambdas :: Expr t a0 () a2 -> Expr t a0 () a2
combineLambdas =
  cata $ \case
    ELam _ xs (Fix (ELam _ ys expr)) -> eLam (xs <> ys) expr
    e -> embed e

-- from:
--   (g(x))(y)
--
-- to: 
--   g(x, y)
combineApps :: Expr t Type a1 a2 -> Expr t Type a1 a2
combineApps =
  cata $ \case
    EApp _ (Fix (EApp t expr xs)) ys ->
      let t1 = foldType1 (drop (length ys) (unwindType t))
       in eApp t1 expr (xs <> ys)
    e -> embed e

-- from:
--   foo = lam(a) => lam(b) => b
--   baz(x) = lam(a) => lam(b) => b
--
-- to: 
--   foo(a, b) = b
--   baz(x, a, b) = b
hoistTopLambdas ::
     Definition (Label t) (Expr t a0 () a2)
  -> Definition (Label t) (Expr t a0 () a2)
hoistTopLambdas =
  \case
    Function args (t, expr)
      | isCon LamE expr -> combine t (toList args) expr
    Constant (t, expr)
      | isCon LamE expr -> combine t [] expr
    def -> def
  where
    combine t as =
      project . combineLambdas >>> \case
        ELam _ bs expr -> Function (fromList (as <> bs)) (returnType t, expr)
        _ -> error "Implementation error"

-- from:
--   plus(x, y) = x + y
--   foo(x) = plus(x)      
--
-- to: 
--   plus(x, y) = x + y
--   foo(x, v_0) = plus(x, v_0)
--
fillParams ::
     Definition (Label Type) (Expr Type Type () a2)
  -> Definition (Label Type) (Expr Type Type () a2)
fillParams = hoistTopLambdas <<< fmap fillExprParams

--
-- from:
--   let
--     g =
--      f(1)
--   in
--     g(2)
--
-- to:
--   let
--     g =
--       (\v_0 -> f(1, v_0))
--   in
--     g(2)
--
fillExprParams :: Expr Type Type () a2 -> Expr Type Type () a2
fillExprParams =
  replaceVarLets >>>
  fst >>>
  para
    (\case
       EVar (t, fun)
         | arity t > 0 ->
           let extra = argTypes t `zip` names
            in eLam extra (eApp (returnType t) (eVar (t, fun)) (extra <#> eVar))
       EApp t fun args
         | arity (fst fun) > n ->
           let extra = drop n (argTypes (fst fun)) `zip` names
               t1 = foldType1 (drop (length extra) (unwindType t))
            in eLam
                 extra
                 (eApp t1 (varCon fun) ((args <#> varCon) <> (extra <#> eVar)))
         | otherwise -> eApp t (varCon fun) (args <#> varCon)
         where n = length args
               varCon ::
                    (Expr Type Type () a2, Expr Type Type () a2)
                 -> Expr Type Type () a2
               varCon (e1, e2) =
                 case project e1 of
                   EVar v -> eVar v
                   _ -> e2
       e -> embed (e <#> snd))
  where
    names = [".v" <> showt n | n <- [0 :: Int ..]]

--
-- from:
--   lam(x) => x + h
--
-- to:
--   (lam[h, x] => x + h)(h)
--
convertClosures :: Expr Type () () a2 -> Expr Type () () a2
convertClosures =
  cata $ \case
    ELam _ args expr -> do
      let extra = freeVars expr `without` args
          lambda = eLam (extra <> args) expr
      case extra of
        [] -> lambda
        _ -> eApp () lambda (eVar <$> extra)
    expr -> embed expr

--
-- from:
--   let x = y in x + z
--
-- to:
--   y + z
--
replaceVarLets :: Expr Type a0 a1 a2 -> (Expr Type a0 a1 a2, [(Name, Name)])
replaceVarLets input = (substMany subs e, subs)
  where
    (e, subs) =
      input &
      (runWriter <<<
       cata
         (\case
            ELet (t, name) expr body ->
              expr >>=
              (project >>> \case
                 EVar (_, var) -> do
                   tell [(name, var)]
                   body
                 _ -> eLet (t, name) <$> expr <*> body)
            e -> embed <$> sequence e))

varSubst :: Name -> Name -> Expr Type a0 a1 a2 -> Expr Type a0 a1 a2
varSubst from to =
  cata $ \case
    EVar (t, v)
      | v == from -> eVar (t, to)
    e -> embed e

substMany :: [(Name, Name)] -> Expr Type a0 a1 a2 -> Expr Type a0 a1 a2
substMany subs e = foldr (uncurry varSubst) e subs

liftLambdas ::
     (MonadState (Program PreAst) m) => Expr Type Type () Void -> m PreAst
liftLambdas input = do
  (expr, subs) <- replaceVarLets <$> fun input
  modifyProgram (substMany subs <$$>)
  pure expr
  where
    fun =
      cata $ \case
        ELam _ args expr -> do
          name <- uniqueName ".f"
          body <- expr
          let t = typeOf body
              signature = Function (fromList args) (t, body)
          insertDef name signature
          pure (eVar (foldType t (args <#> fst), name))
        EVar v -> pure (eVar v)
        ECon c -> pure (eCon c)
        ELit l -> pure (eLit l)
        EIf a1 a2 a3 -> eIf <$> a1 <*> a2 <*> a3
        ELet a1 a2 a3 -> eLet a1 <$> a2 <*> a3
        EOp2 op a1 a2 -> eOp2 op <$> a1 <*> a2
        ECase a1 a2 -> eCase <$> a1 <*> traverse sequence a2
        EApp t expr args -> eApp t <$> expr <*> sequence args
        ERow row ->
          eRow <$>
          (`cata` row)
            (\case
               RNil -> pure rNil
               RVar v -> pure (rVar v)
               RExt name expr r -> rExt name <$> liftLambdas expr <*> r)

uniqueName :: (MonadState (Program a) m) => Name -> m Name
uniqueName prefix = do
  Program defs <- get
  pure (prefix <> showt (Map.size defs))

--liftLambdas input = (e2, fmap (substMany subs <$$>) defs)
--  where
--    (e1, defs) = runWriter (evalStateT (fun input) 0)
--    (e2, subs) = replaceVarLets e1
--fun :: Expr Type Type () a2 -> Program (Expr Type Type () a2)
--fun =
--      cata $ \case
--        ELam _ args expr -> do
--          name <- uniqueName
--          body <- expr
--          let t = typeOf body
--              signature = Function (fromList args) (t, body)
--          insertDef name (fillParams signature)
--          --tell [(name, fillParams signature)]
--          pure (eVar (foldType t (args <#> fst), name))
--        expr -> embed <$> sequence expr
--      where
--        uniqueName = do
--          undefined
--          --n <- get
--          --put (succ n :: Int)
--          --pure (".f" <> showt n)
--liftLambdas ::
--     Expr Type Type () a2
--  -> ( Expr Type Type () a2
--     , [(Name, Definition (Label Type) (Expr Type Type () a2))])
--liftLambdas input = (e2, fmap (substMany subs <$$>) defs)
--  where
--    (e1, defs) = runWriter (evalStateT (fun input) 0)
--    (e2, subs) = replaceVarLets e1
--    fun =
--      cata $ \case
--        ELam _ args expr -> do
--          name <- uniqueName
--          body <- expr
--          let t = typeOf body
--              signature = Function (fromList args) (t, body)
--          tell [(name, fillParams signature)]
--          pure (eVar (foldType t (args <#> fst), name))
--        expr -> embed <$> sequence expr
--      where
--        uniqueName = do
--          n <- get
--          put (succ n :: Int)
--          pure (".f" <> showt n)
alignCallSigns :: (MonadState (Program PreAst) m) => PreAst -> m PreAst
alignCallSigns =
  cata $ \case
    EApp t fun args -> do
      f <- fun
      as <- sequence args
      case project f of
        EVar (t1, var) -> do
          def <- lookupDef var
          case unify (typeOf def) t1 of
            Right sub
              | sub /= mempty -> do
                name <- uniqueName ".g"
                insertDef name =<< preprocess (apply sub def)
                pure (eApp t (eVar (t1, name)) as)
            _ -> pure (eApp t f as)
        _ -> pure (eApp t f as)
    e -> embed <$> sequence e

preprocess ::
     (MonadState (Program PreAst) m)
  => Definition (Label Type) (Expr Type Type a1 a2)
  -> m (Definition (Label Type) PreAst)
preprocess def = do
  traverse liftLambdas (fillParams (convert <$> def))
  where
    convert =
      cata $ \case
        EVar v -> eVar v
        ECon c -> eCon c
        ELit l -> eLit l
        EIf a1 a2 a3 -> eIf a1 a2 a3
        ELet a1 a2 a3 -> eLet a1 a2 a3
        EApp a1 a2 a3 -> eApp a1 a2 a3
        EOp2 op a1 a2 -> eOp2 op a1 a2
        ECase a1 a2 -> eCase a1 a2
        ERow row ->
          eRow $
          (`cata` row)
            (\case
               RNil -> rNil
               RVar v -> rVar v
               RExt name expr r -> rExt name (convert expr) r)

replaceFunArgs :: (MonadState (Program PreAst) m) => PreAst -> m PreAst
replaceFunArgs =
  cata $ \case
    EApp t fun args -> do
      f <- fun
      as <- sequence args
      let (as1, as2) = partition (isTCon ArrT . typeOf . snd) (zip [0 ..] as)
      case project f of
        EVar (t1, var)
          | not (null as1) -> do
            def <- lookupDef var
            let Function ps (t2, e) = def
            name <- uniqueName ".h"
            let getVar i =
                  let EVar v = project (as List.!! i)
                   in snd v
                subs = [(snd (ps !! i), getVar i) | i <- fst <$> as1]
                def1 =
                  Function
                    (fromList [ps !! i | i <- fst <$> as2])
                    (t2, substMany subs e)
            insertDef name =<< preprocess def1
            pure (eApp t (eVar (typeOf def1, name)) (snd <$> as2))
        _ -> pure (eApp t f as)
    e -> embed <$> sequence e

convertFunApps :: PreAst -> Ast
convertFunApps =
  combineApps >>>
  cata
    (\case
       EVar a -> eVar a
       ECon a -> eCon a
       ELit a -> eLit a
       EIf a1 a2 a3 -> eIf a1 a2 a3
       ELet a1 a2 a3 -> eLet a1 a2 a3
       EOp2 op a1 a2 -> eOp2 op a1 a2
       ECase a1 a2 -> eCase a1 a2
       EApp _ expr args -> do
         case project expr of
           EVar var -> eCall var args
           ECall _ fun as1 -> do
             eCall fun (as1 <> args)
           e -> error "Implementation error"
       ERow row ->
         eRow $
         (`cata` row)
           (\case
              RNil -> rNil
              RVar v -> rVar v
              RExt name expr r -> rExt name (convertFunApps expr) r))
