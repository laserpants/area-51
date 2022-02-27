{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Pong.Compiler where

import Control.Monad.State
import Control.Monad.Writer
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty, fromList, toList)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Debug.Trace
import Pong.Data
import Pong.Lang
import Pong.Util
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
       in embed3 EApp t1 expr (xs <> ys)
    e -> embed e

-- from:
--   foo = lam(a) => lam(b) => b
--   baz(x) = lam(a) => lam(b) => b
--
-- to: 
--   foo(a, b) = b
--   baz(x, a, b) = b
hoistTopLambdas ::
     Definition (Label t) (Expr t a1 () a2)
  -> Definition (Label t) (Expr t a1 () a2)
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
replaceVarLets expr = (substMany subs e, subs)
  where
    (e, subs) =
      expr &
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
substMany subs a = foldr (uncurry varSubst) a subs

liftLambdas ::
     Expr Type Type () a2
  -> ( Expr Type Type () a2
     , [(Name, Definition (Label Type) (Expr Type Type () a2))])
liftLambdas input = (e2, fmap (substMany subs <$$>) defs)
  where
    (e1, defs) = runWriter (evalStateT (fun input) 0)
    (e2, subs) = replaceVarLets e1
    fun =
      cata $ \case
        ELam _ args expr -> do
          name <- uniqueName
          body <- expr
          let t = typeOf body
              signature = Function (fromList args) (t, body)
          tell [(name, fillParams signature)]
          pure (eVar (foldType t (args <#> fst), name))
        expr -> embed <$> sequence expr
      where
        uniqueName = do
          n <- get
          put (succ n :: Int)
          pure (".f" <> showt n)

xyz1234 :: 
  ( Expr Type Type () a2, [(Name, Definition (Label Type) (Expr Type Type () a2))] )
  -> ( Expr Type Type () a2, [(Name, Definition (Label Type) (Expr Type Type () a2))] )
xyz1234 =
  undefined

---- g(x) = x
----
---- foo =
----   let 
----     f' =
----       f(2)            
----     in
----       g(f')(g(5)) + f'(1)
----
---- foo =
----   f(2)(g(5)) + f(2, 1)  
--elimPartials :: (Monad m) => PreAst -> m PreAst
--elimPartials =
--  cata $ \case
--    EApp _ fun args -> undefined
--    expr -> embed <$> sequence expr

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
       ERow row ->
         eRow $
         row &
         cata
           (\case
              RNil -> rNil
              RVar v -> rVar v
              RExt name expr r -> rExt name (convertFunApps expr) r)
       EApp _ expr args -> do
         case project expr of
           EVar var -> eCall var args
           ECall _ fun as1 -> do
             eCall fun (as1 <> args)
           e -> error "Implementation error")
