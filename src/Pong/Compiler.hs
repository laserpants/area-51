{-# LANGUAGE LambdaCase #-}
module Pong.Compiler where

import Data.List.NonEmpty (NonEmpty, fromList, toList)
import Pong.Data
import Pong.Lang
import Pong.Util
import TextShow (showt)

-- from:
--   lam(a) => lam(b) => b
--
-- to: 
--   lamb[a, b] => b
combineLambdas :: Expr t () a1 a2 -> Expr t () a1 a2
combineLambdas =
  cata $ \case
    ELam _ xs (Fix (ELam _ ys expr)) -> eLam (xs <> ys) expr
    e -> embed e

-- from:
--   (g(x))(y)
--
-- to: 
--   g(x, y)
combineApps :: Expr t () () a2 -> Expr t () () a2
combineApps =
  cata $ \case
    EApp _ (Fix (EApp _ expr xs)) ys -> eApp expr (xs <> ys)
    e -> embed e

-- from:
--   foo = lam(a) => lam(b) => b
--   baz(x) = lam(a) => lam(b) => b
--
-- to: 
--   foo(a, b) = b
--   baz(x, a, b) = b
hoistTopLambdas ::
     Definition (Label t) (Expr t () a1 a2)
  -> Definition (Label t) (Expr t () a1 a2)
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
fillParams ::
     Definition (Label Type) (Expr Type () () a2)
  -> Definition (Label Type) (Expr Type () () a2)
fillParams =
  fmap combineApps <<< \case
    Function args (t, expr)
      | isTCon ArrT t -> transform t (toList args) expr
    Constant (t, expr)
      | isTCon ArrT t -> transform t [] expr
    def -> def
  where
    transform t as expr =
      let extra = argTypes t `zip` [".v" <> showt n | n <- [0 ..] :: [Int]]
       in Function
            (fromList (as <> extra))
            (returnType t, eApp expr (extra <#> eVar))
