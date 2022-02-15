{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Pong.Compiler where

import Control.Monad.Reader
import Control.Monad.State
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty, fromList, toList)
import Data.Void (Void)
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
combineApps :: Expr t a0 () a2 -> Expr t a0 () a2
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
  \case
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
            (returnType t, combineApps (eApp expr (extra <#> eVar)))

convertClosures :: Expr Type () () a2 -> Expr Type () () a2
convertClosures =
  cata $ \case
    ELam _ args expr -> do
      let extra = free expr `without` args
          lambda = eLam (extra <> args) expr
      case extra of
        [] -> lambda
        _ -> eApp lambda (eVar <$> extra)
    expr -> embed expr

newtype Program a =
  Program
    { definitions :: Map Name (Definition (Label Type) a) 
    }

liftLambdas :: Expr Type () () Void -> ReaderT (Environment Type) (State (Program PreAst)) PreAst
liftLambdas = 
  cata $ \case
    ELam _ args expr -> do
      --name <- uniqueName "def"
      body <- local (insertArgs args) expr
      let signature = Function undefined undefined -- (Signature args (typeOf body, body))
      undefined
      --modify (insertDefinition name signature)
      --pure (var (typeOf signature, name))

-- g(x) = x
--
-- foo =
--   let 
--     f' =
--       f(2)            
--     in
--       g(f')(g(5)) + f'(1)
--
-- foo =
--   f(2)(g(5)) + f(2, 1)  
elimPartials :: (MonadState (Program PreAst) m) => PreAst -> m PreAst
elimPartials = 
  cata $ \case 
    EApp _ fun args ->
      undefined
    expr -> embed <$> sequence expr

convertFunApps :: PreAst -> Ast
convertFunApps = 
  combineApps >>> 
    cata (\case
      EVar a -> eVar a
      ECon a -> eCon a
      ELit a -> eLit a
      EIf a1 a2 a3 -> eIf a1 a2 a3
      ELet a1 a2 a3 -> eLet a1 a2 a3
      EOp2 op a1 a2 -> eOp2 op a1 a2
      ECase a1 a2 -> eCase a1 a2
      ERow row -> eRow $ row & cata (\case
        RNil -> rNil
        RVar v -> rVar v
        RExt name expr r -> rExt name (convertFunApps expr) r)
      EApp _ expr args -> do
        case project expr of 
          EVar var -> 
            eCall var args
          ECall _ fun as1 -> do
            eCall fun (as1 <> args)
          e ->
            error "Implementation error")
