{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Pong.Eval where

import Control.Monad.Reader
import Data.Char (isUpper)
import Data.List.NonEmpty (fromList, toList)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Tuple.Extra (first)
import Debug.Trace
import Pong.Data
import Pong.Lang
import Pong.Util
import Pong.Util.Env (Environment)
import qualified Pong.Util.Env as Env

{- ORMOLU_DISABLE -}

-- | A fully evaluated expression
data Value
  = PrimValue Prim                           -- ^ Primitive value
  | ConValue Name [Value]                    -- ^ Applied data constructor
  | RecValue (Row Value Void)                -- ^ Record value
  | Closure (Label MonoType) [Eval Value]    -- ^ Partially applied function

{- ORMOLU_ENABLE -}

instance Eq Value where
  a == b =
    case (a, b) of
      (PrimValue p, PrimValue q) ->
        p == q
      (ConValue c vs, ConValue d ws) ->
        c == d && vs == ws
      (RecValue r, RecValue s) ->
        r == s
      _ ->
        error "Implementation error"

instance Show Value where
  showsPrec d = \case
    PrimValue prim ->
      showParen
        (d > 10)
        (showString "PrimValue " . showsPrec 11 prim)
    ConValue name vals ->
      showParen
        (d > 10)
        (showString ("ConValue " <> show name <> " ") . showsPrec 11 vals)
    RecValue row ->
      showParen
        (d > 10)
        (showString "RecValue " . showsPrec 11 row)
    Closure{} ->
      showString "<<function>>"

type ValueEnv =
  ( Environment (Definition MonoType Ast)
  , Environment Value
  )

newtype Eval a = Eval {unEval :: Reader ValueEnv a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader ValueEnv
    )

eval :: Ast -> Eval Value
eval =
  cata $ \case
    EVar (t, var) -> do
      (defs, vals) <- ask
      case Env.lookup var vals of
        Just val -> pure val
        Nothing ->
          case Env.lookup var defs of
            Just (Constant (_, expr)) -> do
              eval expr
            Just (Function args (_, expr)) ->
              pure (Closure (t, var) [])
            Nothing ->
              error ("Variable not in scope: " <> show var)
    ELit prim ->
      pure (PrimValue prim)
    EIf cond true false ->
      cond >>= \case
        PrimValue (PBool True) ->
          true
        PrimValue (PBool False) ->
          false
        _ ->
          error "Ill-formed expression"
    ELet (_, var) body expr -> do
      val <- body
      localSecond (Env.insert var val) expr
    ECall _ fun args ->
      evalCall fun args
    EOp1 (_, op) a ->
      PrimValue <$> (evalOp1 op . getPrim <$> a)
    EOp2 (_, op) a b ->
      case op of
        OLogicOr ->
          a >>= \case
            PrimValue (PBool True) -> a
            _ -> b
        OLogicAnd ->
          a >>= \case
            PrimValue (PBool False) -> a
            _ -> b
        _ ->
          PrimValue <$> (evalOp2 op <$> (getPrim <$> a) <*> (getPrim <$> b))
    EPat expr cs -> do
      e <- expr
      evalMatch e cs
    ERec row ->
      RecValue <$> evalRow row
    ERes field expr1 expr2 -> do
      e1 <- expr1
      evalRestriction (getRow e1) field expr2

evalCall :: Label MonoType -> [Eval Value] -> Eval Value
evalCall (t, fun) args
  | arity t > length args =
      pure (Closure (t, fun) args)
  | arity t < length args =
      evalCall (t, fun) (take (arity t) args) >>= \case
        Closure c as1 -> evalCall c (as1 <> drop (arity t) args)
  | isUpper (Text.head fun) =
      ConValue fun <$> sequence args
  | otherwise = do
      (defs, vals) <- ask
      as <- sequence args
      case Env.lookup fun defs of
        Just (Function vs (_, body)) ->
          localSecond (Env.inserts (zip (snd <$> toList vs) as)) (eval body)
        Just Extern{} ->
          case (fun, as) of
            ("print_int", [PrimValue (PInt n)]) ->
              pure (PrimValue (PInt 0))
            _ ->
              error "Not implemented"
        Nothing ->
          case Env.lookup fun vals of
            Just (Closure g vs) ->
              evalCall g (vs <> args)
            _ ->
              error "Eval error"

evalMatch :: Value -> [Clause MonoType (Eval Value)] -> Eval Value
evalMatch _ [] = error "Eval error: no match"
evalMatch (ConValue name fields) (((_, con) : vars, value) : clauses)
  | name == con = localSecond (Env.inserts (zip (snd <$> vars) fields)) value
  | otherwise = evalMatch (ConValue name fields) clauses

evalRow :: Row Ast (Label MonoType) -> Eval (Row Value Void)
evalRow =
  cata $ \case
    RNil -> pure rNil
    RVar (_, var) -> do
      (_, env) <- ask
      case Env.lookup var env of
        Just (RecValue val) -> pure val
        _ -> error "Eval error"
    RExt name v row ->
      rExt name <$> eval v <*> row

evalRestriction :: Row Value Void -> [Label MonoType] -> Eval Value -> Eval Value
evalRestriction row [(_, name), (_, v), (_, r)] =
  let (p, q) = restrictRow name row
   in localSecond (Env.inserts [(v, p), (r, RecValue q)])

getPrim :: Value -> Prim
getPrim (PrimValue prim) = prim
getPrim _ = error "Ill-formed expression"

getRow :: Value -> Row Value Void
getRow (RecValue row) = row
getRow _ = error "Ill-formed expression"

evalOp1 :: Op1 -> Prim -> Prim
evalOp1 ONot (PBool b) = PBool (not b)
evalOp1 ONeg (PFloat p) = PFloat (negate p)
evalOp1 ONeg (PDouble p) = PDouble (negate p)
evalOp1 ONeg (PInt n) = PInt (negate n)

evalOp2 :: Op2 -> Prim -> Prim -> Prim
evalOp2 OAdd (PFloat p) (PFloat q) = PFloat (p + q)
evalOp2 OMul (PFloat p) (PFloat q) = PFloat (p * q)
evalOp2 OSub (PFloat p) (PFloat q) = PFloat (p - q)
evalOp2 ODiv (PFloat p) (PFloat q) = PFloat (p / q)
evalOp2 OLt (PFloat p) (PFloat q) = PBool (p < q)
evalOp2 OGt (PFloat p) (PFloat q) = PBool (p > q)
evalOp2 OLtE (PFloat p) (PFloat q) = PBool (p <= q)
evalOp2 OGtE (PFloat p) (PFloat q) = PBool (p >= q)
evalOp2 OAdd (PDouble p) (PDouble q) = PDouble (p + q)
evalOp2 OMul (PDouble p) (PDouble q) = PDouble (p * q)
evalOp2 OSub (PDouble p) (PDouble q) = PDouble (p - q)
evalOp2 ODiv (PDouble p) (PDouble q) = PDouble (p / q)
evalOp2 OLt (PDouble p) (PDouble q) = PBool (p < q)
evalOp2 OGt (PDouble p) (PDouble q) = PBool (p > q)
evalOp2 OLtE (PDouble p) (PDouble q) = PBool (p <= q)
evalOp2 OGtE (PDouble p) (PDouble q) = PBool (p >= q)
evalOp2 OEq (PInt m) (PInt n) = PBool (m == n)
evalOp2 ONEq (PInt m) (PInt n) = PBool (m /= n)
evalOp2 OAdd (PInt m) (PInt n) = PInt (m + n)
evalOp2 OSub (PInt m) (PInt n) = PInt (m - n)
evalOp2 OMul (PInt m) (PInt n) = PInt (m * n)
evalOp2 OLt (PInt p) (PInt q) = PBool (p < q)
evalOp2 OGt (PInt p) (PInt q) = PBool (p > q)
evalOp2 OLtE (PInt p) (PInt q) = PBool (p <= q)
evalOp2 OGtE (PInt p) (PInt q) = PBool (p >= q)
evalOp2 _ _ _ = error "Not implemented"
