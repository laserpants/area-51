{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Pong.Eval where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char (isUpper)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Tuple.Extra (first)
import Pong.Lang
import Pong.Util
import Pong.Util.Env (Environment)
import qualified Pong.Util.Env as Env
import TextShow (showt)

{- ORMOLU_DISABLE -}

-- | A fully evaluated expression
data Value
  = PrimValue Prim                           -- ^ Primitive value
  | ConValue Name [Value]                    -- ^ Applied data constructor
  | RecValue (FieldSet Value)                -- ^ Record value
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

newtype Eval a = Eval {unEval :: ReaderT ValueEnv (WriterT Text IO) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadWriter Text
    , MonadReader ValueEnv
    )

eval :: Ast -> Eval Value
eval =
  cata $ \case
    EVar (t, var)
      | isUpper (Text.head var) ->
          pure (ConValue var [])
      | otherwise -> do
          (defs, vals) <- ask
          case Env.lookup var vals of
            Just val ->
              pure val
            Nothing ->
              case Env.lookup var defs of
                Just (Constant (_, expr)) -> do
                  eval expr
                Just Function{} ->
                  pure (Closure (t, var) [])
                _ ->
                  error "Eval error"
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
      evalPat e cs
    ENil ->
      pure (RecValue mempty)
    EExt name expr row -> do
      e <- expr
      row >>= \case
        RecValue fs ->
          pure (RecValue (Map.insertWith (<>) name [e] fs))
        _ ->
          error "Implementation error"
    ERes field expr1 expr2 -> do
      e1 <- expr1
      evalRestriction e1 field expr2
    _ ->
      error "Eval error"

evalCall :: Label MonoType -> [Eval Value] -> Eval Value
evalCall (t, fun) args
  | arity t > length args =
      pure (Closure (t, fun) args)
  | arity t < length args =
      evalCall (t, fun) (take (arity t) args) >>= \case
        Closure c as1 -> evalCall c (as1 <> drop (arity t) args)
        _ -> error "Eval error"
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
            ("print_int", [PrimValue (PInt n)]) -> do
              --              liftIO (print n)
              tell (showt n)
              pure (PrimValue (PInt 0))
            _ ->
              error "Not implemented"
        Nothing ->
          case Env.lookup fun vals of
            Just (Closure g vs) ->
              evalCall g (vs <> args)
            _ ->
              error "Eval error"
        _ ->
          error "Eval error"

evalPat :: Value -> [Clause MonoType (Eval Value)] -> Eval Value
evalPat (ConValue name fields) (((_, con) : vars, value) : clauses)
  | conName == con =
      localSecond (Env.inserts (zip (snd <$> vars) fields)) value
  | otherwise =
      evalPat (ConValue name fields) clauses
  where
    conName =
      case Text.splitOn "-" name of
        [c, _] -> c
        _ -> name
evalPat c _ = error ("No constructor: " <> show c)

evalRestriction :: Value -> [Label MonoType] -> Eval Value -> Eval Value
evalRestriction (RecValue fs) [(_, field), (_, v), (_, r)] =
  localSecond
    ( Env.inserts
        [ (v, e)
        ,
          ( r
          , RecValue
              ( case es of
                  [] -> Map.delete field fs
                  _ -> Map.insert field es fs
              )
          )
        ]
    )
  where
    Just (e : es) = Map.lookup field fs
evalRestriction _ _ = error "Eval error"

getPrim :: Value -> Prim
getPrim (PrimValue prim) = prim
getPrim _ = error "Ill-formed expression"

{- ORMOLU_DISABLE -}

evalOp1 :: Op1 -> Prim -> Prim
evalOp1 ONot (PBool b)   = PBool (not b)
evalOp1 ONeg (PFloat p)  = PFloat (negate p)
evalOp1 ONeg (PDouble p) = PDouble (negate p)
evalOp1 ONeg (PInt n)    = PInt (negate n)
evalOp1 _ _              = error "Not implemented"

evalOp2 :: Op2 -> Prim -> Prim -> Prim
evalOp2 OAdd (PFloat p) (PFloat q) = PFloat (p + q)
evalOp2 OMul (PFloat p) (PFloat q) = PFloat (p * q)
evalOp2 OSub (PFloat p) (PFloat q) = PFloat (p - q)
evalOp2 ODiv (PFloat p) (PFloat q) = PFloat (p / q)
evalOp2 OLt  (PFloat p) (PFloat q) = PBool (p < q)
evalOp2 OGt  (PFloat p) (PFloat q) = PBool (p > q)
evalOp2 OLtE (PFloat p) (PFloat q) = PBool (p <= q)
evalOp2 OGtE (PFloat p) (PFloat q) = PBool (p >= q)
evalOp2 OAdd (PDouble p) (PDouble q) = PDouble (p + q)
evalOp2 OMul (PDouble p) (PDouble q) = PDouble (p * q)
evalOp2 OSub (PDouble p) (PDouble q) = PDouble (p - q)
evalOp2 ODiv (PDouble p) (PDouble q) = PDouble (p / q)
evalOp2 OLt  (PDouble p) (PDouble q) = PBool (p < q)
evalOp2 OGt  (PDouble p) (PDouble q) = PBool (p > q)
evalOp2 OLtE (PDouble p) (PDouble q) = PBool (p <= q)
evalOp2 OGtE (PDouble p) (PDouble q) = PBool (p >= q)
evalOp2 OEq  (PInt m) (PInt n) = PBool (m == n)
evalOp2 ONEq (PInt m) (PInt n) = PBool (m /= n)
evalOp2 OEq  (PBool m) (PBool n) = PBool (m == n)
evalOp2 ONEq (PBool m) (PBool n) = PBool (m /= n)
evalOp2 OEq  (PFloat m) (PFloat n) = PBool (m == n)
evalOp2 ONEq (PFloat m) (PFloat n) = PBool (m /= n)
evalOp2 OEq  (PDouble m) (PDouble n) = PBool (m == n)
evalOp2 ONEq (PDouble m) (PDouble n) = PBool (m /= n)
evalOp2 OAdd (PInt m) (PInt n) = PInt (m + n)
evalOp2 OSub (PInt m) (PInt n) = PInt (m - n)
evalOp2 OMul (PInt m) (PInt n) = PInt (m * n)
evalOp2 OLt  (PInt p) (PInt q) = PBool (p < q)
evalOp2 OGt  (PInt p) (PInt q) = PBool (p > q)
evalOp2 OLtE (PInt p) (PInt q) = PBool (p <= q)
evalOp2 OGtE (PInt p) (PInt q) = PBool (p >= q)
evalOp2 _ _ _ = error "Not implemented"

{- ORMOLU_ENABLE -}

runEval :: Environment (Definition MonoType Ast) -> Eval a -> IO (a, Text)
runEval env ast = runWriterT (runReaderT (unEval ast) (env, mempty))

evalModule :: Module MonoType Ast -> Label Scheme -> IO (Maybe (Value, Text))
evalModule (Module _ p) def =
  case Map.lookup def p of
    Just (Function _ (_, ast)) -> evaluate ast
    Just (Constant (_, ast)) -> evaluate ast
    _ -> pure Nothing
  where
    evaluate ast = Just <$> runEval env (eval ast)
    env = Env.fromList (first snd <$> Map.toList p)
