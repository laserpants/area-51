{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Pong.Eval where

import Control.Monad.Reader
import Data.Char (isUpper)
import Data.List.NonEmpty (fromList, toList)
import Data.Tuple.Extra (first)
import Debug.Trace
import Pong.Data
import Pong.Lang
import Pong.Util
import Pong.Util.Env (Environment)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pong.Util.Env as Env

data Value
  = PrimValue Prim                           -- ^ Primitive value
  | ConValue Name [Value]                    -- ^ Applied data constructor
  | RowValue (Row Value Void)                -- ^ Row value
  | Closure (Label MonoType) [Eval Value]    -- ^ Partially applied function

instance Eq Value where
  a == b =
    case (a, b) of
      (PrimValue p, PrimValue q) -> 
        p == q
      (ConValue c vs, ConValue d ws) -> 
        c == d && vs == ws
      (RowValue r, RowValue s) -> 
        r == s
      _ -> 
        error "Implementation error"

instance Show Value where
  showsPrec d = \case
    PrimValue prim -> 
      showParen (d > 10) 
        (showString "PrimValue " . showsPrec 11 prim)
    ConValue name vals -> 
      showParen (d > 10) 
        (showString ("ConValue " <> show name <> " ") . showsPrec 11 vals)
    RowValue row -> 
      showParen (d > 10) 
        (showString "RowValue " . showsPrec 11 row)
    Closure{} -> 
      showString "<<function>>"

type ValueEnv =
  ( Environment (Definition MonoType Ast)
  , Environment Value
  )

newtype Eval a = Eval { unEval :: Reader ValueEnv a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader ValueEnv
    )

eval :: Ast -> Eval Value
eval =
  cata $ \case
    EVar (ttt, var) -> do
      (x, env) <- ask
      case Env.lookup var env of
        Just val -> pure val
        Nothing -> 
          case Env.lookup var x of
            Just (Constant (_, ee)) -> do
              eval ee
            --Just (Function args (_, gg)) ->
            --  eval (eLam 1 (toList args) gg)
            Nothing -> 
              error ("Runtime error (1): " <> show var)
    ELit prim ->
      pure (PrimValue prim)
    EIf cond true false ->
      cond >>= \case
        PrimValue (PBool True) -> true
        PrimValue (PBool False) -> false
        _ -> error "Runtime error (2)"
    ELet (_, var) body expr -> do
      val <- body
      localSecond (Env.insert var val) expr
    ECall _ fun args ->
      evalCall fun args
    EOp2 (_, OLogicOr) a b ->
      a >>= \case
        PrimValue (PBool True) -> a
        _ -> b
    EOp2 (_, OLogicAnd) a b ->
      a >>= \case
        PrimValue (PBool False) -> a
        _ -> b
    EOp2 (_, OEq) a b -> do
      lhs <- a
      rhs <- b
      pure (PrimValue (PBool (lhs == rhs)))
    EOp2 (_, op) a b ->
      PrimValue <$> (evalOp2 op <$> (getPrim <$> a) <*> (getPrim <$> b))
    ECase expr cs -> do
      e <- expr
      evalCase e cs
    ERow row ->
      RowValue <$> evalRow row
    EField field expr1 expr2 -> do
      e1 <- expr1
      evalRowCase (getRow e1) field expr2

evalCall a b = do
  traceShow "****************************************"
    $ traceShow a
      $ traceShow (length b)
        $ xvalCall a b 

xvalCall :: Label MonoType -> [Eval Value] -> Eval Value
--evalCall (t, "$lam2") args = do
--  traceShow "><><><><><>"
--    $ traceShow (length args)
--      $ pure (PrimValue (PInt 1))
--
xvalCall (t, "f") args = do
  (env, vals) <- ask
  as <- sequence args
  case Env.lookup "f" env of
    Just (Constant (_, expr)) -> do
      c <- eval expr
      let Closure g vs = c
      --traceShow "********>"
      --  $ traceShow g
      --    $ traceShow (length args)
      --      $ traceShow "<********"
      evalCall g (vs <> args)
xvalCall (t, fun) args
  | arity t > length args =
    pure (Closure (t, fun) args)
  | arity t < length args =
    evalCall (t, fun) (take (arity t) args) >>= \case
      Closure c as1 -> evalCall c (as1 <> drop (arity t) args)
  | isUpper (Text.head fun) = do
    as <- sequence args
    pure (ConValue fun as)
  | otherwise = do
    (env, vals) <- ask
    as <- sequence args
    case Env.lookup fun env of
      Just (Function vs (_, body)) -> do
        --traceShowM (show (zip (snd <$> toList vs) as))
        --error (show (zip (snd <$> toList vs) as))
        --traceShowM ">>>>>>>>>>>>>>>"
        if length args > length vs 
          then do
            localSecond (Env.inserts (zip (snd <$> toList vs) as)) $ do
              x <- eval body
              case x of
                Closure c as1 -> 
                  evalCall c (as1 <> drop (length vs) args)

            --x <- localSecond (Env.inserts (zip (snd <$> toList vs) as)) (eval body)
            --case x of
            --  Closure c as1 -> 
            --    evalCall c (as1 <> drop (length vs) args)
            --evalCall (t, fun) (take (length vs) args) >>= \case
            --  Closure c as1 -> evalCall c (as1 <> drop (length vs) args)
          else
            localSecond (Env.inserts (zip (snd <$> toList vs) as)) (eval body)
      --localSecond (Env.inserts undefined) (eval body)
      Just (Constant (_, expr)) -> do
        c <- eval expr
        let Closure g vs = c
        --traceShowM "********>"
        --traceShowM g
        --traceShowM (length vs)
        --traceShowM "<********"
        evalCall g (vs <> args)
      _ ->
        case Env.lookup fun vals of
          Just (Closure g vs) -> evalCall g (vs <> args)
          _ -> error ("Runtime error (3): " <> show fun)

evalRow :: Row Ast (Label MonoType) -> Eval (Row Value Void)
evalRow =
  cata $ \case
    RNil -> pure rNil
    RVar (_, var) -> do
      (_, env) <- ask
      case Env.lookup var env of
        Just (RowValue val) -> pure val
        _ -> error "Runtime error (4)"
    RExt name v row ->
      rExt name <$> eval v <*> row

evalCase ::
  Value ->
  [([Label MonoType], Eval Value)] ->
  Eval Value
evalCase _ [] = error "Runtime error: No matching clause"
evalCase (ConValue name fields) (((_, con) : vars, value) : clauses)
  | name == con = localSecond (Env.inserts (zip (snd <$> vars) fields)) value
  | otherwise = evalCase (ConValue name fields) clauses

evalRowCase :: Row Value Void -> [Label MonoType] -> Eval Value -> Eval Value
evalRowCase row [(_, name), (_, v), (_, r)] =
  localSecond (Env.inserts [(v, p), (r, RowValue q)])
 where
  (p, q) = restrictRow name row

getPrim :: Value -> Prim
getPrim (PrimValue prim) = prim
getPrim _ = error "Runtime error (5)"

getRow :: Value -> Row Value Void
getRow (RowValue row) = row
getRow _ = error "Runtime error (6)"

evalOp2 :: Op2 -> Prim -> Prim -> Prim
evalOp2 OAdd (PFloat p) (PFloat q) = PFloat (p + q)
evalOp2 OMul (PFloat p) (PFloat q) = PFloat (p * q)
evalOp2 OSub (PFloat p) (PFloat q) = PFloat (p - q)
evalOp2 ODiv (PFloat p) (PFloat q) = PFloat (p / q)
evalOp2 OAdd (PDouble p) (PDouble q) = PDouble (p + q)
evalOp2 OMul (PDouble p) (PDouble q) = PDouble (p * q)
evalOp2 OSub (PDouble p) (PDouble q) = PDouble (p - q)
evalOp2 ODiv (PDouble p) (PDouble q) = PDouble (p / q)
evalOp2 OEq (PInt m) (PInt n) = PBool (m == n)
evalOp2 OAdd (PInt m) (PInt n) = PInt (m + n)
evalOp2 OSub (PInt m) (PInt n) = PInt (m - n)
evalOp2 OMul (PInt m) (PInt n) = PInt (m * n)
evalOp2 _ _ _ = error "Runtime error (6)"

evalProgram_ :: (Ast, [(Label Scheme, Definition MonoType Ast)]) -> Value
evalProgram_ (ast, defs) = runReader (unEval (eval ast)) (env, mempty)
  where
    env = Env.fromList (first snd <$> defs)

evalProgram__ :: (Ast, Program Scheme MonoType Ast) -> Value
evalProgram__ (ast, Program p) = evalProgram_ (ast, Map.toList p)
