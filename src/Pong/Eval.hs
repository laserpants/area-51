{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Pong.Eval where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Char (isUpper)
import Data.List.NonEmpty (fromList, toList)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Debug.Trace
import Pong.Data
import Pong.Lang
import Pong.Util
import Pong.Util.Env (Environment)
import qualified Pong.Util.Env as Env
import Text.Show.Deriving (deriveShow1)

data Value
  = PrimValue Prim
  | ConValue Name [Value]
  | RowValue (Row Value Void)
  | Closure (Label MonoType) [Eval Value]

--data ValF a
--  = VLitVal Prim
--  | VConVal Name [a]
--  | VRowVal (Row Val Void)
--  | VClosure (Label MonoType) [Eval a]
--
--type Val = Fix ValF
--
--instance Show Val where
--  show = project >>> (\case
--    VLitVal p -> show p
--    VConVal _ _ -> show "ConValue"
--    VRowVal _ -> show "RowValue"
--                     )
--    VClosure n as -> show ("Closure " <> show n <> ":" <> show (length as))

--type Value = Fix ValueF
--
---- instance Show Value where
----   show = \case
----     PrimValue p -> show p
----     ConValue _ _ -> show "ConValue"
----     RowValue _ -> show "RowValue"
----     Closure n as -> show ("Closure " <> show n <> ":" <> show (length as))

instance Eq Value where
  a == b =
    case (a, b) of
      (PrimValue p, PrimValue q) -> p == q
      (RowValue r, RowValue s) -> r == s

instance Show Value where
  show = \case
    PrimValue prim -> show prim
    ConValue name vals -> show name <> " " <> show vals
    RowValue row -> show row
    Closure{} -> "<<function>>"

----instance Eq Value where
----  a == b =
----    case (project a, project b) of
----      (PrimValue p, PrimValue q) -> p == q
----      (RowValue r, RowValue s) -> r == s

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
    EVar (_, var) -> do
      (_, env) <- ask
      case Env.lookup var env of
        Just val -> pure val
        Nothing -> error ("Runtime error (1): " <> show var)
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

evalCall :: Label MonoType -> [Eval Value] -> Eval Value
evalCall (t, fun) args
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
        localSecond (Env.inserts (zip (snd <$> toList vs) as)) (eval body)
      --localSecond (Env.inserts undefined) (eval body)
      _ ->
        case Env.lookup fun vals of
          Just (Closure g vs) -> evalCall g (vs <> args)
          _ -> error ("Runtime error (3): " <> show fun)

---- --evalCall :: (MonadFix m) => Label Type -> [Value m] -> EvalT m (Value m)
---- --evalCall (t, fun) args
---- --  | "i" == fun = do
---- --        (env, vals) <- ask
---- --        --let boo = Env.lookup fun vals
---- --        error (show vals)
---- --        --        Just (Closure g vs) -> do
---- --        --          error "XXX"
---- --        --          --evalCall g (vs <> args)
---- --        --        _ -> error ("Runtime error (r3) : " <> show fun)
---- --  | arity t > length args = pure (Closure (t, fun) args)
---- --  | isUpper (Text.head fun) =
---- --      pure (ConValue fun args)
---- --  | otherwise = do
---- --      (env, vals) <- ask
---- --      case Env.lookup fun env of
---- --        Just (Function vs (_, body)) -> do
---- --          localSecond (Env.inserts (zip (snd <$> toList vs) args)) (eval body)
---- --        _ -> case Env.lookup fun vals of
---- --                Just (Closure g vs) -> do
---- --                  evalCall g (vs <> args)
---- --                _ -> error ("Runtime error (3) : " <> show fun)

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
  (p, q) = splitRow name row

---- --eval ::
---- --     ( MonadFix m
---- --     , MonadReader ( Environment (Definition Type Ast)
---- --                   , Environment Value) m
---- --     )
---- --  => Ast
---- --  -> m Value
---- --eval =
---- --  cata $ \case
---- --    EVar (_, var) -> do
---- --      (_, env) <- ask
---- --      case Env.lookup var env of
---- --        Just val -> pure val
---- --        Nothing -> error ("Runtime error (1) : " <> show var)
---- --    ELit lit -> pure (PrimValue lit)
---- --    EIf cond true false ->
---- --      cond >>= \case
---- --        PrimValue (PBool True) -> true
---- --        PrimValue (PBool False) -> false
---- --        _ -> error "Runtime error (2)"
---- --    ELet (_, var) body expr -> do
---- --      --mdo let insertVar = localSecond (Env.insert var val)
---- --      --    val <- insertVar body
---- --      --    insertVar expr
---- --      mdo val <- localSecond (Env.insert var val) body
---- --          localSecond (Env.insert var val) expr
---- --    ECall _ fun args -> do
---- --      as <- sequence args
---- --      evalCall fun as
---- --    EOp2 (Op2 OLogicOr _) a b ->
---- --      a >>= \case
---- --        PrimValue (PBool True) -> a
---- --        _ -> b
---- --    EOp2 (Op2 OLogicAnd _) a b ->
---- --      a >>= \case
---- --        PrimValue (PBool False) -> a
---- --        _ -> b
---- --    EOp2 (Op2 OEq _) a b -> do
---- --      lhs <- a
---- --      rhs <- b
---- --      pure (PrimValue (PBool (lhs == rhs)))
---- --    EOp2 op a b ->
---- --      PrimValue <$> (evalOp2 op <$> (getPrim <$> a) <*> (getPrim <$> b))
---- --    ECase expr cs -> do
---- --      e <- expr
---- --      evalCase e cs
---- --    ERow row -> RowValue <$> evalRow row
---- --    EField field expr1 expr2 -> do
---- --      e1 <- expr1
---- --      evalRowCase (getRow e1) field expr2

-- evalCall ::
--      ( MonadFix m
--      , MonadReader ( Environment (Definition MonoType Ast)
--                    , Environment Value) m
--      )
--   => Label MonoType
--   -> [Value]
--   -> m Value
-- evalCall =
--   undefined
---- --evalCall (t, fun) args
---- --  | "i" == fun = do
---- --        (env, vals) <- ask
---- --        --let boo = Env.lookup fun vals
---- --        error (show vals)
---- --        --        Just (Closure g vs) -> do
---- --        --          error "XXX"
---- --        --          --evalCall g (vs <> args)
---- --        --        _ -> error ("Runtime error (r3) : " <> show fun)
---- --  | arity t > length args = pure (Closure (t, fun) args)
---- --  | isUpper (Text.head fun) =
---- --      pure (ConValue fun args)
---- --  | otherwise = do
---- --      (env, vals) <- ask
---- --      case Env.lookup fun env of
---- --        Just (Function vs (_, body)) -> do
---- --          localSecond (Env.inserts (zip (snd <$> toList vs) args)) (eval body)
---- --        _ -> case Env.lookup fun vals of
---- --                Just (Closure g vs) -> do
---- --                  evalCall g (vs <> args)
---- --                _ -> error ("Runtime error (3) : " <> show fun)

---- --evalRow ::
---- --     ( MonadFix m
---- --     , MonadReader ( Environment (Definition Type Ast)
---- --                   , Environment Value) m
---- --     )
---- --  => Row Ast (Label Type)
---- --  -> m (Row Value Void)
---- --evalRow =
---- --  cata $ \case
---- --    RNil -> pure rNil
---- --    RVar (_, var) -> do
---- --      (_, env) <- ask
---- --      case Env.lookup var env of
---- --        Just (RowValue val) -> pure val
---- --        _ -> error "Runtime error (4)"
---- --    RExt name v row ->
---- --      rExt name <$> eval v <*> row

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

--evalCase =
--  undefined

---- --evalCase ::
---- --     ( MonadFix m
---- --     , MonadReader ( Environment (Definition Type Ast)
---- --                   , Environment Value) m
---- --     )
---- --  => Value
---- --  -> [([Label Type], m Value)]
---- --  -> m Value
---- --evalCase _ [] = error "Runtime error: No matching clause"
---- --evalCase (ConValue name fields) (((_, con):vars, value):clauses)
---- --  | name == con = localSecond (Env.inserts (zip (snd <$> vars) fields)) value
---- --  | otherwise = evalCase (ConValue name fields) clauses

---- --evalRowCase ::
---- --     ( MonadFix m
---- --     , MonadReader ( Environment (Definition Type Ast)
---- --                   , Environment Value) m
---- --     )
---- --  => Row Value Void
---- --  -> [Label Type]
---- --  -> m Value
---- --  -> m Value
---- ----evalRowCase (Fix RNil) ([(_, "{}")], value) = value
---- --evalRowCase row [(_, name), (_, v), (_, r)] value = do
---- --  let (p, q) = splitRow (trimLabel name) row
---- --  localSecond (Env.inserts [(v, p), (r, RowValue q)]) value
---- ----evalRowCase row ([(_, name)], value) = do
---- ----  localSecond (Env.insert name (RowValue row)) value
---- --
---- --evalProgram_ :: (Ast, [(Name, Definition Type Ast)]) -> Value

--evalProgram_ :: (Ast, [(Name, Definition MonoType Ast)]) -> Value
--evalProgram_ (ast, defs) = runReader (eval ast) (Env.fromList defs, mempty)

evalProgram__ :: (Ast, Program MonoType Ast) -> Value
evalProgram__ (ast, Program p) = evalProgram_ (ast, Map.toList p)

evalProgram_ :: (Ast, [(Name, Definition MonoType Ast)]) -> Value
evalProgram_ (ast, defs) = runIdentity (runReaderT (unEval (eval ast)) (Env.fromList defs, mempty))

--deriving instance (Show a) => Show (Eval a)
--
--deriveShow1 ''Eval
--
----
--
--deriving instance (Show a) => Show (ValF a)
--
--deriveShow1 ''ValF

--deriving instance Functor ValF
