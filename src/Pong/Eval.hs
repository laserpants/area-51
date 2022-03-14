{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}

-- Note: Do not use StrictData in this module, since this will lead to
-- infinite recursion in evaluation of let expressions 

module Pong.Eval where

import Control.Monad.Reader
import Data.Char (isUpper)
import Data.List.NonEmpty (fromList, toList)
import qualified Data.Text as Text
import Data.Void (Void)
import Pong.Data
import Pong.Lang
import Pong.Util
import qualified Pong.Util.Env as Env

data Value
  = LitValue Prim
  | ConValue Name [Value]
  | RowValue (Row Value Void)

deriving instance Show Value

deriving instance Eq Value

deriving instance Ord Value

eval ::
     ( MonadFix m
     , MonadReader ( Environment (Definition (Label Type) Ast)
                   , Environment Value) m
     )
  => Ast
  -> m Value
eval =
  cata $ \case
    EVar (_, var) -> do
      (_, env) <- ask
      case Env.lookup var env of
        Just val -> pure val
        Nothing -> error "Runtime error"
    ELit lit -> pure (LitValue lit)
    EIf cond true false ->
      cond >>= \case
        LitValue (PBool True) -> true
        LitValue (PBool False) -> false
        _ -> error "Runtime error"
    ELet (_, var) body expr ->
      mdo let insertVar = localSecond (Env.insert var val)
          val <- insertVar body
          insertVar expr
    ECall _ (_, con) args
      | isUpper (Text.head con) -> do
        as <- sequence args
        pure (ConValue con as)
    ECall _ (_, fun) args -> do
      (env, _) <- ask
      case Env.lookup fun env of
        Just (Function vs (_, body)) -> do
          as <- sequence args
          localSecond (Env.inserts (zip (snd <$> toList vs) as)) (eval body)
        _ -> error "Runtime error"
    EOp2 (Op2 OLogicOr _) a b ->
      a >>= \case
        LitValue (PBool True) -> a
        _ -> b
    EOp2 (Op2 OLogicAnd _) a b ->
      a >>= \case
        LitValue (PBool False) -> a
        _ -> b
    EOp2 op a b ->
      LitValue <$> (evalOp2 op <$> (getPrim <$> a) <*> (getPrim <$> b))
    ECase expr cs -> do
      e <- expr
      evalCase e cs
    ERow row -> RowValue <$> evalRow row

evalRow ::
     ( MonadFix m
     , MonadReader ( Environment (Definition (Label Type) Ast)
                   , Environment Value) m
     )
  => Row Ast (Label Type)
  -> m (Row Value Void)
evalRow =
  cata $ \case
    RNil -> pure rNil
    RVar (_, var) -> do
      (_, env) <- ask
      case Env.lookup var env of
        Just (RowValue val) -> pure val
        _ -> error "Runtime error"
    RExt name v row ->
      rExt name <$> eval v <*> row

getPrim :: Value -> Prim
getPrim (LitValue lit) = lit
getPrim _ = error "Runtime error"

evalOp2 :: Op2 Type -> Prim -> Prim -> Prim
evalOp2 (Op2 OAdd _) (PFloat p) (PFloat q) = PFloat (p + q)
evalOp2 (Op2 OMul _) (PFloat p) (PFloat q) = PFloat (p * q)
evalOp2 (Op2 OSub _) (PFloat p) (PFloat q) = PFloat (p - q)
evalOp2 (Op2 ODiv _) (PFloat p) (PFloat q) = PFloat (p / q)
evalOp2 (Op2 OAdd _) (PDouble p) (PDouble q) = PDouble (p + q)
evalOp2 (Op2 OMul _) (PDouble p) (PDouble q) = PDouble (p * q)
evalOp2 (Op2 OSub _) (PDouble p) (PDouble q) = PDouble (p - q)
evalOp2 (Op2 ODiv _) (PDouble p) (PDouble q) = PDouble (p / q)
evalOp2 (Op2 OEq _) (PInt m) (PInt n) = PBool (m == n)
evalOp2 (Op2 OAdd _) (PInt m) (PInt n) = PInt (m + n)
evalOp2 (Op2 OSub _) (PInt m) (PInt n) = PInt (m - n)
evalOp2 (Op2 OMul _) (PInt m) (PInt n) = PInt (m * n)
evalOp2 _ _ _ = error "Runtime error"

evalCase ::
     ( MonadFix m
     , MonadReader ( Environment (Definition (Label Type) Ast)
                   , Environment Value) m
     )
  => Value
  -> [([Label Type], m Value)]
  -> m Value
evalCase (RowValue row) [c] = evalRowCase row c
evalCase _ [] = error "Runtime error: No matching clause"
evalCase (ConValue name fields) (((_, con):vars, value):clauses)
  | name == con = localSecond (Env.inserts (zip (snd <$> vars) fields)) value
  | otherwise = evalCase (ConValue name fields) clauses

evalRowCase ::
     ( MonadFix m
     , MonadReader ( Environment (Definition (Label Type) Ast)
                   , Environment Value) m
     )
  => Row Value Void
  -> ([Label Type], m Value)
  -> m Value
evalRowCase (Fix RNil) ([], value) = value
evalRowCase row ([(_, name), (_, v), (_, r)], value) = do
  let (p, q) = splitRow (trimLabel name) row
  localSecond (Env.inserts [(v, p), (r, RowValue q)]) value

evalProgram_ :: (Ast, [(Name, Definition (Label Type) Ast)]) -> Value
evalProgram_ (ast, defs) = runReader (eval ast) (Env.fromList defs, mempty)
