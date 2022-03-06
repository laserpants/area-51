{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}

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
  = LitValue Literal
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
        LitValue (LBool True) -> true
        LitValue (LBool False) -> false
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
    EOp2 OLogicOr a b ->
      a >>= \case
        LitValue (LBool True) -> a
        _ -> b
    EOp2 OLogicAnd a b ->
      a >>= \case
        LitValue (LBool False) -> a
        _ -> b
    EOp2 op a b ->
      LitValue <$> (evalOp2 op <$> (getLiteral <$> a) <*> (getLiteral <$> b))
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
    RExt name v row -> do
      rExt name <$> eval v <*> row

getLiteral :: Value -> Literal
getLiteral (LitValue lit) = lit
getLiteral _ = error "Runtime error"

evalOp2 :: Op2 -> Literal -> Literal -> Literal
evalOp2 OEqInt32 (LInt32 m) (LInt32 n) = LBool (m == n)
evalOp2 OAddInt32 (LInt32 m) (LInt32 n) = LInt32 (m + n)
evalOp2 OSubInt32 (LInt32 m) (LInt32 n) = LInt32 (m - n)
evalOp2 OMulInt32 (LInt32 m) (LInt32 n) = LInt32 (m * n)
evalOp2 OAddFloat (LFloat p) (LFloat q) = LFloat (p + q)
evalOp2 OMulFloat (LFloat p) (LFloat q) = LFloat (p * q)
evalOp2 OSubFloat (LFloat p) (LFloat q) = LFloat (p - q)
evalOp2 ODivFloat (LFloat p) (LFloat q) = LFloat (p / q)
evalOp2 OAddDouble (LDouble p) (LDouble q) = LDouble (p + q)
evalOp2 OMulDouble (LDouble p) (LDouble q) = LDouble (p * q)
evalOp2 OSubDouble (LDouble p) (LDouble q) = LDouble (p - q)
evalOp2 ODivDouble (LDouble p) (LDouble q) = LDouble (p / q)
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
