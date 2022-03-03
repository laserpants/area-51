{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
module Pong.Eval where

import Data.List.NonEmpty (fromList, toList)
import Control.Monad.Reader
import Pong.Data
import Pong.Util
import qualified Pong.Util.Env as Env

data Value 
  = LitValue Literal
  | ConValue Name [Value]

deriving instance Show Value

deriving instance Eq Value

deriving instance Ord Value

eval 
  :: (MonadFix m, MonadReader (Environment (Definition (Label Type) Ast), Environment Value) m) 
  => Ast 
  -> m Value
eval = cata $ \case
  EVar (_, var) -> do
    (_, env) <- ask
    case Env.lookup var env of
      Just val -> pure val
      Nothing -> error "Runtime error"
  ECon (_, con) -> undefined
  ELit lit -> pure (LitValue lit)
  EIf cond true false -> 
    cond >>= \case 
      LitValue (LBool True) -> true 
      LitValue (LBool False) -> false
      _ -> error "Runtime error"
  ELet (_, var) body expr -> mdo
    let insertVar = localSecond (Env.insert var val)
    val <- insertVar body
    insertVar expr
  ECall _ (_, fun) args -> do
    (env, _) <- ask
    case Env.lookup fun env of
      Just (Function vs (_, body)) -> do
        as <- sequence args
        localSecond (Env.inserts (zip (snd <$> toList vs) as)) (eval body)
      _ -> error "Runtime error"
  EOp2 op a b -> evalOp2 op <$> a <*> b
  ECase expr cs -> undefined
  ERow row -> undefined

evalOp2 :: Op2 -> Value -> Value -> Value
evalOp2 OAddInt32 (LitValue (LInt32 m)) (LitValue (LInt32 n)) =
  LitValue (LInt32 (m + n))
evalOp2 _ _ _ =
  error "Runtime error"

evalCase =
  undefined

evalProgram_ :: (Ast, [(Name, Definition (Label Type) Ast)]) -> Value
evalProgram_ (ast, defs) = runReader (eval ast) (Env.fromList defs, mempty)
