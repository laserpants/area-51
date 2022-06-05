{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Pong.LLVM.Emit where

import Control.Monad.Reader
import Control.Monad.State
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy.IO as Text
import Data.Tuple.Extra (first, second)
import Debug.Trace
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Typed as LLVM
import Pong.Data
import Pong.LLVM hiding (Typed, typeOf, void)
import Pong.Lang
import Pong.TypeChecker
import Pong.Util
import Pong.Util.Env (Environment (..))
import qualified Pong.Util.Env as Env
import TextShow (TextShow, showt)

data OpType
  = Op MonoType
  | Partial MonoType [MonoType]

instance Typed OpType where
  typeOf =
    \case
      Op t -> t
      Partial t _ -> t

type Info = (OpType, Operand)

type CodeGenEnv = Environment Info

newtype CodeGen a = CodeGen
  { getCodeGen ::
      StateT
        (Int, Program MonoType Ast)
        (ReaderT CodeGenEnv (IRBuilderT ModuleBuilder))
        a
  }

runCodeGen ::
  CodeGenEnv ->
  Program MonoType Ast ->
  CodeGen a ->
  IRBuilderT ModuleBuilder a
runCodeGen env prog (CodeGen code) = runReaderT (evalStateT code (1, prog)) env

llvmRep :: (IsString s) => Name -> s
llvmRep = fromString <<< unpack

charPtr :: LLVM.Type
charPtr = ptr i8

forEachDef3 ::
  (Monad m) =>
  Program MonoType t ->
  (Name -> MonoType -> Definition MonoType t -> m a) ->
  m [a]
forEachDef3 p f = forEachDef p (\label def -> f (snd label) (typeOf def) def)

buildEnv ::
  Program MonoType Ast ->
  (Name  -> MonoType -> Definition MonoType Ast -> ModuleBuilder [(Name, Info)]) ->
  ModuleBuilder CodeGenEnv
buildEnv p = Env.fromList . concat <$$> forEachDef3 p

buildProgram :: Name -> Program MonoType Ast -> LLVM.Module
buildProgram pname p = do
  buildModule (llvmRep pname) $ do
    env <-
      buildEnv p $ \name t ->
        \case
          Extern args t1 -> do
            op <-
              extern
                (llvmRep name)
                (llvmType <$> args)
                (llvmType t1)
            pure [(name, (Op t, op))]
          Constant (t1, _) -> do
            pure
              [
                ( name
                ,
                  ( Op t
                  , functionRef
                      (llvmRep name)
                      (llvmType t1)
                      []
                  )
                )
              ]
          Function args (t1, _) ->
            pure
              [
                ( name
                ,
                  ( Op t
                  , functionRef
                      (llvmRep name)
                      (llvmType t1)
                      (llvmType . fst <$> toList args)
                  )
                )
              ]
    forEachDef3 p $ \name t ->
      \case
        Constant (t1, body) -> do
          void $
            function
              (llvmRep name)
              []
              (llvmType t1)
              ( \ops -> do
                  runCodeGen
                    env
                    p
                    (emitBody body >>= ret . snd)
              )
        Function args (t1, body) ->
          void $
            function
              (llvmRep name)
              (toList args <&> llvmType *** llvmRep)
              (llvmType t1)
              ( \ops -> do
                  runCodeGen
                    (Env.inserts [(n, (Op t, op)) | (op, (t, n)) <- zip ops (toList args)] env)
                    p
                    (emitBody body >>= ret . snd)
              )
        _ ->
          pure ()

emitBody :: Ast -> CodeGen Info
emitBody =
  cata $ \case
    ELet (t, name) expr1 expr2 -> do
      e1 <- expr1
      local (Env.insert name e1) expr2
    ELit lit -> do
      op <- emitPrim lit
      pure (Op (typeOf lit), op)
    EIf expr1 expr2 expr3 -> mdo
      (_, ifOp) <- expr1
      condBr ifOp thenBlock elseBlock
      thenBlock <- block `named` "then"
      (_, thenOp) <- expr2
      br mergeBlock
      elseBlock <- block `named` "else"
      (t, elseOp) <- expr3
      br mergeBlock
      mergeBlock <- block `named` "ifcont"
      r <- phi [(thenOp, thenBlock), (elseOp, elseBlock)]
      pure (t, r)
    EOp1 op expr1 -> do
      (_, a) <- expr1
      r <- emitOp1Instr op a
      pure (Op (returnType (fst op)), r)
    EOp2 op expr1 expr2 -> do
      (_, a) <- expr1
      (_, b) <- expr2
      r <- emitOp2Instr op a b
      pure (Op (returnType (fst op)), r)
    EVar (t, name) ->
      Env.askLookup name
        >>= \case
          Just c -> pure c
          _ -> error "Implementation error"
    ECall () (_, fun) args -> do
      emitCall fun args

emitCall :: Name -> [CodeGen Info] -> CodeGen Info
emitCall fun args = do
  as <- sequence args
  Env.askLookup fun
    >>= \case
      Just (Op t, op) ->
        if arity t == length as
          then do
            r <- call op (zip (snd <$> as) (repeat []))
            pure (Op (returnType t), r)
          else partial t op as
      Just (Partial t ts, op) -> do
        let sty = StructureType False (llvmType t : (llvmType <$> ts))
        s <- bitcast op (ptr sty)
        p <- gep s [int32 0, int32 0]
        f <- load p 0
        bs <- forM (zip (unwindType t) [1 .. length ts]) $ \(ti, i) -> do
          pi <- gep s [int32 0, int32 (fromIntegral i)]
          r <- load pi 0
          pure (Op ti, r)

        if arity t == length bs + length as
          then do
            r <- call f (zip (snd <$> (bs <> as)) (repeat []))
            pure (Op (returnType t), r)
          else 
            partial t f (bs <> as)
      _ -> do
        error (show fun) -- TODO: remove

partial :: MonoType -> Operand -> [(OpType, Operand)] -> CodeGen Info
partial t op as = do
  let sty = StructureType False (llvmType t : (LLVM.typeOf . snd <$> as))
  s <- malloc sty
  p <- gep s [int32 0, int32 0]
  store p 0 op
  forM_ (as `zip` [1 ..]) $ \((_, arg), i) -> do
    q <- gep s [int32 0, int32 i]
    store q 0 arg
  pure (Partial t (typeOf . fst <$> as), s)

-- | Translate a language type to its equivalent LLVM type
llvmType :: MonoType -> LLVM.Type
llvmType =
  project
    >>> \case
      TUnit -> StructureType False []
      TBool{} -> LLVM.i1
      TInt{} -> LLVM.i64
      TFloat{} -> LLVM.float
      TDouble{} -> LLVM.double
      TVar{} -> charPtr
      TChar{} -> error "Not implemented" -- TODO
      TString{} -> error "Not implemented" -- TODO
      ty@TArr{} -> ptr funTy
        where
          types = llvmType <$> unwindType (embed ty)
          funTy =
            FunctionType
              { argumentTypes = init types
              , resultType = last types
              , isVarArg = False
              }

llvmPrim :: Prim -> Constant
llvmPrim =
  \case
    PBool True -> Int 1 1
    PBool False -> Int 1 0
    PInt n -> Int 64 (toInteger n)
    PFloat f -> Float (Single f)
    PDouble d -> Float (Double d)
    PChar c -> error "Not implemented" -- TODO
    PString s -> error "Not implemented" -- TODO
    PUnit -> Struct Nothing False []

emitPrim :: Prim -> CodeGen Operand
emitPrim = pure <<< ConstantOperand <<< llvmPrim

emitOp1Instr :: (MonoType, Op1) -> Operand -> CodeGen Operand
emitOp1Instr =
  \case
    (_, ONot) -> undefined
    (t, ONeg) | (tInt ~> tInt) == t -> undefined
    (t, ONeg) | (tFloat ~> tFloat) == t -> undefined
    (t, ONeg) | (tFloat ~> tFloat) == t -> undefined

emitOp2Instr :: (MonoType, Op2) -> Operand -> Operand -> CodeGen Operand
emitOp2Instr =
  \case
    (t, OEq) | (tInt ~> tInt ~> tBool) == t -> icmp LLVM.EQ
    (t, ONEq) | (tInt ~> tInt ~> tBool) == t -> icmp LLVM.NE
    (t, OAdd) | (tInt ~> tInt ~> tInt) == t -> add
    (t, OSub) | (tInt ~> tInt ~> tInt) == t -> sub
    (t, OMul) | (tInt ~> tInt ~> tInt) == t -> mul
    (t, OAdd) | (tFloat ~> tFloat ~> tFloat) == t -> fadd
    (t, OAdd) | (tDouble ~> tDouble ~> tDouble) == t -> fadd
    (t, OMul) | (tFloat ~> tFloat ~> tFloat) == t -> fmul
    (t, OMul) | (tDouble ~> tDouble ~> tDouble) == t -> fmul
    (t, OSub) | (tFloat ~> tFloat ~> tFloat) == t -> fsub
    (t, OSub) | (tDouble ~> tDouble ~> tDouble) == t -> fsub
    (t, ODiv) | (tFloat ~> tFloat ~> tFloat) == t -> fdiv
    (t, ODiv) | (tDouble ~> tDouble ~> tDouble) == t -> fdiv
    (t, OGt) | (tInt ~> tInt ~> tBool) == t -> icmp LLVM.UGT
    (t, OGtE) | (tInt ~> tInt ~> tBool) == t -> icmp LLVM.UGE
    (t, OLt) | (tInt ~> tInt ~> tBool) == t -> icmp LLVM.ULT
    (t, OLtE) | (tInt ~> tInt ~> tBool) == t -> icmp LLVM.ULE
    o -> error (show o)

globalRef :: LLVM.Name -> LLVM.Type -> Operand
globalRef name ty = ConstantOperand (GlobalReference ty name)

ptrRef :: LLVM.Name -> LLVM.Type -> Operand
ptrRef name = globalRef name . ptr

functionRef :: LLVM.Name -> LLVM.Type -> [LLVM.Type] -> Operand
functionRef name rty argtys = ptrRef name (FunctionType rty argtys False)

malloc :: (MonadIRBuilder m) => LLVM.Type -> m Operand
malloc ty = do
  size <- zext (ConstantOperand (sizeof ty)) i64
  m <- allocate size
  bitcast m (ptr ty)
  where
    allocate size = call (functionRef "gc_malloc" charPtr [i64]) [(size, [])]

runModule :: LLVM.Module -> IO ()
runModule = Text.putStrLn . ppll

-- OpType
deriving instance Show OpType

deriving instance Eq OpType

deriving instance Ord OpType

-- CodeGen
deriving instance Functor CodeGen

deriving instance Applicative CodeGen

deriving instance Monad CodeGen

deriving instance (MonadReader CodeGenEnv) CodeGen

deriving instance (MonadState (Int, Program MonoType Ast)) CodeGen

deriving instance MonadFix CodeGen

deriving instance MonadIRBuilder CodeGen

deriving instance MonadModuleBuilder CodeGen
