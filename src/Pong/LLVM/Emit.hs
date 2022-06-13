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
import Data.Tuple.Extra (first, second, fst3, snd3, thd3)
import Data.Maybe (fromMaybe)
import Debug.Trace
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Typed as LLVM
import Pong.Data
import Pong.LLVM hiding (Typed, typeOf, void)
import Pong.Lang
import Pong.Type
import Pong.Util
import Pong.Util.Env (Environment (..))
import qualified Pong.Util.Env as Env
import TextShow (TextShow, showt)

type OpInfo = (MonoType, [MonoType], Operand)

type CodeGenEnv = Environment OpInfo

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

forEachIn ::
  (Monad m) =>
  Program MonoType t ->
  (Name -> MonoType -> Definition MonoType t -> m a) ->
  m [a]
forEachIn p f = forEachDef p (\(_, name) def -> f name (typeOf def) def)

buildProgram :: Name -> Program MonoType Ast -> LLVM.Module
buildProgram pname p = do
  buildModule (llvmRep pname) $ do
    extern "gc_malloc" [i64] charPtr
    env <-
      buildEnv $ \name t ->
        \case
          Extern args t1 -> do
            op <-
              extern
                (llvmRep name)
                (llvmType <$> args)
                (llvmType t1)
            pure [(name, (t, [], op))]
          Constant (t1, _) -> do
            pure
              [
                ( name
                ,
                  ( t
                  , []
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
                  ( t
                  , []
                  , functionRef
                      (llvmRep name)
                      (llvmType t1)
                      (llvmType . fst <$> toList args)
                  )
                )
              ]
    forEachIn p $ \name t ->
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
                    (emitBody body >>= ret . thd3)
              )
        Function args (t1, body) ->
          void $
            function
              (llvmRep name)
              (toList args <&> llvmType *** llvmRep)
              (llvmType t1)
              ( \ops -> do
                  runCodeGen
                    (Env.inserts [(n, (t, [], op)) | (op, (t, n)) <- zip ops (toList args)] env)
                    p
                    (emitBody body >>= ret . thd3)
              )
        _ ->
          pure ()
  where
    buildEnv = Env.fromList . concat <$$> forEachIn p

emitBody :: Ast -> CodeGen OpInfo
emitBody =
  cata $ \case
    ELet (t, name) expr1 expr2 -> do
      e1 <- expr1
      local (Env.insert name e1) expr2
    ELit lit -> do
      op <- emitPrim lit
      pure (typeOf lit, [], op)
    EIf expr1 expr2 expr3 -> mdo
      (_, _, ifOp) <- expr1
      condBr ifOp thenBlock elseBlock
      thenBlock <- block `named` "then"
      (_, _, thenOp) <- expr2
      br mergeBlock
      elseBlock <- block `named` "else"
      (t, _, elseOp) <- expr3
      br mergeBlock
      mergeBlock <- block `named` "ifcont"
      r <- phi [(thenOp, thenBlock), (elseOp, elseBlock)]
      pure (t, [], r)
    EOp1 op expr1 -> do
      (_, _, a) <- expr1
      r <- emitOp1Instr op a
      pure (returnType (fst op), [], r)
    EOp2 op expr1 expr2 -> do
      (_, _, a) <- expr1
      (_, _, b) <- expr2
      r <- emitOp2Instr op a b
      pure (returnType (fst op), [], r)
    EVar (t, name) ->
      Env.askLookup name <&> fromMaybe (error "Implementation error")
    ECall () (_, fun) args -> do
      emitCall fun args
    ERec{} ->
      error "TODO"
    ERes{} ->
      error "TODO"

emitCall :: Name -> [CodeGen OpInfo] -> CodeGen OpInfo
emitCall fun args = do
  as <- sequence args
  Env.askLookup fun
    >>= \case
      Just (t, _, op@LocalReference{}) -> do
        let u = returnType t
        if arity t == length as
          then do
            let sty = StructureType False [llvmType (tVar 0 ~> t)]
            s <- bitcast op (ptr sty)
            p <- gep s [int32 0, int32 0]
            q <- bitcast op charPtr 
            f <- load p 0
            r <- call f (zip (q : (thd3 <$> as)) (repeat []))
            pure (u, [], r)
          else do
            n <- uniqueName "$g"
            let zs = take (length as) (argTypes t)
                us = drop (length as) (argTypes t)
                xx0 = foldType u (tVar 0 : us)
                xx1 = foldType u (tVar 0 : zs <> us)
                --sty = StructureType False (llvmType xx0 : llvmType xx1 : (llvmType <$> zs))
                sty = StructureType False (llvmType xx0 : charPtr : (llvmType <$> zs))
                sty2 = StructureType False [llvmType xx1]
            f <- function
              (llvmRep n)
              ((charPtr, "s") : [(llvmType t, llvmRep "a") | t <- us]) 
              (llvmType u)
              ( \(a : as) -> do
                  s <- bitcast a (ptr sty)
                  --p0 <- gep s [int32 0, int32 0]
                  --p0 <- gep s [int32 0, int32 0]
                  ------yy <- load p0 0
                  --q0 <- load p0 0
                  p1 <- gep s [int32 0, int32 1]
                  q1 <- load p1 0
                  ss <- bitcast q1 (ptr sty2)
                  p2 <- gep ss [int32 0, int32 0]
                  q2 <- load p2 0
                  --p2 <- gep s [int32 0, int32 2]
                  --q2 <- load p2 0
                  bs <- forM [2..(1 + length args)] $ \i -> do
                  --bs <- forM [1..(length args)] $ \i -> do
                    pi <- gep s [int32 0, int32 (fromIntegral i)]
                    load pi 0
                  r <- call q2 (zip (q1 : bs <> as) (repeat []))
                  --r <- lift $ emitPrim (PInt 99)
                  ret r
              )
            s <- malloc sty
            p0 <- gep s [int32 0, int32 0]
            p1 <- gep s [int32 0, int32 1]
            --p2 <- gep s [int32 0, int32 2]
            ----- store p 0 f
            store p0 0 f
            xx0 <- bitcast op charPtr
            store p1 0 xx0
            forM_ (as `zip` [2..]) $ \((_, _, a), i) -> do
              pi <- gep s [int32 0, int32 (fromIntegral i)]
              store pi 0 a
            pure (foldType u us, [], s)
            --r <- emitPrim (PInt 987)
            --pure (foldType u us, [], r)

      Just (t, _, op) -> do
        let u = returnType t
        if arity t == length as
          then do
            r <- call op (zip (thd3 <$> as) (repeat []))
            pure (u, [], r)
          else do
            n <- uniqueName "$f"
            let zs = take (length as) (argTypes t)
                us = drop (length as) (argTypes t)
                xx1 = foldType u (tVar 0 : us)
                sty = StructureType False (llvmType xx1 : (llvmType <$> zs))
            f <- function
              (llvmRep n)
              ((charPtr, "s") : [(llvmType t, llvmRep "a") | t <- us]) 
              (llvmType u)
              ( \(a : as) -> do
                  s <- bitcast a (ptr sty)
                  bs <- forM [1..length args] $ \i -> do
                    pi <- gep s [int32 0, int32 (fromIntegral i)]
                    load pi 0
                  r <- call op (zip (bs <> as) (repeat []))
                  ret r
              )
            s <- malloc sty
            p <- gep s [int32 0, int32 0]
            store p 0 f
            forM_ (as `zip` [1..]) $ \((_, _, a), i) -> do
              pi <- gep s [int32 0, int32 (fromIntegral i)]
              store pi 0 a
            pure (foldType u us, [], s)

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

uniqueName :: Name -> CodeGen Name
uniqueName prefix = do
  n <- gets fst
  modify (first succ)
  pure (prefix <> showt n)

runModule :: LLVM.Module -> IO ()
runModule = Text.putStrLn . ppll

-------------------------------------------------------------------------------
-- Typeclass instances
-------------------------------------------------------------------------------

-- CodeGen
deriving instance Functor CodeGen

deriving instance Applicative CodeGen

deriving instance Monad CodeGen

deriving instance (MonadReader CodeGenEnv) CodeGen

deriving instance (MonadState (Int, Program MonoType Ast)) CodeGen

deriving instance MonadFix CodeGen

deriving instance MonadIRBuilder CodeGen

deriving instance MonadModuleBuilder CodeGen
