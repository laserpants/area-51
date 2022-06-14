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
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy.IO as Text
import Data.Tuple.Extra (first)
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.AST.Type as LLVM
import Debug.Trace
import qualified LLVM.AST.Typed as LLVM
import Pong.Data
import Pong.LLVM hiding (Typed, typeOf, var, void)
import Pong.Lang
import Pong.Util
import Pong.Util.Env (Environment (..))
import qualified Pong.Util.Env as Env
import TextShow (showt)

type OpInfo = (MonoType, Operand)

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
forEachIn p f = forEachDef p (\label def -> f (snd label) (typeOf def) def)

buildProgram :: Name -> Program MonoType Ast -> LLVM.Module
buildProgram pname p = do
  buildModule (llvmRep pname) $ do
    void (extern "gc_malloc" [i64] charPtr)

    --function
    --  undefined
    --  undefined
    --  undefined
    --  undefined

    env <-
      buildEnv $ \name_ t ->
        \case
          Extern args t1 -> do
            op <-
              extern
                (llvmRep name_)
                (llvmType <$> args)
                (llvmType t1)
            pure [(name_, (t, op))]
          Constant (t1, _) -> do
            pure
              [
                ( name_
                ,
                  ( t
                  , functionRef
                      (llvmRep name_)
                      (llvmType t1)
                      []
                  )
                )
              ]
          Function args (t1, _) ->
            pure
              [
                ( name_
                ,
                  ( t
                  , functionRef
                      (llvmRep name_)
                      (llvmType t1)
                      (llvmType . fst <$> toList args)
                  )
                )
              ]
          _ ->
            error "TODO"
--    let env = env <> Env.fromList 
--          [ ( "Cons"
--            , ( tVar 0 ~> tCon "List" [tVar 0] ~> tCon "List" [tVar 0]
--              , functionRef (llvmRep "Cons")
--                  charPtr
--                  undefined
--              )
--            )
--          , ( "Nil"
--            , ( tCon "List" [tVar 0]
--              , functionRef (llvmRep "Nil")
--                  charPtr
--                  undefined
--              )
--            ) 
--          ]
    forEachIn p $ \name_ _ ->
      \case
        Constant (t1, body) -> do
          void $
            function
              (llvmRep name_)
              []
              (llvmType t1)
              ( \_ -> do
                  runCodeGen
                    env
                    p
                    (emitBody body >>= ret . snd)
              )
        Function args (t1, body) ->
          void $
            function
              (llvmRep name_)
              (toList args <&> llvmType *** llvmRep)
              (llvmType t1)
              ( \ops -> do
                  runCodeGen
                    ( Env.inserts
                        [ (n, (ty, op))
                        | (op, (ty, n)) <- zip ops (toList args)
                        ]
                        env
                    )
                    p
                    (emitBody body >>= ret . snd)
              )
        _ ->
          pure ()
  where
    buildEnv = Env.fromList . concat <$$> forEachIn p

emitBody :: Ast -> CodeGen OpInfo
emitBody =
  cata $ \case
    ELet (_, var) expr1 expr2 -> do
      e1 <- expr1
      local (Env.insert var e1) expr2
    ELit lit -> do
      op <- emitPrim lit
      pure (typeOf lit, op)
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
      pure (returnType (fst op), r)
    EOp2 op expr1 expr2 -> do
      (_, a) <- expr1
      (_, b) <- expr2
      r <- emitOp2Instr op a b
      pure (returnType (fst op), r)
    EVar (_, var) ->
      Env.askLookup var <&> fromMaybe (error "Implementation error")
    ECall () (_, fun) args -> 
      emitCall fun args
    EPat expr cs ->
      emitPat expr cs
    ERec{} ->
      error "TODO: ERec"
    ERes{} ->
      error "TODO: ERes"
    ECon {} ->
      error "TODO: ECon"

emitCall :: Name -> [CodeGen OpInfo] -> CodeGen OpInfo
emitCall fun args = do
  as <- sequence args
  Env.askLookup fun
    >>= \case
      Just (t, op@LocalReference{}) -> do
        -- Partially applied function
        let u = returnType t
        if arity t == length as
          then do
            let sty = StructureType False [llvmType (tVar 0 ~> t)]
            s <- bitcast op (ptr sty)
            a <- bitcast op charPtr
            f <- loadOffset 0 s
            r <- call f (zip (a : (snd <$> as)) (repeat []))
            pure (u, r)
          else do
            n <- uniqueName "$f"
            let (ts, us) = splitAt (length as) (argTypes t)
                toFty tys = llvmType (foldType u (tVar 0 : tys))
                sty1 = StructureType False (toFty us : charPtr : (llvmType <$> ts))
                sty2 = StructureType False [toFty (ts <> us)]
            s <- malloc sty1
            storeOffset 0 s
              =<< function
                (llvmRep n)
                ((charPtr, "s") : [(llvmType uj, llvmRep "a") | uj <- us])
                (llvmType u)
                ( \(w : ws) -> do
                    s1 <- bitcast w (ptr sty1)
                    v <- loadOffset 1 s1
                    f <- loadOffset 0 =<< bitcast v (ptr sty2)
                    vs <- forM [2 .. (1 + length args)] $ \i ->
                      loadOffset i s1
                    r <- call f (zip (v : vs <> ws) (repeat []))
                    ret r
                )
            storeOffset 1 s =<< bitcast op charPtr
            forM_ (as `zip` [2 ..]) $ \((_, a), i) ->
              storeOffset i s a
            pure (foldType u us, s)
      Just (t, op) -> do
        -- Regular function pointer
        let u = returnType t
        if arity t == length as
          then do
            r <- call op (zip (snd <$> as) (repeat []))
            pure (u, r)
          else do
            n <- uniqueName "$f"
            let (ts, us) = splitAt (length as) (argTypes t)
                fty = llvmType (foldType u (tVar 0 : us))
                sty = StructureType False (fty : (llvmType <$> ts))
            s <- malloc sty
            storeOffset 0 s
              =<< function
                (llvmRep n)
                ((charPtr, "s") : [(llvmType uj, llvmRep "a") | uj <- us])
                (llvmType u)
                ( \(w : ws) -> do
                    s1 <- bitcast w (ptr sty)
                    vs <- forM [1 .. length args] $ \i ->
                      loadOffset i s1
                    r <- call op (zip (vs <> ws) (repeat []))
                    ret r
                )
            forM_ (as `zip` [1 :: Int ..]) $ \((_, a), i) ->
              storeOffset i s a
            pure (foldType u us, s)
      _ ->
        error "Implementation error"

emitPat 
  :: CodeGen (MonoType, Operand) 
  -> [Clause MonoType (CodeGen (MonoType, Operand))] 
  -> CodeGen (MonoType, Operand)
emitPat expr cs = do
  (_, e) <- expr
  traceShowM (LLVM.typeOf e)
  m <- loadOffset 0 e
  undefined

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
      _ -> error "Not implemented" -- TODO

llvmPrim :: Prim -> Constant
llvmPrim =
  \case
    PBool True -> Int 1 1
    PBool False -> Int 1 0
    PInt n -> Int 64 (toInteger n)
    PFloat f -> Float (Single f)
    PDouble d -> Float (Double d)
    PChar{} -> error "Not implemented" -- TODO
    PString{} -> error "Not implemented" -- TODO
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
    o -> error (show o)

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
globalRef name_ ty = ConstantOperand (GlobalReference ty name_)

ptrRef :: LLVM.Name -> LLVM.Type -> Operand
ptrRef name_ = globalRef name_ . ptr

functionRef :: LLVM.Name -> LLVM.Type -> [LLVM.Type] -> Operand
functionRef name_ rty atys = ptrRef name_ (FunctionType rty atys False)

loadOffset ::
  (MonadIRBuilder m, MonadModuleBuilder m) =>
  Int ->
  Operand ->
  m Operand
loadOffset i ds = do
  p <- gep ds [int32 0, int32 (fromIntegral i)]
  load p 0

storeOffset ::
  (MonadIRBuilder m, MonadModuleBuilder m) =>
  Int ->
  Operand ->
  Operand ->
  m ()
storeOffset i ds op = do
  p <- gep ds [int32 0, int32 (fromIntegral i)]
  store p 0 op

malloc :: (MonadIRBuilder m) => LLVM.Type -> m Operand
malloc ty = do
  size_ <- zext (ConstantOperand (sizeof ty)) i64
  m <- allocate size_
  bitcast m (ptr ty)
  where
    allocate n = call (functionRef "gc_malloc" charPtr [i64]) [(n, [])]

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
