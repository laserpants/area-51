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
import Control.Newtype.Generics (unpack)
import Data.Char (isUpper, ord)
import Data.Function ((&))
import Data.List (sort, sortOn)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import Data.String (IsString, fromString)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Text
import Data.Tuple.Extra (first)
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.FloatingPointPredicate as Float
import qualified LLVM.AST.IntegerPredicate as Int
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Typed as LLVM
import Pong.Data
import Pong.LLVM hiding (Typed, name, typeOf, var, void)
import Pong.Lang
import Pong.Util (Fix (..), Name, cata, embed, project, (***), (<$$>), (<&>), (<<<), (>>>))
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
llvmRep = fromString <<< Text.unpack

charPtr :: LLVM.Type
charPtr = ptr i8

revealOp :: Operand -> MonoType -> CodeGen Operand
revealOp op =
  project >>> \case
    TInt ->
      ptrtoint op i64
    TBool ->
      ptrtoint op i1
    TUnit ->
      ptrtoint op i1
    TFloat -> do
      p <- bitcast op (ptr LLVM.float)
      load p 0
    TDouble -> do
      p <- bitcast op (ptr LLVM.double)
      load p 0
    _ ->
      pure op

concealOp :: Operand -> CodeGen Operand
concealOp op =
  case LLVM.typeOf op of
    IntegerType 1 ->
      inttoptr op charPtr
    IntegerType 8 ->
      inttoptr op charPtr
    IntegerType 64 ->
      inttoptr op charPtr
    FloatingPointType FloatFP -> do
      p <- malloc LLVM.float
      store p 0 op
      bitcast p charPtr
    FloatingPointType DoubleFP -> do
      p <- malloc LLVM.double
      store p 0 op
      bitcast p charPtr
    _ ->
      bitcast op charPtr

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
    void (extern "hashmap_init" [] charPtr)
    void (extern "hashmap_lookup" [charPtr, charPtr] charPtr)
    void (extern "hashmap_insert" [charPtr, charPtr, charPtr] LLVM.void)
    env <-
      buildEnv $ \defn t ->
        \case
          Extern args t1 -> do
            op <-
              extern
                (llvmRep defn)
                (llvmType <$> args)
                (llvmType t1)
            pure [(defn, (t, op))]
          Constant (t1, _) ->
            pure
              [
                ( defn
                ,
                  ( t
                  , functionRef
                      (llvmRep defn)
                      (llvmType t1)
                      []
                  )
                )
              ]
          Function args (t1, _) ->
            pure
              [
                ( defn
                ,
                  ( t
                  , functionRef
                      (llvmRep defn)
                      (llvmType t1)
                      (llvmType . fst <$> toList args)
                  )
                )
              ]
          _ ->
            pure []
    forEachIn p $ \defn _ ->
      \case
        Constant{}
          | isUpper (Text.head defn) ->
              case lookup (normalizedCon defn) consIndices of
                Just n ->
                  void $
                    function
                      (llvmRep defn)
                      []
                      charPtr
                      ( \_ ->
                          runCodeGen env p $ do
                            let sty = StructureType False [i8]
                            s <- malloc sty
                            storeOffset s 0 (int8 n)
                            a <- bitcast s charPtr
                            ret a
                      )
                _ -> error "Implementation error"
        Function args _
          | isUpper (Text.head defn) ->
              case lookup (normalizedCon defn) consIndices of
                Just n ->
                  void $
                    function
                      (llvmRep defn)
                      (toList args <&> llvmType *** llvmRep)
                      charPtr
                      ( \ops -> do
                          runCodeGen env p $ do
                            let sty = StructureType False (i8 : (llvmType . fst <$> toList args))
                            s <- malloc sty
                            storeRange s 0 (int8 n : ops)
                            a <- bitcast s charPtr
                            ret a
                      )
                _ -> error "Implementation error"
        Constant (t1, body) ->
          void $
            function
              (llvmRep defn)
              []
              (llvmType t1)
              ( \_ ->
                  runCodeGen
                    env
                    p
                    (emitBody body >>= ret . snd)
              )
        Function args (t1, body) ->
          void $
            function
              (llvmRep defn)
              (toList args <&> llvmType *** llvmRep)
              (llvmType t1)
              ( \ops ->
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
    consIndices =
      unpack p
        & Map.toList
        & concatMap
          ( \case
              (_, Data _ cs) ->
                sort (conName <$> cs) `zip` [0 :: Integer ..]
              _ ->
                []
          )
    normalizedCon con =
      case Text.splitOn "-" con of
        [c, _] -> c
        _ -> con

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
    EOp1 (_, ONot) expr1 -> do
      (_, a) <- expr1
      r <- icmp Int.EQ (ConstantOperand (Int 1 0)) a
      pure (tBool, r)
    EOp1 (t, ONeg) expr1 -> do
      (_, a) <- expr1
      r <- emitNegOp a t
      pure (returnType t, r)
    EOp2 (_, OLogicOr) expr1 expr2 -> mdo
      (_, a) <- expr1
      condBr a op1Block op2Block
      op1Block <- block `named` "op1"
      br mergeBlock
      op2Block <- block `named` "op2"
      (_, b) <- expr2
      br mergeBlock
      mergeBlock <- block `named` "mergeOr"
      r <- phi [(a, op1Block), (b, op2Block)]
      pure (tBool, r)
    EOp2 (_, OLogicAnd) expr1 expr2 -> mdo
      (_, a) <- expr1
      condBr a op1Block op2Block
      op1Block <- block `named` "op1"
      (_, b) <- expr2
      br mergeBlock
      op2Block <- block `named` "op2"
      br mergeBlock
      mergeBlock <- block `named` "mergeAnd"
      r <- phi [(b, op1Block), (a, op2Block)]
      pure (tBool, r)
    EOp2 op expr1 expr2 -> do
      (_, a) <- expr1
      (_, b) <- expr2
      r <- emitOp2Instr op a b
      pure (returnType (fst op), r)
    EVar (t, var) | isUpper (Text.head var) && 0 == arity t -> do
      r <- call (functionRef (llvmRep var) charPtr []) []
      pure (t, r)
    EVar (t, "{{data}}") -> do
      r <- emitPrim PUnit
      s <- inttoptr r charPtr
      pure (t, s)
    EVar (_, var) ->
      Env.askLookup var
        >>= \case
          Just (t, op@(ConstantOperand (GlobalReference PointerType{pointerReferent = FunctionType{}} _)))
            | arity t == 0 -> do
                r <- call op []
                pure (t, r)
          Just op ->
            pure op
          Nothing -> error ("Not in scope: " <> show var)
    ECall () (_, fun) args ->
      emitCall fun args
    EPat expr_ cs ->
      emitPat expr_ cs
    ERec row ->
      emitRow row
    ERes fs e1 e2 ->
      emitRes fs e1 e2
    ECon{} ->
      error "Implementation error"

emitRes :: [Label MonoType] -> CodeGen OpInfo -> CodeGen OpInfo -> CodeGen OpInfo
emitRes [(_, field), (t0, v), (t1, r)] a1 a2 = do
  str <- uniqueName "$str"
  f <- globalStringPtr (Text.unpack field) (llvmRep str)
  (_, hmap) <- a1
  p <- call (functionRef "hashmap_lookup" charPtr []) (zip [hmap, ConstantOperand f] (repeat []))
  op <- revealOp p t0
  local (Env.inserts [(v, (t0, op)), (r, (t1, hmap))]) a2
emitRes _ _ _ = error "Implementation error"

emitRow :: Row Ast (Label MonoType) -> CodeGen OpInfo
emitRow (Fix RNil) = do
  hmap <- call (functionRef "hashmap_init" charPtr []) []
  pure (tRec rNil, hmap)
emitRow (Fix (RVar (_, var))) = do
  Env.askLookup var
    >>= \case
      Just op ->
        pure op
      Nothing -> error ("Not in scope: " <> show var)
emitRow row = do
  let (fvs, r) = unwindRow row
  (_, hmap) <- emitRow r
  forM_ (Map.toList fvs) $ \(field, exprs) -> do
    es <- traverse emitBody exprs
    str <- uniqueName "$str"
    f <- globalStringPtr (Text.unpack field) (llvmRep str)
    -- TODO
    let (_, e) = head es
    v <- concealOp e
    call (functionRef "hashmap_insert" LLVM.void []) (zip [hmap, ConstantOperand f, v] (repeat []))
  -- TODO
  -- forM_ es $ \(_, e) -> do
  --  v <- concealOp e
  --  call (functionRef "hashmap_insert" LLVM.void []) (zip [hmap, ConstantOperand k, v] (repeat []))
  pure (typeOf row, hmap)

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
            f <- loadOffset s 0
            r <- call f (zip (a : (snd <$> as)) (repeat []))
            pure (u, r)
          else do
            n <- uniqueName "$f"
            let (ts, us) = splitAt (length as) (argTypes t)
                toFty tys = llvmType (foldType u (tVar 0 : tys))
                sty1 = StructureType False (toFty us : charPtr : (llvmType <$> ts))
                sty2 = StructureType False [toFty (ts <> us)]
            s <- malloc sty1
            storeOffset s 0
              =<< function
                (llvmRep n)
                ((charPtr, "s") : [(llvmType uj, llvmRep "a") | uj <- us])
                (llvmType u)
                ( \(w : ws) -> do
                    s1 <- bitcast w (ptr sty1)
                    v <- loadOffset s1 1
                    s2 <- bitcast v (ptr sty2)
                    f <- loadOffset s2 0
                    vs <- loadRange s1 [2 .. 1 + length args]
                    r <- call f (zip (v : vs <> ws) (repeat []))
                    ret r
                )
            storeOffset s 1 =<< bitcast op charPtr
            storeRange s 2 (snd <$> as)
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
            storeOffset s 0
              =<< function
                (llvmRep n)
                ((charPtr, "s") : [(llvmType uj, llvmRep "a") | uj <- us])
                (llvmType u)
                ( \(w : ws) -> do
                    s1 <- bitcast w (ptr sty)
                    vs <- loadRange s1 [1 .. length args]
                    r <- call op (zip (vs <> ws) (repeat []))
                    ret r
                )
            storeRange s 1 (snd <$> as)
            pure (foldType u us, s)
      _ ->
        error "Implementation error"

emitPat :: CodeGen OpInfo -> [Clause MonoType (CodeGen OpInfo)] -> CodeGen OpInfo
emitPat expr_ cs = mdo
  (_, e) <- expr_
  let sty = StructureType False [i8]
  s <- bitcast e (ptr sty)
  m <- loadOffset s 0
  let names = blocks <&> snd
  switch
    m
    (last names) -- Default case
    [ (Int 8 (fromIntegral i), blk)
    | (i, blk) <- zip [0 :: Integer ..] (init names)
    ]
  blocks <-
    forM (sortOn (snd . head . fst) cs) $ \((t, _) : fields, body) -> do
      blk <-
        block `named` "case"
      r <-
        if null fields
          then body
          else do
            s1 <- bitcast e (ptr (StructureType False (i8 : (argTypes t <&> llvmType))))
            ops <- zip (argTypes t) <$> loadRange s1 [1 .. arity t]
            local (Env.inserts (zip (fields <&> snd) ops)) body
      br end
      cb <- currentBlock
      pure ((r, cb), blk)
  end <- block `named` "end"
  op <- phi (first snd . fst <$> blocks)
  pure (fst (fst (fst (head blocks))), op)

-- | Translate a language type to its equivalent LLVM type
llvmType :: MonoType -> LLVM.Type
llvmType =
  project
    >>> \case
      TUnit -> i1
      TBool{} -> i1
      TInt{} -> i64
      TFloat{} -> LLVM.float
      TDouble{} -> LLVM.double
      TVar{} -> charPtr
      TString{} -> charPtr
      TChar{} -> i8
      ty@TArr{} -> ptr funTy
        where
          types = llvmType <$> unwindType (embed ty)
          funTy =
            FunctionType
              { argumentTypes = init types
              , resultType = last types
              , isVarArg = False
              }
      _ -> charPtr

llvmPrim :: Prim -> CodeGen Constant
llvmPrim =
  \case
    PString s -> do
      str <- uniqueName "$str"
      globalStringPtr (Text.unpack s) (llvmRep str)
    p ->
      pure
        ( p & \case
            PBool True ->
              Int 1 1
            PBool False ->
              Int 1 0
            PInt n ->
              Int 64 (toInteger n)
            PFloat f ->
              Float (Single f)
            PDouble d ->
              Float (Double d)
            PChar c ->
              Int 8 (fromIntegral (ord c))
            PUnit -> Int 1 0
            _ ->
              error "Implementation error"
        )

emitPrim :: Prim -> CodeGen Operand
emitPrim = ConstantOperand <$$> llvmPrim

emitNegOp :: Operand -> MonoType -> CodeGen Operand
emitNegOp op =
  \case
    t | (tInt ~> tInt) == t -> sub (ConstantOperand (Int 64 0)) op
    t | (tFloat ~> tFloat) == t -> sub (ConstantOperand (Float (Single 0))) op
    t | (tDouble ~> tDouble) == t -> sub (ConstantOperand (Float (Double 0))) op
    _ -> error "Not implemented"

emitOp2Instr :: (MonoType, Op2) -> Operand -> Operand -> CodeGen Operand
emitOp2Instr =
  \case
    (t, OEq) | (tInt ~> tInt ~> tBool) == t -> icmp Int.EQ
    (t, ONEq) | (tInt ~> tInt ~> tBool) == t -> icmp Int.NE
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
    (t, OGt) | (tInt ~> tInt ~> tBool) == t -> icmp Int.UGT
    (t, OGtE) | (tInt ~> tInt ~> tBool) == t -> icmp Int.UGE
    (t, OLt) | (tInt ~> tInt ~> tBool) == t -> icmp Int.ULT
    (t, OLtE) | (tInt ~> tInt ~> tBool) == t -> icmp Int.ULE
    (t, OGt) | (tFloat ~> tFloat ~> tBool) == t -> fcmp Float.UGT
    (t, OGtE) | (tFloat ~> tFloat ~> tBool) == t -> fcmp Float.UGE
    (t, OLt) | (tFloat ~> tFloat ~> tBool) == t -> fcmp Float.ULT
    (t, OLtE) | (tFloat ~> tFloat ~> tBool) == t -> fcmp Float.ULE
    (t, OGt) | (tDouble ~> tDouble ~> tBool) == t -> fcmp Float.UGT
    (t, OGtE) | (tDouble ~> tDouble ~> tBool) == t -> fcmp Float.UGE
    (t, OLt) | (tDouble ~> tDouble ~> tBool) == t -> fcmp Float.ULT
    (t, OLtE) | (tDouble ~> tDouble ~> tBool) == t -> fcmp Float.ULE
    o -> error ("Not implemented: " <> show o)

globalRef :: LLVM.Name -> LLVM.Type -> Operand
globalRef name_ ty = ConstantOperand (GlobalReference ty name_)

ptrRef :: LLVM.Name -> LLVM.Type -> Operand
ptrRef name_ = globalRef name_ . ptr

functionRef :: LLVM.Name -> LLVM.Type -> [LLVM.Type] -> Operand
functionRef name_ rty atys = ptrRef name_ (FunctionType rty atys False)

loadOffset ::
  (MonadIRBuilder m, MonadModuleBuilder m) =>
  Operand ->
  Int ->
  m Operand
loadOffset ds i = do
  p <- gep ds [int32 0, int32 (fromIntegral i)]
  load p 0

storeOffset ::
  (MonadIRBuilder m, MonadModuleBuilder m) =>
  Operand ->
  Int ->
  Operand ->
  m ()
storeOffset ds i op = do
  p <- gep ds [int32 0, int32 (fromIntegral i)]
  store p 0 op

loadRange ::
  (MonadIRBuilder m, MonadModuleBuilder m) =>
  Operand ->
  [Int] ->
  m [Operand]
loadRange = mapM . loadOffset

storeRange ::
  (MonadIRBuilder m, MonadModuleBuilder m) =>
  Operand ->
  Int ->
  [Operand] ->
  m ()
storeRange ds from ops = forM_ as (uncurry (storeOffset ds))
  where
    as = [from ..] `zip` ops

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

-- ppllModule ?
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
