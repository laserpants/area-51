{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module Pong.LLVM.Emit where

import Control.Monad.Reader
import Control.Monad.State
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
import qualified Pong.Data as Pong
import Pong.LLVM hiding (Typed, name, typeOf, var, void)
import Pong.Lang
import Pong.Util
  ( Fix (..)
  , Name
  , cata
  , embed
  , getAndModify
  , para
  , project
  , (***)
  , (<$$>)
  , (<&>)
  , (<<<)
  , (>>>)
  )
import Pong.Util.Env (Environment (..))
import qualified Pong.Util.Env as Env
import TextShow (showt)

type OpInfo = (MonoType, Operand)

type CodeGenEnv = Environment OpInfo

type CodeGen a =
  IRBuilderT
    ( StateT
        (Int, ModuleDefs MonoType Ast)
        (ReaderT CodeGenEnv ModuleBuilder)
    )
    a

runCodeGen ::
  CodeGenEnv ->
  StateT (Int, ModuleDefs MonoType Ast) (ReaderT CodeGenEnv ModuleBuilder) a ->
  ModuleBuilder a
runCodeGen env a = runReaderT (evalStateT a (1, mempty)) env

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
  ModuleDefs MonoType t ->
  (Name -> MonoType -> Definition MonoType t -> m a) ->
  m [a]
forEachIn p f = forEachDef p (\(_, n) def -> f n (typeOf def) def)

buildModule_ :: Pong.Module MonoType Ast -> LLVM.Module
buildModule_ (Pong.Module modname p) =
  buildModule (llvmRep modname) $ do
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
                (llvmType True <$> args)
                (llvmType True t1)
            pure [(defn, (t, op))]
          Constant (t1, _) ->
            pure
              [
                ( defn
                ,
                  ( t
                  , functionRef
                      (llvmRep defn)
                      (llvmType True t1)
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
                      (llvmType True t1)
                      (llvmType True . fst <$> toList args)
                  )
                )
              ]
          _ ->
            pure []
    runCodeGen env $
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
                        ( \_ -> do
                            let sty = StructureType False [i8]
                            s <- malloc sty
                            storeOffset s 0 (int8 n)
                            a <- bitcast s charPtr
                            ret a
                        )
                  _ ->
                    error "Implementation error"
          Function args _
            | isUpper (Text.head defn) ->
                case lookup (normalizedCon defn) consIndices of
                  Just n ->
                    void $
                      function
                        (llvmRep defn)
                        (toList args <&> llvmType True *** llvmRep)
                        charPtr
                        ( \ops -> do
                            let sty =
                                  StructureType
                                    False
                                    (i8 : (llvmType False . fst <$> toList args))
                            s <- malloc sty
                            storeRange s 0 (int8 n : ops)
                            a <- bitcast s charPtr
                            ret a
                        )
                  _ ->
                    error "Implementation error"
          Constant (t1, body) ->
            void $
              function
                (llvmRep defn)
                []
                (llvmType True t1)
                ( \_ ->
                    emitBody body >>= ret . snd
                )
          Function args (t1, body) ->
            void $
              function
                (llvmRep defn)
                (toList args <&> llvmType True *** llvmRep)
                (llvmType True t1)
                ( \ops ->
                    do
                      r <-
                        local
                          ( Env.inserts
                              [ (n, (ty, op))
                              | (op, (ty, n)) <- zip ops (toList args)
                              ]
                          )
                          (emitBody body)
                      ret (snd r)
                )
          _ ->
            pure ()
  where
    buildEnv = Env.fromList . concat <$$> forEachIn p
    consIndices =
      p
        & Map.toList
        & concatMap
          ( \case
              (_, Data _ cs) ->
                sort (consName <$> cs) `zip` [0 :: Integer ..]
              _ ->
                []
          )
    normalizedCon con =
      case Text.splitOn "-" con of
        [c, _] -> c
        _ -> con
    consName (Fix (TCon con _)) = con
    consName _ = error "Implementation error"

emitBody :: Ast -> CodeGen OpInfo
emitBody =
  para
    ( \case
        ELet (_, var) (e1@(Fix (ECall _ (_, fun) as)), _) (_, expr2) -> mdo
          n <- uniqueName "$g"
          let insertVar = Env.insert var (typeOf e1, f)
          f <-
            local insertVar $
              lift
                ( function
                    (llvmRep n)
                    ((llvmType True <$> argTypes e1) `zip` repeat "a")
                    (llvmType True (returnType e1))
                    ( \args -> do
                        as1 <- traverse emitBody as
                        let as2 = zip (argTypes e1) args
                        (_, r) <- emitCall fun (pure <$> (as1 <> as2))
                        ret r
                    )
                )
          local insertVar expr2
        e ->
          embed (fst <$> e)
            & cata
              ( \case
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
                  EVar (t, var)
                    | arity t > 0 ->
                        emitCall var []
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
                  ENil ->
                    emitNil
                  EExt field e1 e2 ->
                    emitExt field e1 e2
                  ERes fs e1 e2 ->
                    emitRes fs e1 e2
                  ECon{} ->
                    error "Implementation error"
              )
    )

emitNil :: CodeGen OpInfo
emitNil = do
  hmap <- call (functionRef "hashmap_init" charPtr []) []
  pure (tRec rNil, hmap)

emitExt :: Name -> CodeGen OpInfo -> CodeGen OpInfo -> CodeGen OpInfo
emitExt field e1 e2 = do
  (t1, e) <- e1
  (t2, m) <- e2
  str <- uniqueName "$str"
  f <- globalStringPtr (Text.unpack field) (llvmRep str)
  v <- concealOp e
  _ <- call (functionRef "hashmap_insert" LLVM.void []) (zip [m, ConstantOperand f, v] (repeat []))
  let Fix (TRec t) = t2
  pure (tRec (rExt field t1 t), m)

emitRes :: [Label MonoType] -> CodeGen OpInfo -> CodeGen OpInfo -> CodeGen OpInfo
emitRes [(_, field), (t0, v), (t1, r)] a1 a2 = do
  str <- uniqueName "$str"
  f <- globalStringPtr (Text.unpack field) (llvmRep str)
  (_, hmap) <- a1
  p <- call (functionRef "hashmap_lookup" charPtr []) (zip [hmap, ConstantOperand f] (repeat []))
  op <- revealOp p t0
  local (Env.inserts [(v, (t0, op)), (r, (t1, hmap))]) a2
emitRes _ _ _ = error "Implementation error"

emitCall :: Name -> [CodeGen OpInfo] -> CodeGen OpInfo
emitCall name_ args =
  Env.askLookup name_
    >>= \case
      Nothing ->
        error "Implementation error"
      Just fun -> do
        as <- sequence args
        case fun of
          (t, op@LocalReference{}) -> do
            -- Partially applied function
            let u = returnType t
            if arity t == length as
              then do
                let sty = StructureType False [llvmType False (tVar 0 ~> t)]
                s <- bitcast op (ptr sty)
                f <- loadOffset s 0
                r <- call f (zip (op : (snd <$> as)) (repeat []))
                pure (u, r)
              else do
                n <- uniqueName "$f"
                let (ts, us) = splitAt (length as) (argTypes t)
                    toFty tys = llvmType False (foldType u (tVar 0 : tys))
                    sty1 = StructureType False (toFty us : charPtr : (llvmType False <$> ts))
                    sty2 = StructureType False [toFty (ts <> us)]
                s <- malloc sty1
                storeOffset s 0
                  =<< function
                    (llvmRep n)
                    ((charPtr, "s") : [(llvmType True uj, llvmRep "a") | uj <- us])
                    (llvmType True u)
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
                r <- bitcast s charPtr
                pure (foldType u us, r)
          (t, op) -> do
            -- Regular function pointer
            let u = returnType t
            if arity t == length as
              then do
                r <- call op (zip (snd <$> as) (repeat []))
                pure (u, r)
              else do
                n <- uniqueName "$f"
                let (ts, us) = splitAt (length as) (argTypes t)
                    fty = llvmType False (foldType u (tVar 0 : us))
                    sty = StructureType False (fty : (llvmType False <$> ts))
                s <- malloc sty
                storeOffset s 0
                  =<< function
                    (llvmRep n)
                    ((charPtr, "s") : [(llvmType True uj, llvmRep "a") | uj <- us])
                    (llvmType True u)
                    ( \(w : ws) -> do
                        s1 <- bitcast w (ptr sty)
                        vs <- loadRange s1 [1 .. length args]
                        r <- call op (zip (vs <> ws) (repeat []))
                        ret r
                    )
                storeRange s 1 (snd <$> as)
                r <- bitcast s charPtr
                pure (foldType u us, r)

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
            s1 <- bitcast e (ptr (StructureType False (i8 : (argTypes t <&> llvmType False))))
            ops <- zip (argTypes t) <$> loadRange s1 [1 .. arity t]
            local (Env.inserts (zip (fields <&> snd) ops)) body
      br end
      cb <- currentBlock
      pure ((r, cb), blk)
  end <- block `named` "end"
  op <- phi (first snd . fst <$> blocks)
  pure (fst (fst (fst (head blocks))), op)

{- ORMOLU_DISABLE -}

llvmPrimType :: MonoType -> LLVM.Type
llvmPrimType =
 project
   >>> \case
     TUnit     -> i1
     TBool{}   -> i1
     TInt{}    -> i64
     TFloat{}  -> LLVM.float
     TDouble{} -> LLVM.double
     TVar{}    -> charPtr
     TString{} -> charPtr
     TChar{}   -> i8
     _         -> charPtr

llvmType :: Bool -> MonoType -> LLVM.Type
llvmType voidPtrs =
  \case
    t
      | isFunTy && voidPtrs -> charPtr
      | isFunTy   -> ptr (funTy t)
      | otherwise -> llvmPrimType t
      where
        isFunTy = hasHeadT ArrT t

{- ORMOLU_ENABLE -}

funTy :: MonoType -> LLVM.Type
funTy t =
  FunctionType
    { argumentTypes = init types
    , resultType = last types
    , isVarArg = False
    }
  where
    types = llvmType False <$> unwindType t

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
    t | t == (tInt ~> tInt) -> sub (ConstantOperand (Int 64 0)) op
    t | t == (tFloat ~> tFloat) -> sub (ConstantOperand (Float (Single 0))) op
    t | t == (tDouble ~> tDouble) -> sub (ConstantOperand (Float (Double 0))) op
    _ -> error "Not implemented"

{- ORMOLU_DISABLE -}

emitOp2Instr :: (MonoType, Op2) -> Operand -> Operand -> CodeGen Operand
emitOp2Instr =
 \case
   (t, OEq)  | t == (tInt ~> tInt ~> tBool) -> icmp Int.EQ
   (t, ONEq) | t == (tInt ~> tInt ~> tBool) -> icmp Int.NE
   (t, OEq)  | t == (tBool ~> tBool ~> tBool) -> icmp Int.EQ
   (t, ONEq) | t == (tBool ~> tBool ~> tBool) -> icmp Int.NE
   (t, OEq)  | t == (tFloat ~> tFloat ~> tBool) -> fcmp Float.UEQ
   (t, ONEq) | t == (tFloat ~> tFloat ~> tBool) -> fcmp Float.UNE
   (t, OEq)  | t == (tDouble ~> tDouble ~> tBool) -> fcmp Float.UEQ
   (t, ONEq) | t == (tDouble ~> tDouble ~> tBool) -> fcmp Float.UNE
   (t, OAdd) | t == (tInt ~> tInt ~> tInt) -> add
   (t, OSub) | t == (tInt ~> tInt ~> tInt) -> sub
   (t, OMul) | t == (tInt ~> tInt ~> tInt) -> mul
   (t, OAdd) | t == (tFloat ~> tFloat ~> tFloat) -> fadd
   (t, OSub) | t == (tFloat ~> tFloat ~> tFloat) -> fsub
   (t, OMul) | t == (tFloat ~> tFloat ~> tFloat) -> fmul
   (t, ODiv) | t == (tFloat ~> tFloat ~> tFloat) -> fdiv
   (t, OAdd) | t == (tDouble ~> tDouble ~> tDouble) -> fadd
   (t, OSub) | t == (tDouble ~> tDouble ~> tDouble) -> fsub
   (t, OMul) | t == (tDouble ~> tDouble ~> tDouble) -> fmul
   (t, ODiv) | t == (tDouble ~> tDouble ~> tDouble) -> fdiv
   (t, OGt)  | t == (tInt ~> tInt ~> tBool) -> icmp Int.UGT
   (t, OGtE) | t == (tInt ~> tInt ~> tBool) -> icmp Int.UGE
   (t, OLt)  | t == (tInt ~> tInt ~> tBool) -> icmp Int.ULT
   (t, OLtE) | t == (tInt ~> tInt ~> tBool) -> icmp Int.ULE
   (t, OGt)  | t == (tFloat ~> tFloat ~> tBool) -> fcmp Float.UGT
   (t, OGtE) | t == (tFloat ~> tFloat ~> tBool) -> fcmp Float.UGE
   (t, OLt)  | t == (tFloat ~> tFloat ~> tBool) -> fcmp Float.ULT
   (t, OLtE) | t == (tFloat ~> tFloat ~> tBool) -> fcmp Float.ULE
   (t, OGt)  | t == (tDouble ~> tDouble ~> tBool) -> fcmp Float.UGT
   (t, OGtE) | t == (tDouble ~> tDouble ~> tBool) -> fcmp Float.UGE
   (t, OLt)  | t == (tDouble ~> tDouble ~> tBool) -> fcmp Float.ULT
   (t, OLtE) | t == (tDouble ~> tDouble ~> tBool) -> fcmp Float.ULE
   o -> error ("Not implemented: " <> show o)

{- ORMOLU_ENABLE -}

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
  (n, _) <- getAndModify (first succ)
  pure (prefix <> showt n)

printModule :: LLVM.Module -> IO ()
printModule = Text.putStrLn . ppll
