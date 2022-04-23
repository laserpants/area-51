{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Pong.LLVM.Emit where

--import Control.Monad.Writer
--import Data.Char (isUpper)
--import Data.Foldable (foldlM, foldrM)
--import Data.Tuple.Extra (dupe, first, second)
--import LLVM.AST.Attribute (ParameterAttribute)
--import TextShow (TextShow, showt)
--import qualified Data.Map.Strict as Map
--import qualified Data.Text as Text
--import qualified Pong.Util.Env as Env
import Control.Monad.Reader
import Data.List (sortOn)
import Data.List.NonEmpty (toList)
import Data.String (IsString, fromString)
import Data.Tuple.Extra (uncurry3)
import Debug.Trace
import LLVM.Pretty
import Pong.Data
import Pong.LLVM hiding (Typed, typeOf, void)
import Pong.Lang
import Pong.Util
import Pong.Util.Env (Environment (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy.IO as Text
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Typed as LLVM
import qualified Pong.Util.Env as Env

type CodeGenEnv = Environment (MonoType, Operand)

newtype CodeGen a =
  CodeGen
    { getCodeGen :: ReaderT CodeGenEnv (IRBuilderT ModuleBuilder) a
    }

llvmRep :: (IsString s) => Name -> s
llvmRep = fromString <<< unpack

charPtr :: LLVM.Type
charPtr = ptr i8

namedReference :: Name -> LLVM.Type
namedReference = NamedTypeReference . llvmRep

-- | Translate a language type to the equivalent LLVM type
llvmType :: MonoType -> LLVM.Type
llvmType =
  project >>> 
    \case
      TUnit      -> StructureType False []
      TBool   {} -> LLVM.i1
      TInt    {} -> LLVM.i64
      TFloat  {} -> LLVM.float
      TDouble {} -> LLVM.double
      TVar    {} -> charPtr
      TChar   {} -> error "Not implemented" -- TODO
      TString {} -> error "Not implemented" -- TODO
      ty@TArr {} -> ptr (StructureType False [ptr funTy])
        where
          types = llvmType <$> unwindType (embed ty)
          funTy =
            FunctionType
              { argumentTypes = charPtr : init types
              , resultType = last types
              , isVarArg = False
              }

----llvmFunType :: Operand -> LLVM.Type
----llvmFunType =
----  \case
----    ConstantOperand (GlobalReference PointerType {..} _) -> pointerReferent
----    LocalReference functionType _ -> functionType
----
----llvmArgTypes :: Operand -> [LLVM.Type]
----llvmArgTypes = argumentTypes <<< llvmFunType
----
----llvmRetType :: Operand -> LLVM.Type
----llvmRetType = resultType <<< llvmFunType
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
    o -> error (show o)

globalRef :: LLVM.Name -> LLVM.Type -> Operand
globalRef name ty = ConstantOperand (GlobalReference ty name)

functionRef :: LLVM.Name -> LLVM.Type -> [LLVM.Type] -> Operand
functionRef name rty argtys = globalRef name (ptr (FunctionType rty argtys False))

----buildDataType :: (MonadModuleBuilder m) => Name -> [Constructor] -> m ()
----buildDataType tyName cstrs = do
----  tp <- typedef (llvmRep tyName) (Just (StructureType False [i8]))
----  forM_ (sortOn consName cstrs `zip` [1 ..]) $ \(Constructor tcon fields, i) -> do
----    let ts = llvmType <$> fields
----        con = llvmRep (tyName <> "__" <> tcon)
----    td <-
----      typedef con
----        (Just (StructureType False (i8 : ts)))
----    function con
----      (zip ts (repeat "a"))
----      (ptr tp)
----      (\args -> do 
----        op <- malloc td
----        storeAtOffs 0 op (int8 i) -- constructor #
----        forM_ (args `zip` [1 ..]) $ \(arg, j) -> 
----          storeAtOffs j op arg
----        p <- bitcast op (ptr tp)
----        ret p)
----
--
--forEachDef :: (Monad m) => Map Name (a, b) -> (Name -> a -> b -> m c) -> m [c]
--forEachDef map f = forM (Map.toList map) $ uncurry (uncurry . f)
--
--buildProgram :: Name -> Program -> LLVM.Module
--buildProgram prog Program {..} =
--  buildModule (llvmRep prog) $ do
--    let ds = definitions <#> annotate
--    env <-
--      Env.fromList . concat <$$> forEachDef ds $ \name t -> do
--        \case
--          External Signature {..} -> do
--            op <-
--              extern
--                (llvmRep name)
--                (llvmType <$> arguments)
--                (llvmType body)
--            pure [(name, (t, op))]
--          Constant lit -> do
--            let constant = llvmPrim lit
--            op <- global (llvmRep name) (LLVM.typeOf constant) constant
--            pure [(name, (t, op))]
--          Data tyName cstrs -> do
--            tp <- typedef (llvmRep tyName) (Just (StructureType False [i16]))
--            concat <$$> forM (sortOn consName cstrs `zip` [1 ..]) $ \(Constructor tcon fields, i) -> do
--              let ts = llvmType <$> fields
--                  con = llvmRep (tyName <> "__" <> tcon)
--              td <- typedef con (Just (StructureType False (i16 : ts)))
--              f <-
--                function
--                  con
--                  (zip ts (repeat "a"))
--                  (ptr tp)
--                  (\args -> do
--                     op <- malloc td
--                     storeAtOffs 0 op (ConstantOperand (Int 16 i)) -- constructor #
--                     forM_ (args `zip` [1 ..]) $ \(arg, j) ->
--                       storeAtOffs j op arg
--                     p <- bitcast op (ptr tp)
--                     ret p)
--              pure [(tcon, (foldType (tData tyName) fields, f))]
--          Function Signature {..} ->
--            pure
--              [ ( name
--                , ( t
--                  , functionRef
--                      (llvmRep name)
--                      (llvmType (fst body))
--                      (llvmType . fst <$> arguments)))
--              ]
--    forEachDef ds $ \name t ->
--      \case
--        Function Signature {..} -> do
--          void $
--            function
--              (llvmRep name)
--              (arguments <#> llvmType *** llvmRep)
--              (llvmType (fst body))
--              (\ops -> do
--                 let args = [(n, (t, op)) | (op, (t, n)) <- zip ops arguments]
--                 runReaderT
--                   (getCodeGen $ emitBody (snd body) >>= ret)
--                   (Env.inserts args env))
--        _ -> pure ()

emitBody :: Ast -> CodeGen Operand
emitBody =
  cata $ \case
    ELit lit -> emitPrim lit
    EIf expr1 expr2 expr3 -> mdo 
      ifOp <- expr1
      condBr ifOp thenBlock elseBlock
      thenBlock <- block `named` "then"
      thenOp <- expr2
      br mergeBlock
      elseBlock <- block `named` "else"
      elseOp <- expr3
      br mergeBlock
      mergeBlock <- block `named` "ifcont"
      phi [(thenOp, thenBlock), (elseOp, elseBlock)]
    EOp1 op expr1 -> do
      a <- expr1
      emitOp1Instr op a 
    EOp2 op expr1 expr2 -> do
      a <- expr1
      b <- expr2
      emitOp2Instr op a b
    EVar (t, name) ->
      Env.askLookup name >>= \case
        Just (_, op) | isConT ArrT t ->
          emitCall (t, name) []
        Just (_, op) ->
          pure op
        _ ->
          error ("Not in scope: '" <> show name <> "'")
    ECase expr clauses ->
      emitCase expr (sortOn fst clauses)
    ECall () fun args ->
      emitCall fun args
    ELet (t, name) expr1 expr2 -> do
      e1 <- expr1
      local (Env.insert name (t, e1)) expr2

emitCase 
  :: CodeGen Operand 
  -> [([Label MonoType], CodeGen Operand)] 
  -> CodeGen Operand
emitCase expr cs = do
  e <- expr
  traceShowM ">>>>>>>>>>"
  traceShowM e
  traceShowM "<<<<<<<<<<"
  undefined
--emitCase expr cs = mdo
--  e <- expr
--  m <- loadFromOffset 0 e
--  let names = blocks <#> snd
--  switch
--    m
--    (last names)  -- Default case
--    [ (Int 16 (fromIntegral i), block)
--    | (i, block) <- zip [1 ..] (init names)
--    ]
--  blocks <-
--    forM cs $ \((t, con):fields, body) -> do
--      blk <- block `named` "case"
--      r <-
--        if null fields
--          then body
--          else do
--            Env env <- ask
--            let t0 = fst (env ! con)
--            struct <- bitcast e (ptr (StructureType False (i16 : (argTypes t0 <#> llvmType))))
--            ops <-
--              forM (zip3 [1..] (argTypes t0) (argTypes t)) $ \(i, t0, t) -> do
--                op <- loadFromOffset i struct
--                op' <- foo2 (op, t0, t)
--                pure (t, op')
--            local (Env.inserts (zip (fields <#> snd) ops)) body
--      br end
--      cb <- currentBlock
--      pure ((r, cb), blk)
--  end <- block `named` "end"
--  phi (blocks <#> fst)

xcall fun t0 t1 as = do
  as1 <- traverse (uncurry3 cast) (zip3 as ts1 ts2)
  r <- call fun (zip as1 (repeat []))
  case (project rt1, project rt2) of
    _ | rt1 == rt2 -> pure r
    (TInt, TVar {}) ->
      ptrtoint r (llvmType tInt)
  where
    ts1, ts2 :: [MonoType]
    ts1 = unwindType t0
    ts2 = unwindType t1
    rt1 = last ts1
    rt2 = last ts2
    cast op t1 t2 =
      case (project t1, project t2) of
        _ | t1 == t2 -> pure op
        (TInt, TVar {}) ->
          inttoptr op charPtr

emitCall :: Label MonoType -> [CodeGen Operand] -> CodeGen Operand

emitCall (t, "Nil") args = do
  as <- sequence args
  error "boooo"

emitCall (t, "Cons") args = do
  as <- sequence args
  error "zoooo"

emitCall (t, fun) args = do
  as <- sequence args
  Env.askLookup fun >>= \case
    Just (t0, op@(LocalReference PointerType {..} _)) -> do
--      let sty = StructureType False [i8, llvmType t0]
--      p <- bitcast op (ptr sty)
      p0 <- gep op [int32 0, int32 0]
      p1 <- gep op [int32 0, int32 1]
      n <- load p0 0
      fun <- load p1 0
----      as1 <- forM_ [2..2] $ \i -> do -- TODO
----        q <- gep p [int32 0, int32 i]
----        load q 0
      --fun <- bitcast xp (llvmType t0)
      --emitPrim (PInt 32)
      traceShowM "======="
      traceShowM t
      traceShowM t0
      xcall fun t t0 as
      --call fun (zip as (repeat []))

    Just (t0, op) -> do
      case compare (arity t) (length args) of
        EQ -> do
          -- call op (zip as (repeat []))
          xcall op t t0 as
        GT -> do
          let sty = StructureType False [i8, LLVM.typeOf op] --  : (LLVM.typeOf <$> as))
          struct <- malloc sty
          p0 <- gep struct [int32 0, int32 0]
          p1 <- gep struct [int32 0, int32 1]
          store p0 0 (int8 0) -- (fromIntegral (length as)))
          store p1 0 op
          --forM_ (as `zip` [2 ..]) $ \(arg, i) -> do
          --  q <- gep struct [int32 0, int32 i]
          --  store q 0 arg
          --traceShowM "************************"
          --traceShowM struct
          --traceShowM "************************"
          pure struct
          -- error "FOO"
    x ->
      error (show x)
--    Just (t0, op@(LocalReference PointerType {..} _)) -> do
--      a <- bitcast op charPtr
--      undefined
--      h <- loadFromOffset 0 op
--      x <- inttoptr (ConstantOperand (Int 32 9991)) charPtr -- TODO: TEMP
--      -- TEMP
--      --zz <- inttoptr (ConstantOperand (Int 32 123)) charPtr
--      call h (zip (a : as) (repeat []))
--    Just (t0, op) -> do
--      as' <- traverse foo1 (zip3 as (unwindType t0) (unwindType t))
--      let tys = argTypes t
--          (ts1, ts2) = splitAt (length args) (llvmType <$> tys)
--      case compare (arity t) (length args) of
--        EQ -> do
--          r <- call op (zip as' (repeat []))
--          foo2 (r, returnTypeOf t0, returnTypeOf t)
--          -- TODO return?
--        GT -> mdo 
--          let sty = StructureType False (LLVM.typeOf g : LLVM.typeOf op : ts1)
--          name <- freshName "fun"
--          g <- 
--            function
--              name
--              (zip (charPtr : ts2) (repeat "a"))
--              (llvmType (returnTypeOf t0))
--              (\(v:vs) -> do 
--                s <- bitcast v (ptr sty)
--                h <- loadFromOffset 1 s
--                an <-
--                  forM [2 .. length ts1 + 1] $ \n -> 
--                    loadFromOffset (fromIntegral n) s
--                r <- call h (zip (an <> vs) (repeat []))
--                ret r)
--          struct <- malloc sty
--          storeAtOffs 0 struct g
--          storeAtOffs 1 struct op
--          forM_ (as `zip` [2 ..]) $ \(a, i) -> 
--            storeAtOffs i struct a
--          bitcast struct (ptr (StructureType False [LLVM.typeOf g]))
--        LT -> error "Implementation error"
--    _ -> error ("Function not in scope: '" <> show fun <> "'")
--
--foo1 :: (Operand, Type, Type) -> CodeGen Operand
--foo1 = \case
--  (op, s, t) | isTCon VarT s && t `elem` [tInt32, tInt64] -> 
--    inttoptr op charPtr
----  (op@(ConstantOperand Int {}), t) | tOpaque == t -> do
----    inttoptr op charPtr 
--  (op, _, _) -> pure op
--
--foo2 :: (Operand, Type, Type) -> CodeGen Operand
--foo2 = \case
--  (op, s, t) | isTCon VarT s && t `elem` [tInt32, tInt64] -> 
--    ptrtoint op (llvmType t)
----  (op@(ConstantOperand Int {}), t) | tOpaque == t -> do
----    inttoptr op charPtr 
--  (op, _, _) -> pure op
--
----foo2 :: (Operand, Type) -> CodeGen Operand
----foo2 = \case
----  (op, t) | charPtr == LLVM.typeOf op && t `elem` [tInt32, tInt64] -> 
----    ptrtoint op (llvmType t)
----  (op, _) -> pure op
--
--
----forEachDef ::
----     (Monad m)
----  => Map Name (Definition Ast)
----  -> (Name -> Type -> Definition Ast -> m b)
----  -> m [b]
----forEachDef map f = forM (Map.toList map) g
----  where
----    g (name, def) = f name (Pong.Lang.typeOf def) def
----
----buildProgram :: Name -> Program -> LLVM.Module
----buildProgram mod Program {..} =
----  buildModule (llvmRep mod) $ do
----    env <-
----      Env.fromList . concat <$$> forEachDef definitions $ \name ->
----        \case 
----          Data _ css -> do
----            t1 <- typedef (llvmRep name) (Just (StructureType False [i8]))
----            concat <$$> forM (sortOn consName css `zip` [1 ..]) $ \(Constructor tcon fields, i) -> do
----              let ts = llvmType <$> fields
----                  con = llvmRep (name <> "__" <> tcon)
----              td <-
----                typedef con
----                  (Just (StructureType False (i8 : ts)))
----              f <-
----                function con
----                  (zip ts (repeat "a"))
----                  (ptr t1)
----                  (\args -> do 
----                    op <- malloc td
----                    storeAtOffs 0 op (int8 i) -- constructor #
----                    forM_ (args `zip` [1 ..]) $ \(arg, j) -> 
----                      storeAtOffs j op arg
----                    bitcast op (ptr t1) >>= ret)
----              pure [(tcon, f)]
----          Function Signature {..} -> do
----            pure 
----              [ (name
----              , functionRef
----                  (llvmRep name)
----                  (llvmType (fst body))
----                  (llvmType . fst <$> arguments)) ]
----          External sig -> do
----            op <- emitExtern name sig
----            pure [(name, op)]
----          Constant _ -> pure []
----    void <$$> forEachDef definitions $ \name ->
----      \case 
----        --Function Signature {..} ->
----        --  void $
----        --  function
----        --    (llvmRep name)
----        --    (arguments <#> llvmType *** llvmRep)
----        --    (llvmType (fst body))
----        --    $ \ops -> do
----        --          let args = zip (arguments <#> snd) ops
----        --          flip runReaderT (Env.inserts args env) $ getCodeGen $ 
----        --            emitBody (snd body) >>=
----        --            ret
----        _ ->
----          pure ()
----
----emitBody :: Ast -> CodeGen Operand
----emitBody =
----  cata $ \case
----    ELit prim -> 
----      emitPrim prim
----    EIf expr1 expr2 expr3 ->
----      mdo ifOp <- expr1
----          condBr ifOp thenBlock elseBlock
----          thenBlock <- block `named` "then"
----          thenOp <- expr2
----          br mergeBlock
----          elseBlock <- block `named` "else"
----          elseOp <- expr3
----          br mergeBlock
----          mergeBlock <- block `named` "ifcont"
----          phi [(thenOp, thenBlock), (elseOp, elseBlock)]
----    EOp2 op expr1 expr2 -> do
----      a <- expr1
----      b <- expr2
----      emitOp2Instr op a b
----    EVar (t, name) -> 
----      Env.askLookup name >>= \case
----        Just op | isTCon ArrT t -> do
----          emitCall (t, name) []
----        Just op -> pure op
----        _ -> 
----          error ("Not in scope: '" <> show name <> "'")
----    ECase expr cs -> emitCase expr (sortOn fst cs)
----    ECall () fun args -> emitCall fun args
----
----emitCall :: Label Type -> [CodeGen Operand] -> CodeGen Operand
----emitCall (t, fun) args = do
----  as <- sequence args
----  Env.askLookup fun >>= \case
----    Just op@(LocalReference PointerType {..} _) -> do
----      a <- bitcast op charPtr
----      h <- loadFromOffset 0 op
----      -- TEMP
----      --zz <- inttoptr (ConstantOperand (Int 32 123)) charPtr
----      call h (zip (a : as) (repeat []))
----      --pure zz
----    Just op -> do
----      let tys = argTypes t
----          (ts1, ts2) = splitAt (length args) (llvmType <$> tys)
----      as111 <- traverse downCast (zip3 as (llvmArgTypes op) tys)
----      case compare (arity t) (length args) of
----        EQ -> do
----          r <- call op (zip as111 (repeat []))
----          traceShowM "******************************"
----          traceShowM "******************************"
----          traceShowM r
----          traceShowM op
----          traceShowM (returnTypeOf t)
----          upCast (r, llvmRetType op, returnTypeOf t)
----          --zz <- inttoptr (ConstantOperand (Int 32 999)) charPtr
----          --pure zz
----          -- TODO
----        GT -> mdo 
----         let sty = StructureType False (LLVM.typeOf g : LLVM.typeOf op : ts1)
----         name <- freshName "fun"
----         g <- 
----           function
----             name
----             (zip (charPtr : ts2) (repeat "a"))
----             (llvmRetType op)
----             (\(v:vs) -> do
----               s <- bitcast v (ptr sty)
----               h <- loadFromOffset 1 s
----               an <-
----                 forM [2 .. length ts1 + 1] $ \n -> 
----                   loadFromOffset (fromIntegral n) s
----               r <- call h (zip (an <> vs) (repeat []))
----               -- TEMP
----               --zz <- inttoptr (ConstantOperand (Int 32 456)) charPtr
----               --q <- upCast (r, llvmRetType op, returnTypeOf t)
----               --upCast undefined -- (r, llvmRetType op, returnTypeOf t)
----               ret r
----               )
----               -- ret r)
----         struct <- malloc sty
----         storeAtOffs 0 struct g
----         storeAtOffs 1 struct op
----         forM_ (as `zip` [2 ..]) $ \(a, i) -> 
----           storeAtOffs i struct a
----         bitcast struct (ptr (StructureType False [LLVM.typeOf g]))
----        LT -> error "Implementation error"
----    _ -> error ("Function not in scope: '" <> show fun <> "'")
----
----downCast :: (MonadIRBuilder m) => (Operand, LLVM.Type, Type) -> m Operand
----downCast = \case
----  (op, llvmt, t) | charPtr == llvmt && (t `elem` [tInt32, tInt64]) -> do
----    inttoptr op charPtr 
----  (op, _, _) -> pure op
----
----upCast :: (MonadIRBuilder m) => (Operand, LLVM.Type, Type) -> m Operand
----upCast = \case
----  (op, llvmt, t) | charPtr == llvmt && (t `elem` [tInt32, tInt64]) -> do
----    ptrtoint op (llvmType t)
----  (op, _, _) -> pure op
----
----emitCase :: CodeGen Operand -> [([Label Type], CodeGen Operand)] -> CodeGen Operand
----emitCase expr cs = mdo 
----  dt <- expr
----  m <- loadFromOffset 0 dt
----  let names = blocks <#> snd
----  switch
----    m
----    (last names)  -- Default case
----    [ (Int 8 (fromIntegral i), block)
----    | (i, block) <- zip [1 ..] (init names)
----    ]
----  blocks <-
----    forM cs $ \(con:fields, body) -> do
----      blk <- block `named` "case"
----      r <-
----        if null fields
----          then body
----          else do
----            Env env <- ask
----            let argTys = argTypes (fst con)
----                llvmTys = llvmArgTypes (env ! snd con)
----            dn <- bitcast dt (ptr (StructureType False (i8 : llvmTys)))
----            ops <-
----              forM (zip3 [1..] llvmTys argTys) $ \(i, llvmt, t) -> do
----                op <- loadFromOffset i dn
----                upCast (op, llvmt, t)
----            local (Env.inserts (zip (fields <#> snd) ops)) body
----      br end
----      cb <- currentBlock
----      pure ((r, cb), blk)
----  end <- block `named` "end"
----  phi (blocks <#> fst)
----
----emitExtern :: Name -> Signature a -> ModuleBuilder Operand
----emitExtern name Signature{ .. } =
----  extern
----    (llvmRep name)
----    (llvmType . fst <$> arguments)
----    (llvmType (fst body))
--malloc :: (MonadIRBuilder m) => LLVM.Type -> m Operand
--malloc ty = do
--  size <- zext (ConstantOperand (sizeof ty)) i64
--  m <- allocate size
--  bitcast m (ptr ty)
--  where
--    allocate size = call (functionRef "gc_malloc" charPtr [i64]) [(size, [])]
--
--storeAtOffs ::
--     (MonadModuleBuilder m, MonadIRBuilder m)
--  => Integer
--  -> Operand
--  -> Operand
--  -> m ()
--storeAtOffs offs op val = do
--  p <- gep op [int32 0, int32 offs]
--  store p 0 val
--
--loadFromOffset ::
--     (MonadModuleBuilder m, MonadIRBuilder m) => Integer -> Operand -> m Operand
--loadFromOffset offs op = do
--  p <- gep op [int32 0, int32 offs]
--  load p 0

buildProgram :: Name -> Program MonoType Ast -> LLVM.Module
buildProgram name (Program defs) = do
  buildModule (llvmRep name) $ do
    env <-
      Env.fromList . concat <$$> forEachDef defs $ \name t ->
        \case
          Extern args t1 -> do
            op <-
              extern
                (llvmRep name)
                (llvmType <$> args)
                (llvmType t1)
            pure [ ( name, (t, op) ) ]
          Function args (t1, _) ->
            pure
              [ ( name
                , ( t
                  , functionRef
                      (llvmRep name)
                      (llvmType t1)
                      (llvmType . fst <$> toList args)
                  )
                )
              ]
    forEachDef defs $ \name t ->
      \case
        Function args (t1, body) ->
          void $
            function
              (llvmRep name)
              (toList args <#> llvmType *** llvmRep)
              (llvmType t1)
              (\ops -> do
                runReaderT
                  (getCodeGen $ emitBody body >>= ret)
                  (Env.inserts [(n, (t, op)) | (op, (t, n)) <- zip ops (toList args)] env))
        _ ->
          pure ()

forEachDef
  :: (Monad m)
  => Map Name (Definition MonoType a)
  -> (Name -> MonoType -> Definition MonoType a -> m e)
  -> m [e]
forEachDef map f =
  forM (Map.toList map) (\(name, def) -> f name (typeOf def) def)

malloc :: (MonadIRBuilder m) => LLVM.Type -> m Operand
malloc ty = do
  size <- zext (ConstantOperand (sizeof ty)) i64
  m <- allocate size
  bitcast m (ptr ty)
  where
    allocate size = call (functionRef "gc_malloc" charPtr [i64]) [(size, [])]

runModule :: LLVM.Module -> IO ()
runModule = Text.putStrLn . ppll 

-- CodeGen
deriving instance Functor CodeGen

deriving instance Applicative CodeGen

deriving instance Monad CodeGen

deriving instance (MonadReader CodeGenEnv) CodeGen

deriving instance MonadFix CodeGen

deriving instance MonadIRBuilder CodeGen

deriving instance MonadModuleBuilder CodeGen
