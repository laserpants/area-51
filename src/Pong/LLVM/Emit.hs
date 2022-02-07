{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}

module Pong.LLVM.Emit where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char (isUpper)
import Data.Foldable (foldlM, foldrM)
import Data.List (sortOn)
import Data.String (IsString, fromString)
import Data.Tuple.Extra (dupe, first, second)
import Debug.Trace
import LLVM.AST.Attribute (ParameterAttribute)
import Pong.LLVM hiding (Typed, typeOf, void)
import Pong.Lang
import TextShow (TextShow, showt)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Typed as LLVM
import qualified Pong.Util.Env as Env

llvmRep :: (IsString s) => Name -> s
llvmRep = fromString <<< unpack

charPtr :: LLVM.Type
charPtr = ptr i8

namedReference :: Name -> LLVM.Type
namedReference = NamedTypeReference . llvmRep

-- | Translate a language type to equivalent LLVM type
llvmType :: Type -> LLVM.Type
llvmType =
  project >>> \case
    TUnit -> StructureType False []
    TBool {} -> LLVM.i1
    TInt32 {} -> LLVM.i32
    TInt64 {} -> LLVM.i64
    TFloat {} -> LLVM.float
    TDouble {} -> LLVM.double
    TVar {} -> charPtr 
    TChar {} -> error "Not implemented" -- TODO
    TString {} -> error "Not implemented" -- TODO
    TData name -> ptr (namedReference name)
--    TOpaque -> charPtr
    ty@TArr {} -> ptr (StructureType False [ptr funTy])
      where types = llvmType <$> unwindType (embed ty)
            funTy =
              FunctionType
                { argumentTypes = charPtr : init types
                , resultType = last types
                , isVarArg = False
                }

--llvmFunType :: Operand -> LLVM.Type
--llvmFunType =
--  \case
--    ConstantOperand (GlobalReference PointerType {..} _) -> pointerReferent
--    LocalReference functionType _ -> functionType
--
--llvmArgTypes :: Operand -> [LLVM.Type]
--llvmArgTypes = argumentTypes <<< llvmFunType
--
--llvmRetType :: Operand -> LLVM.Type
--llvmRetType = resultType <<< llvmFunType
llvmLiteral :: Literal -> Constant
llvmLiteral =
  \case
    LBool True -> Int 1 1
    LBool False -> Int 1 0
    LInt32 n -> Int 32 (toInteger n)
    LInt64 n -> Int 64 (toInteger n)
    LFloat f -> Float (Single f)
    LDouble d -> Float (Double d)
    LChar c -> error "Not implemented" -- TODO
    LString s -> error "Not implemented" -- TODO
    LUnit -> Struct Nothing False []

emitLit :: Literal -> CodeGen Operand
emitLit = pure <<< ConstantOperand <<< llvmLiteral

emitOp2Instr :: Op2 -> Operand -> Operand -> CodeGen Operand
emitOp2Instr =
  \case
    OEqInt32 -> icmp LLVM.EQ
    OAddInt32 -> add
    OSubInt32 -> sub
    OMulInt32 -> mul
    OAddFloat -> fadd
    OMulFloat -> fmul
    OSubFloat -> fsub
    ODivFloat -> fdiv
    OAddDouble -> fadd
    OMulDouble -> fmul
    OSubDouble -> fsub
    ODivDouble -> fdiv

globalRef :: LLVM.Name -> LLVM.Type -> Operand
globalRef name ty = ConstantOperand (GlobalReference ty name)

functionRef :: LLVM.Name -> LLVM.Type -> [LLVM.Type] -> Operand
functionRef name rty argtys = globalRef name (ptr (FunctionType rty argtys False))

--buildDataType :: (MonadModuleBuilder m) => Name -> [Constructor] -> m ()
--buildDataType tyName cstrs = do
--  tp <- typedef (llvmRep tyName) (Just (StructureType False [i8]))
--  forM_ (sortOn consName cstrs `zip` [1 ..]) $ \(Constructor tcon fields, i) -> do
--    let ts = llvmType <$> fields
--        con = llvmRep (tyName <> "__" <> tcon)
--    td <-
--      typedef con
--        (Just (StructureType False (i8 : ts)))
--    function con
--      (zip ts (repeat "a"))
--      (ptr tp)
--      (\args -> do 
--        op <- malloc td
--        storeAtOffs 0 op (int8 i) -- constructor #
--        forM_ (args `zip` [1 ..]) $ \(arg, j) -> 
--          storeAtOffs j op arg
--        p <- bitcast op (ptr tp)
--        ret p)
--

forEachDef :: (Monad m) => Map Name (a, b) -> (Name -> a -> b -> m c) -> m [c]
forEachDef map f = forM (Map.toList map) $ uncurry (uncurry . f)

buildProgram :: Name -> Program -> LLVM.Module
buildProgram prog Program {..} =
  buildModule (llvmRep prog) $ do
    let ds = definitions <#> annotate
    env <-
      Env.fromList . concat <$$> forEachDef ds $ \name t -> do
        \case
          External Signature {..} -> do
            op <-
              extern
                (llvmRep name)
                (llvmType . fst <$> arguments)
                (llvmType (fst body))
            pure [(name, (t, op))]
          Constant lit -> do
            let constant = llvmLiteral lit
            op <- global (llvmRep name) (LLVM.typeOf constant) constant
            pure [(name, (t, op))]
          Data tyName cstrs -> do
            tp <- typedef (llvmRep tyName) (Just (StructureType False [i16]))
            concat <$$> forM (sortOn consName cstrs `zip` [1 ..]) $ \(Constructor tcon fields, i) -> do
              let ts = llvmType <$> fields
                  con = llvmRep (tyName <> "__" <> tcon)
              td <- typedef con (Just (StructureType False (i16 : ts)))
              f <-
                function
                  con
                  (zip ts (repeat "a"))
                  (ptr tp)
                  (\args -> do
                     op <- malloc td
                     storeAtOffs 0 op (ConstantOperand (Int 16 i)) -- constructor #
                     forM_ (args `zip` [1 ..]) $ \(arg, j) ->
                       storeAtOffs j op arg
                     p <- bitcast op (ptr tp)
                     ret p)
              pure [(tcon, (foldType (tData tyName) fields, f))]
          Function Signature {..} ->
            pure
              [ ( name
                , ( t
                  , functionRef
                      (llvmRep name)
                      (llvmType (fst body))
                      (llvmType . fst <$> arguments)))
              ]
    forEachDef ds $ \name t ->
      \case
        Function Signature {..} -> do
          void $
            function
              (llvmRep name)
              (arguments <#> llvmType *** llvmRep)
              (llvmType (fst body))
              (\ops -> do
                 let args = [(n, (t, op)) | (op, (t, n)) <- zip ops arguments]
                 runReaderT
                   (getCodeGen $ emitBody (snd body) >>= ret)
                   (Env.inserts args env))
        _ -> pure ()

emitBody :: Ast -> CodeGen Operand
emitBody =
  cata $ \case
    ELit lit -> emitLit lit
    EIf expr1 expr2 expr3 ->
      mdo ifOp <- expr1
          condBr ifOp thenBlock elseBlock
          thenBlock <- block `named` "then"
          thenOp <- expr2
          br mergeBlock
          elseBlock <- block `named` "else"
          elseOp <- expr3
          br mergeBlock
          mergeBlock <- block `named` "ifcont"
          phi [(thenOp, thenBlock), (elseOp, elseBlock)]
    EOp2 op expr1 expr2 -> do
      a <- expr1
      b <- expr2
      emitOp2Instr op a b
    EVar (t, name) ->
      Env.askLookup name >>= \case
        Just (t1, op) | isTCon ArrT t1 -> 
          undefined -- TODO
        Just (_, op) -> pure op
        _ -> error ("Not in scope: '" <> show name <> "'")
    ECase expr clss -> emitCase expr (sortOn fst clss)
    ECall () fun args -> emitCall fun args

--        Just (t1, op) | isTCon ArrT t -> do
--          emitCall (t, name) []
emitCase ::
     CodeGen Operand -> [([Label Type], CodeGen Operand)] -> CodeGen Operand
emitCase expr cs = mdo
  e <- expr
  m <- loadFromOffset 0 e
  let names = blocks <#> snd
  switch
    m
    (last names)  -- Default case
    [ (Int 16 (fromIntegral i), block)
    | (i, block) <- zip [1 ..] (init names)
    ]
  blocks <-
    forM cs $ \((t, con):fields, body) -> do
      blk <- block `named` "case"
      r <-
        if null fields
          then body
          else do
            Env env <- ask
            let t0 = fst (env ! con)
            struct <- bitcast e (ptr (StructureType False (i16 : (argTypes t0 <#> llvmType))))
            ops <-
              forM (zip3 [1..] (argTypes t0) (argTypes t)) $ \(i, t0, t) -> do
                op <- loadFromOffset i struct
                op' <- foo2 (op, t0, t)
                pure (t, op')
            local (Env.inserts (zip (fields <#> snd) ops)) body
      br end
      cb <- currentBlock
      pure ((r, cb), blk)
  end <- block `named` "end"
  phi (blocks <#> fst)

emitCall :: Label Type -> [CodeGen Operand] -> CodeGen Operand
emitCall (t, fun) args = do
  as <- sequence args
  Env.askLookup fun >>= \case
    Just (t0, op@(LocalReference PointerType {..} _)) -> do
      undefined
    Just (t0, op) ->
      case compare (arity t) (length args) of
        EQ -> do
          as' <- traverse foo1 (zip3 as (unwindType t0) (unwindType t))
          r <- call op (zip as' (repeat []))
          foo2 (r, returnTypeOf t0, returnTypeOf t)
          -- TODO return?
        GT -> do 
          undefined
        LT -> error "Implementation error"
    _ -> error ("Function not in scope: '" <> show fun <> "'")

foo1 :: (Operand, Type, Type) -> CodeGen Operand
foo1 = \case
  (op, s, t) | isTCon VarT s && t `elem` [tInt32, tInt64] -> 
    inttoptr op charPtr
--  (op@(ConstantOperand Int {}), t) | tOpaque == t -> do
--    inttoptr op charPtr 
  (op, _, _) -> pure op

foo2 :: (Operand, Type, Type) -> CodeGen Operand
foo2 = \case
  (op, s, t) | isTCon VarT s && t `elem` [tInt32, tInt64] -> 
    ptrtoint op (llvmType t)
--  (op@(ConstantOperand Int {}), t) | tOpaque == t -> do
--    inttoptr op charPtr 
  (op, _, _) -> pure op

--foo2 :: (Operand, Type) -> CodeGen Operand
--foo2 = \case
--  (op, t) | charPtr == LLVM.typeOf op && t `elem` [tInt32, tInt64] -> 
--    ptrtoint op (llvmType t)
--  (op, _) -> pure op


--forEachDef ::
--     (Monad m)
--  => Map Name (Definition Ast)
--  -> (Name -> Type -> Definition Ast -> m b)
--  -> m [b]
--forEachDef map f = forM (Map.toList map) g
--  where
--    g (name, def) = f name (Pong.Lang.typeOf def) def
--
--buildProgram :: Name -> Program -> LLVM.Module
--buildProgram mod Program {..} =
--  buildModule (llvmRep mod) $ do
--    env <-
--      Env.fromList . concat <$$> forEachDef definitions $ \name ->
--        \case 
--          Data _ css -> do
--            t1 <- typedef (llvmRep name) (Just (StructureType False [i8]))
--            concat <$$> forM (sortOn consName css `zip` [1 ..]) $ \(Constructor tcon fields, i) -> do
--              let ts = llvmType <$> fields
--                  con = llvmRep (name <> "__" <> tcon)
--              td <-
--                typedef con
--                  (Just (StructureType False (i8 : ts)))
--              f <-
--                function con
--                  (zip ts (repeat "a"))
--                  (ptr t1)
--                  (\args -> do 
--                    op <- malloc td
--                    storeAtOffs 0 op (int8 i) -- constructor #
--                    forM_ (args `zip` [1 ..]) $ \(arg, j) -> 
--                      storeAtOffs j op arg
--                    bitcast op (ptr t1) >>= ret)
--              pure [(tcon, f)]
--          Function Signature {..} -> do
--            pure 
--              [ (name
--              , functionRef
--                  (llvmRep name)
--                  (llvmType (fst body))
--                  (llvmType . fst <$> arguments)) ]
--          External sig -> do
--            op <- emitExtern name sig
--            pure [(name, op)]
--          Constant _ -> pure []
--    void <$$> forEachDef definitions $ \name ->
--      \case 
--        --Function Signature {..} ->
--        --  void $
--        --  function
--        --    (llvmRep name)
--        --    (arguments <#> llvmType *** llvmRep)
--        --    (llvmType (fst body))
--        --    $ \ops -> do
--        --          let args = zip (arguments <#> snd) ops
--        --          flip runReaderT (Env.inserts args env) $ getCodeGen $ 
--        --            emitBody (snd body) >>=
--        --            ret
--        _ ->
--          pure ()
--
--emitBody :: Ast -> CodeGen Operand
--emitBody =
--  cata $ \case
--    ELit lit -> 
--      emitLit lit
--    EIf expr1 expr2 expr3 ->
--      mdo ifOp <- expr1
--          condBr ifOp thenBlock elseBlock
--          thenBlock <- block `named` "then"
--          thenOp <- expr2
--          br mergeBlock
--          elseBlock <- block `named` "else"
--          elseOp <- expr3
--          br mergeBlock
--          mergeBlock <- block `named` "ifcont"
--          phi [(thenOp, thenBlock), (elseOp, elseBlock)]
--    EOp2 op expr1 expr2 -> do
--      a <- expr1
--      b <- expr2
--      emitOp2Instr op a b
--    EVar (t, name) -> 
--      Env.askLookup name >>= \case
--        Just op | isTCon ArrT t -> do
--          emitCall (t, name) []
--        Just op -> pure op
--        _ -> 
--          error ("Not in scope: '" <> show name <> "'")
--    ECase expr cs -> emitCase expr (sortOn fst cs)
--    ECall () fun args -> emitCall fun args
--
--emitCall :: Label Type -> [CodeGen Operand] -> CodeGen Operand
--emitCall (t, fun) args = do
--  as <- sequence args
--  Env.askLookup fun >>= \case
--    Just op@(LocalReference PointerType {..} _) -> do
--      a <- bitcast op charPtr
--      h <- loadFromOffset 0 op
--      -- TEMP
--      --zz <- inttoptr (ConstantOperand (Int 32 123)) charPtr
--      call h (zip (a : as) (repeat []))
--      --pure zz
--    Just op -> do
--      let tys = argTypes t
--          (ts1, ts2) = splitAt (length args) (llvmType <$> tys)
--      as111 <- traverse downCast (zip3 as (llvmArgTypes op) tys)
--      case compare (arity t) (length args) of
--        EQ -> do
--          r <- call op (zip as111 (repeat []))
--          traceShowM "******************************"
--          traceShowM "******************************"
--          traceShowM r
--          traceShowM op
--          traceShowM (returnTypeOf t)
--          upCast (r, llvmRetType op, returnTypeOf t)
--          --zz <- inttoptr (ConstantOperand (Int 32 999)) charPtr
--          --pure zz
--          -- TODO
--        GT -> mdo 
--         let sty = StructureType False (LLVM.typeOf g : LLVM.typeOf op : ts1)
--         name <- freshName "fun"
--         g <- 
--           function
--             name
--             (zip (charPtr : ts2) (repeat "a"))
--             (llvmRetType op)
--             (\(v:vs) -> do
--               s <- bitcast v (ptr sty)
--               h <- loadFromOffset 1 s
--               an <-
--                 forM [2 .. length ts1 + 1] $ \n -> 
--                   loadFromOffset (fromIntegral n) s
--               r <- call h (zip (an <> vs) (repeat []))
--               -- TEMP
--               --zz <- inttoptr (ConstantOperand (Int 32 456)) charPtr
--               --q <- upCast (r, llvmRetType op, returnTypeOf t)
--               --upCast undefined -- (r, llvmRetType op, returnTypeOf t)
--               ret r
--               )
--               -- ret r)
--         struct <- malloc sty
--         storeAtOffs 0 struct g
--         storeAtOffs 1 struct op
--         forM_ (as `zip` [2 ..]) $ \(a, i) -> 
--           storeAtOffs i struct a
--         bitcast struct (ptr (StructureType False [LLVM.typeOf g]))
--        LT -> error "Implementation error"
--    _ -> error ("Function not in scope: '" <> show fun <> "'")
--
--downCast :: (MonadIRBuilder m) => (Operand, LLVM.Type, Type) -> m Operand
--downCast = \case
--  (op, llvmt, t) | charPtr == llvmt && (t `elem` [tInt32, tInt64]) -> do
--    inttoptr op charPtr 
--  (op, _, _) -> pure op
--
--upCast :: (MonadIRBuilder m) => (Operand, LLVM.Type, Type) -> m Operand
--upCast = \case
--  (op, llvmt, t) | charPtr == llvmt && (t `elem` [tInt32, tInt64]) -> do
--    ptrtoint op (llvmType t)
--  (op, _, _) -> pure op
--
--emitCase :: CodeGen Operand -> [([Label Type], CodeGen Operand)] -> CodeGen Operand
--emitCase expr cs = mdo 
--  dt <- expr
--  m <- loadFromOffset 0 dt
--  let names = blocks <#> snd
--  switch
--    m
--    (last names)  -- Default case
--    [ (Int 8 (fromIntegral i), block)
--    | (i, block) <- zip [1 ..] (init names)
--    ]
--  blocks <-
--    forM cs $ \(con:fields, body) -> do
--      blk <- block `named` "case"
--      r <-
--        if null fields
--          then body
--          else do
--            Env env <- ask
--            let argTys = argTypes (fst con)
--                llvmTys = llvmArgTypes (env ! snd con)
--            dn <- bitcast dt (ptr (StructureType False (i8 : llvmTys)))
--            ops <-
--              forM (zip3 [1..] llvmTys argTys) $ \(i, llvmt, t) -> do
--                op <- loadFromOffset i dn
--                upCast (op, llvmt, t)
--            local (Env.inserts (zip (fields <#> snd) ops)) body
--      br end
--      cb <- currentBlock
--      pure ((r, cb), blk)
--  end <- block `named` "end"
--  phi (blocks <#> fst)
--
--emitExtern :: Name -> Signature a -> ModuleBuilder Operand
--emitExtern name Signature{ .. } =
--  extern
--    (llvmRep name)
--    (llvmType . fst <$> arguments)
--    (llvmType (fst body))
malloc :: (MonadIRBuilder m) => LLVM.Type -> m Operand
malloc ty = do
  size <- zext (ConstantOperand (sizeof ty)) i64
  m <- allocate size
  bitcast m (ptr ty)
  where
    allocate size = call (functionRef "gc_malloc" charPtr [i64]) [(size, [])]

storeAtOffs ::
     (MonadModuleBuilder m, MonadIRBuilder m)
  => Integer
  -> Operand
  -> Operand
  -> m ()
storeAtOffs offs op val = do
  p <- gep op [int32 0, int32 offs]
  store p 0 val

loadFromOffset ::
     (MonadModuleBuilder m, MonadIRBuilder m) => Integer -> Operand -> m Operand
loadFromOffset offs op = do
  p <- gep op [int32 0, int32 offs]
  load p 0
