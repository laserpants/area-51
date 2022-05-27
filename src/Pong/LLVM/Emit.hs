{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Pong.LLVM.Emit where

-- --import Control.Monad.Writer
-- --import Data.Char (isUpper)
-- --import Data.Foldable (foldlM, foldrM)
-- --import LLVM.AST.Attribute (ParameterAttribute)
-- --import qualified Data.Map.Strict as Map
-- --import qualified Data.Text as Text
-- --import qualified Pong.Util.Env as Env

-- import Data.Function ((&))

import Debug.Trace
-- import LLVM.Pretty
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad.State
import Data.List.NonEmpty (toList)
import Data.String (IsString, fromString)
-- , uncurry3)

import qualified Data.Text.Lazy.IO as Text
import Data.Tuple.Extra (first, second)
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

type CodeGenEnv = Environment (Either (Name, [Ast]) (MonoType, Operand))

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

namedReference :: Name -> LLVM.Type
namedReference = NamedTypeReference . llvmRep

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
      ty@TArr{} -> ptr (StructureType False [ptr funTy])
        where
          types = llvmType <$> unwindType (embed ty)
          funTy =
            FunctionType
              { argumentTypes = charPtr : init types
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

ref :: LLVM.Name -> LLVM.Type -> Operand
ref name = globalRef name . ptr

functionRef :: LLVM.Name -> LLVM.Type -> [LLVM.Type] -> Operand
functionRef name rty argtys = ref name (FunctionType rty argtys False)

{- ORMOLU_DISABLE -}

-- ----buildDataType :: (MonadModuleBuilder m) => Name -> [Constructor] -> m ()
-- ----buildDataType tyName cstrs = do
-- ----  tp <- typedef (llvmRep tyName) (Just (StructureType False [i8]))
-- ----  forM_ (sortOn consName cstrs `zip` [1 ..]) $ \(Constructor tcon fields, i) -> do
-- ----    let ts = llvmType <$> fields
-- ----        con = llvmRep (tyName <> "__" <> tcon)
-- ----    td <-
-- ----      typedef con
-- ----        (Just (StructureType False (i8 : ts)))
-- ----    function con
-- ----      (zip ts (repeat "a"))
-- ----      (ptr tp)
-- ----      (\args -> do
-- ----        op <- malloc td
-- ----        storeAtOffs 0 op (int8 i) -- constructor #
-- ----        forM_ (args `zip` [1 ..]) $ \(arg, j) ->
-- ----          storeAtOffs j op arg
-- ----        p <- bitcast op (ptr tp)
-- ----        ret p)
-- ----
-- --
-- --forEachDef :: (Monad m) => Map Name (a, b) -> (Name -> a -> b -> m c) -> m [c]
-- --forEachDef map f = forM (Map.toList map) $ uncurry (uncurry . f)
-- --
-- --buildProgram :: Name -> Program -> LLVM.Module
-- --buildProgram prog Program {..} =
-- --  buildModule (llvmRep prog) $ do
-- --    let ds = definitions <#> annotate
-- --    env <-
-- --      Env.fromList . concat <$$> forEachDef ds $ \name t -> do
-- --        \case
-- --          External Signature {..} -> do
-- --            op <-
-- --              extern
-- --                (llvmRep name)
-- --                (llvmType <$> arguments)
-- --                (llvmType body)
-- --            pure [(name, (t, op))]
-- --          Constant lit -> do
-- --            let constant = llvmPrim lit
-- --            op <- global (llvmRep name) (LLVM.typeOf constant) constant
-- --            pure [(name, (t, op))]
-- --          Data tyName cstrs -> do
-- --            tp <- typedef (llvmRep tyName) (Just (StructureType False [i16]))
-- --            concat <$$> forM (sortOn consName cstrs `zip` [1 ..]) $ \(Constructor tcon fields, i) -> do
-- --              let ts = llvmType <$> fields
-- --                  con = llvmRep (tyName <> "__" <> tcon)
-- --              td <- typedef con (Just (StructureType False (i16 : ts)))
-- --              f <-
-- --                function
-- --                  con
-- --                  (zip ts (repeat "a"))
-- --                  (ptr tp)
-- --                  (\args -> do
-- --                     op <- malloc td
-- --                     storeAtOffs 0 op (ConstantOperand (Int 16 i)) -- constructor #
-- --                     forM_ (args `zip` [1 ..]) $ \(arg, j) ->
-- --                       storeAtOffs j op arg
-- --                     p <- bitcast op (ptr tp)
-- --                     ret p)
-- --              pure [(tcon, (foldType (tData tyName) fields, f))]
-- --          Function Signature {..} ->
-- --            pure
-- --              [ ( name
-- --                , ( t
-- --                  , functionRef
-- --                      (llvmRep name)
-- --                      (llvmType (fst body))
-- --                      (llvmType . fst <$> arguments)))
-- --              ]
-- --    forEachDef ds $ \name t ->
-- --      \case
-- --        Function Signature {..} -> do
-- --          void $
-- --            function
-- --              (llvmRep name)
-- --              (arguments <#> llvmType *** llvmRep)
-- --              (llvmType (fst body))
-- --              (\ops -> do
-- --                 let args = [(n, (t, op)) | (op, (t, n)) <- zip ops arguments]
-- --                 runReaderT
-- --                   (getCodeGen $ emitBody (snd body) >>= ret)
-- --                   (Env.inserts args env))
-- --        _ -> pure ()
--
-- --bap :: Expr MonoType Void Void () -> CodeGen Ast
-- --bap = undefined
-- --
-- --zap :: ExprF MonoType Void Void () (Ast, CodeGen Operand) -> CodeGen Ast
-- --zap x =
-- --  (cata bap x)
-- --    where
-- --      y = project x
-- --  project >>> cata ( \case
-- --    EVar _ -> undefined
-- --  )
--
-- --___emitBody
-- --  :: (MonadModuleBuilder m, MonadFix m, MonadReader CodeGenEnv m, MonadState (Int, Program MonoType Ast) m)
-- --  => Ast
-- --  -> IRBuilderT m Operand
-- ___emitBody :: Ast -> CodeGen Operand
-- ___emitBody =
--   para $
--     \case
--       ELet (t, name) (Fix (ECall _ (_, fun) args), _) (_, body)
--         | arity t > length args ->
--           local (Env.insert name (Left (fun, args))) body
--       expr ->
--         cataBody (snd <$> expr)
--   where
--     cataBody =
--       \case
-- --        ELet (t, name) expr1 expr2 -> do
-- --          e1 <- expr1
-- --          p <- alloca (LLVM.typeOf e1) Nothing 0
-- --          store p 0 e1
-- --          local (Env.insert name (Right (t, p))) expr2
-- --        ELit lit ->
-- --          __emitPrim lit
-- --        EIf expr1 expr2 expr3 -> mdo
-- --          ifOp <- expr1
-- --          condBr ifOp thenBlock elseBlock
-- --          thenBlock <- block `named` "then"
-- --          thenOp <- expr2
-- --          br mergeBlock
-- --          elseBlock <- block `named` "else"
-- --          elseOp <- expr3
-- --          br mergeBlock
-- --          mergeBlock <- block `named` "ifcont"
-- --          phi [(thenOp, thenBlock), (elseOp, elseBlock)]
-- --        EOp1 op expr1 -> do
-- --          a <- expr1
-- --          __emitOp1Instr op a
-- --        EOp2 op expr1 expr2 -> do
-- --          a <- expr1
-- --          b <- expr2
-- --          __emitOp2Instr op a b
--         _ -> do
--           let zzz = undefined :: Ast
--           function
--             ""
--             undefined
--             undefined
--             (\ops ->
--               lift (___emitBody zzz >>= ret))
--
--   --      EVar (t, name) ->
--   --        Env.askLookup name >>= \case
--   --          Just (Right (_, op)) | isConT ArrT t ->
--   --            emitCall (t, name) []
--   --          Just (Right (_, op)) ->
--   --            pure op
--   --          _ ->
--   --            error "1111"
--     --            Just (Left ast) ->
--     --              emitBody ast
--     --        _ ->
--     --          error ("Not in scope: '" <> show name <> "'")
--   --      ECase expr clauses ->
--   --        emitCase expr (sortOn fst clauses)
--   --      ECall () fun args -> do
--   --        as <- sequence args
--   --        emitCall fun as
--
-- --zarf :: (MonadModuleBuilder m, MonadFix m, MonadReader CodeGenEnv m, MonadState (Int, Program MonoType Ast) m) => IRBuilderT (IRBuilderT m) ()
-- --zarf = lift (___emitBody undefined >>= ret)

emitBody :: Ast -> CodeGen Operand
emitBody =
  para $
    \case
      ELet (t, name) (Fix (ECall _ (_, fun) args), _) (_, body)
        | arity t > length args ->
          local (Env.insert name (Left (fun, args))) body
      expr ->
        cataBody (snd <$> expr)
  where
    cataBody =
      \case
        ELet (t, name) expr1 expr2 -> do
          e1 <- expr1
          local (Env.insert name (Right (t, e1))) expr2
          --p <- alloca (LLVM.typeOf e1) Nothing 0
          --store p 0 e1
          --local (Env.insert name (Right (t, p))) expr2
        ELit lit ->
          emitPrim lit
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
        EVar (t, "add1") ->
          error "!!!!OOF"
        EVar (t, name) ->
          Env.askLookup name >>= \case
            Just (Right (_, op)) | isConT ArrT t ->
              emitCall (t, name) []
--            Just (Right (_, ConstantOperand GlobalReference{})) -> do
--              emitCall (t, name) []
--              traceShow op
--                $ undefined
            Just (Right (_, op)) -> do
           --   xx1 <- emitPrim (PInt 9)
           --   xx2 <- emitPrim (PInt 10)
           --   traceShowM op
           --   traceShowM "&&&&&"
           --   call op (zip ([xx1, xx2]) (repeat []))
           --   --undefined
              pure op
            Just (Left ast) -> do
              undefined
            _ ->
              error ("Not in scope: '" <> show name <> "'")
--             Just (Left ast) -> do
--               traceShowM "*(*(*(*("
--               traceShowM ast
--               undefined -- emitBody ast
--             _ ->
--               error ("Not in scope: '" <> show name <> "'")
        ECase expr clauses ->
          emitCase expr (sortOn fst clauses)
        ECall () fun args -> do
          traceShowM ("Call : " <> show (snd fun))
          as <- sequence args
          emitCall fun as
        eee -> do
          ddd <- sequence eee
          error (show ddd)
-- --      flip cata zz $ \case
-- --        _ ->
-- --          undefined
-- --      dd <- zap expr
-- --      --let ee = dd  :: Int
-- --      --undefined
-- --      --let dd = sequence zz
-- --      --ee <- dd
-- --      ----    dd = embed zz
-- --      ----let dd = sequence zz
-- --      ----ee <- dd
-- --      ----let ff = ee
-- --      undefined -- emitBody_ dd
-- --      snd <$> expr & \case
-- --        ELet (t, name) expr1 expr2 -> do
-- --          e1 <- expr1
-- --          p <- alloca (LLVM.typeOf e1) Nothing 0
-- --          store p 0 e1
-- --          local (Env.insert name (Right (t, p))) expr2
-- --        ELit lit ->
-- --          emitPrim lit
-- --        EIf expr1 expr2 expr3 -> mdo
-- --          ifOp <- expr1
-- --          condBr ifOp thenBlock elseBlock
-- --          thenBlock <- block `named` "then"
-- --          thenOp <- expr2
-- --          br mergeBlock
-- --          elseBlock <- block `named` "else"
-- --          elseOp <- expr3
-- --          br mergeBlock
-- --          mergeBlock <- block `named` "ifcont"
-- --          phi [(thenOp, thenBlock), (elseOp, elseBlock)]
-- --        EOp1 op expr1 -> do
-- --          a <- expr1
-- --          emitOp1Instr op a
-- --        EOp2 op expr1 expr2 -> do
-- --          a <- expr1
-- --          b <- expr2
-- --          emitOp2Instr op a b
-- --        EVar (t, name) ->
-- --          Env.askLookup name >>= \case
-- --            Just (Right (_, op)) | isConT ArrT t ->
-- --              emitCall (t, name) []
-- --            Just (Right (_, op)) ->
-- --              pure op
-- ----            Just (Left ast) ->
-- ----              emitBody ast
-- --            _ ->
-- --              error ("Not in scope: '" <> show name <> "'")
-- --        ECase expr clauses ->
-- --          emitCase expr (sortOn fst clauses)
-- --        ECall () fun args ->
-- --          emitCall fun args
--
-- --  cata $ \case
-- --    ELit lit ->
-- --      emitPrim lit
-- --    EIf expr1 expr2 expr3 -> mdo
-- --      ifOp <- expr1
-- --      condBr ifOp thenBlock elseBlock
-- --      thenBlock <- block `named` "then"
-- --      thenOp <- expr2
-- --      br mergeBlock
-- --      elseBlock <- block `named` "else"
-- --      elseOp <- expr3
-- --      br mergeBlock
-- --      mergeBlock <- block `named` "ifcont"
-- --      phi [(thenOp, thenBlock), (elseOp, elseBlock)]
-- --    EOp1 op expr1 -> do
-- --      a <- expr1
-- --      emitOp1Instr op a
-- --    EOp2 op expr1 expr2 -> do
-- --      a <- expr1
-- --      b <- expr2
-- --      emitOp2Instr op a b
-- --    EVar (t, name) ->
-- --      Env.askLookup name >>= \case
-- --        Just (Right (_, op)) | isConT ArrT t ->
-- --          emitCall (t, name) []
-- --        Just (Right (_, op)) ->
-- --          pure op
-- --        Just (Left ast) ->
-- --          emitBody ast
-- --        _ ->
-- --          error ("Not in scope: '" <> show name <> "'")
-- --    ECase expr clauses ->
-- --      emitCase expr (sortOn fst clauses)
-- --    ECall () fun args ->
-- --      emitCall fun args
-- --    ELet (t, name) expr1 expr2 -> do
-- --      e1 <- expr1
-- --      local (Env.insert name (Right (t, e1))) expr2

emitCase
  :: CodeGen Operand
  -> [([Label MonoType], CodeGen Operand)]
  -> CodeGen Operand
emitCase =
  undefined
-- emitCase expr cs = do
--   e <- expr
--   traceShowM ">>>>>>>>>>"
--   traceShowM e
--   traceShowM "<<<<<<<<<<"
--   undefined
-- --emitCase expr cs = mdo
-- --  e <- expr
-- --  m <- loadFromOffset 0 e
-- --  let names = blocks <#> snd
-- --  switch
-- --    m
-- --    (last names)  -- Default case
-- --    [ (Int 16 (fromIntegral i), block)
-- --    | (i, block) <- zip [1 ..] (init names)
-- --    ]
-- --  blocks <-
-- --    forM cs $ \((t, con):fields, body) -> do
-- --      blk <- block `named` "case"
-- --      r <-
-- --        if null fields
-- --          then body
-- --          else do
-- --            Env env <- ask
-- --            let t0 = fst (env ! con)
-- --            struct <- bitcast e (ptr (StructureType False (i16 : (argTypes t0 <#> llvmType))))
-- --            ops <-
-- --              forM (zip3 [1..] (argTypes t0) (argTypes t)) $ \(i, t0, t) -> do
-- --                op <- loadFromOffset i struct
-- --                op' <- foo2 (op, t0, t)
-- --                pure (t, op')
-- --            local (Env.inserts (zip (fields <#> snd) ops)) body
-- --      br end
-- --      cb <- currentBlock
-- --      pure ((r, cb), blk)
-- --  end <- block `named` "end"
-- --  phi (blocks <#> fst)
--
-- --xcall f fun t0 t1 as
-- --  | any (isConT VarT) ts2 =
-- --    case runTypeChecker' (freeIndex [t0, t1]) mempty (unify t0 t1) of
-- --      Right sub -> do
-- --        traceShowM f
-- --        traceShowM fun
-- --        traceShowM sub
-- --        error "*********"
-- --      _ ->
-- --        error "/////////"
-- --  | otherwise = do
-- --    call fun (zip as (repeat []))
-- ----  as1 <- traverse (uncurry3 cast) (zip3 as ts1 ts2)
-- ----  r <- call fun (zip as1 (repeat []))
-- ----  case (project rt1, project rt2) of
-- ----    _ | rt1 == rt2 -> pure r
-- ----    (TInt, TVar {}) ->
-- ----      ptrtoint r (llvmType tInt)
-- --  where
-- --    ts1, ts2 :: [MonoType]
-- --    ts1 = unwindType t0
-- --    ts2 = unwindType t1
-- ----    rt1 = last ts1
-- ----    rt2 = last ts2
-- ----    cast op t1 t2 =
-- ----      case (project t1, project t2) of
-- ----        _ | t1 == t2 -> pure op
-- ----        (TInt, TVar {}) ->
-- ----          inttoptr op charPtr
--
--
-- --emitCall (t, "Nil") args = do
-- --  as <- sequence args
-- --  error "boooo"
-- --
-- --emitCall (t, "Cons") args = do
-- --  as <- sequence args
-- --  error "zoooo"
--
-- --gork :: Name -> p -> CodeGen Name
-- gork f sub = do
--   (_, Program p) <- get
--   case Map.lookup f p of
--     Just def -> do
--       let name = "xxx"
--           Function args (t1, body) = apply sub def
--
--       traceShowM body
--       traceShowM args
--       traceShowM "xxxxxxxx"
--
--       function
--         (llvmRep name)
--         (toList args <#> llvmType *** llvmRep)
--         (llvmType t1)
--         (\ops -> do
--           let foo = [(n, Right (t, op)) | (op, (t, n)) <- zip ops (toList args)]
--           traceShowM foo
--           lift (local (Env.inserts foo) (emitBody body >>= ret))
--           --  undefined -- (Env.inserts [(n, Right (t, op)) | (op, (t, n)) <- zip ops (toList args)] env)
--           --  undefined -- (Program defs)
--           --  undefined -- (emitBody body >>= ret)
--           --  let env = mempty
--           --  runReaderT
--           --    (lift (getCodeGen $ emitBody body >>= ret))
--           --    (Env.inserts [(n, (t, op)) | (op, (t, n)) <- zip ops (toList args)] env)
--           )
--
--       --insertDef name (apply sub def)
--       --pure name
--     Nothing ->
--       error "Implementation error"
--
-- zopp :: IRBuilderT CodeGen ()
-- zopp = undefined
--
-- --runCodeGenX :: CodeGenEnv -> Program MonoType Ast -> CodeGen a -> IRBuilderT CodeGen a
-- --runCodeGenX env prog (CodeGen code) = undefined -- runReaderT (evalStateT code (1, prog)) env

emitCall :: Label MonoType -> [Operand] -> CodeGen Operand
emitCall (t, f) as = do
  traceShowM "----------------"
  traceShowM t 
  traceShowM f 
  Env.askLookup f >>= \case
    --Just (Right (t0, op@(ConstantOperand a))) -> do
    --  traceShowM a
    --  undefined
    Just (Right (t0, op@(LocalReference abc z))) -> do
      --a <- bitcast op (ptr fff0)
      --      --(StructureType { isPacked = False
      --      --               
      --      --               , elementTypes = [IntegerType {typeBits = 8},PointerType { pointerAddrSpace = AddrSpace 0, pointerReferent = FunctionType {resultType = IntegerType {typeBits = 64}, argumentTypes = [IntegerType {typeBits = 64},IntegerType {typeBits = 64}]
      --      --                                                                                                        , isVarArg = False}  } ] 
      --      --               }
      --      --               ))
      --p0 <- gep a [int32 0, int32 0]
      --p1 <- gep a [int32 0, int32 1]
      --n <- load p0 0
      --o <- load p1 0

      xx1 <- emitPrim (PInt 9)
      xx2 <- emitPrim (PInt 10)

      --traceShow abc $
      call op (zip ([xx1, xx2]) (repeat []))

      --let LocalReference xx123 zz123 = o
      --traceShowM fff0
      --traceShowM yy
      --traceShowM z
      --traceShowM xx123
      --traceShowM zz123
      --traceShowM t0
      --traceShowM f
      --traceShowM "//3/3/3/3/3/3"
      --let zoof = GlobalReference xx123 (LLVM.Name "add")
      --    boof = ConstantOperand zoof

      ----call o (zip as (repeat []))
      ----call boof (zip ([xx1] <> as) (repeat []))
      --call o (zip ([xx1] <> as) (repeat []))

--      traceShow n
--        $ traceShow o
--          $ error "<<-=="

      --let bop = ConstantOperand (GlobalReference xx z)
      --traceShow "????"
      --  $ traceShow op
      --      $ do
      --    -- $ traceShow z
      --    --  $ traceShow bop
      --        -- $ error "FOO"
--      --let op = xx
--                          p0 <- gep op [int32 0, int32 0]
----                          p1 <- gep op [int32 0, int32 1]
--                          n <- load p0 0
----                          fun <- load p1 0
----
----                          let LocalReference nnn _ = n
----                          q0 <- gep nnn [int32 0, int32 0]
----                          nn <- load q0 0
----
--                          traceShow n
--                            $ error (show n)
--      --let op = ConstantOperand (GlobalReference (PointerType {pointerReferent = FunctionType {resultType = IntegerType {typeBits = 64}, argumentTypes = [IntegerType {typeBits = 64},IntegerType {typeBits = 64}], isVarArg = False}, pointerAddrSpace = AddrSpace 0}) (LLVM.Name "add")) 
--      --    bop = ConstantOperand (GlobalReference xx (LLVM.Name "add"))
--      --call bop (zip as (repeat []))
--      traceShow "????"
--        $ traceShow op
--          $ error "??"
      --    $ traceShow yy
      --      $ call bop (zip as (repeat []))
    Just (Right (t0, op)) -> do
      case compare (arity t) (length as) of
        EQ -> do
          call op (zip as (repeat []))
        GT -> do
          --traceShow "!!!!"
          --  $ traceShow op
          --    $ pure op
          --
          --pure op

          xx1 <- emitPrim (PInt 9)
          xx2 <- emitPrim (PInt 10)

          call op (zip ([xx1, xx2]) (repeat []))

          --local (Env.inserts undefined) $ do
          --let zzab = GlobalReference (llvmType t0) (llvmRep f)
          --traceShow "!!!!"
          --  $ traceShow zzab
          --    $ traceShow "!!!!"
          --      $ traceShow op
          --        $ do
          --          let sty = StructureType False [i8, LLVM.typeOf op] --  : (LLVM.typeOf <$> as))
          --          struct <- malloc sty
          --          p0 <- gep struct [int32 0, int32 0]
          --          p1 <- gep struct [int32 0, int32 1]
          --          store p0 0 (int8 (fromIntegral (length as)))
          --          store p1 0 op
          --          --traceShow struct
          --          --  $ error "OFSDFDS"
          --          --traceShow struct
          --          pure struct
    Just (Left (fun, args)) -> do
      error "#3"
    e ->
      error (show e)

--emitCall :: Label MonoType -> [Operand] -> CodeGen Operand
--emitCall (t, f) as = do
--   undefined
-- ----  as <- sequence args
-- --  Env.askLookup f >>= \case
-- --    Just (Right (t0, op@(LocalReference PointerType {..} _))) -> do
-- ----      let sty = StructureType False [i8, llvmType t0]
-- ----      p <- bitcast op (ptr sty)
-- --      p0 <- gep op [int32 0, int32 0]
-- --      p1 <- gep op [int32 0, int32 1]
-- --      n <- load p0 0
-- --      fun <- load p1 0
-- ------      as1 <- forM_ [2..2] $ \i -> do -- TODO
-- ------        q <- gep p [int32 0, int32 i]
-- ------        load q 0
-- --      --fun <- bitcast xp (llvmType t0)
-- --      --emitPrim (PInt 32)
-- --      traceShowM "======="
-- --      traceShowM f
-- --      traceShowM t
-- --      traceShowM t0
-- --      undefined
-- --      --xcall f fun t t0 as
-- --      --call fun (zip as (repeat []))
-- --
-- --    Just (Right (t0, op)) -> do
-- --      case compare (arity t) (length as) of
-- --        EQ -> do
-- --          --traceShowM ">>>>>>>>>>>>"
-- --          --traceShowM t
-- --          --traceShowM t0
-- --          --traceShowM "<<<<<<<<<<<<"
-- --          if t == t0
-- --             then
-- --               call op (zip as (repeat []))
-- --             else
-- --               case runTypeChecker' (freeIndex [t, t0]) mempty (unify t t0) of
-- --                 Right sub -> do
-- --                   g <- gork f sub
-- --                   call g (zip as (repeat []))
-- --
-- --          --xcall f op t t0 as
-- --        GT -> do
-- --          let sty = StructureType False [i8, LLVM.typeOf op] --  : (LLVM.typeOf <$> as))
-- --          struct <- malloc sty
-- --          p0 <- gep struct [int32 0, int32 0]
-- --          p1 <- gep struct [int32 0, int32 1]
-- --          store p0 0 (int8 0) -- (fromIntegral (length as)))
-- --          store p1 0 op
-- --          --forM_ (as `zip` [2 ..]) $ \(arg, i) -> do
-- --          --  q <- gep struct [int32 0, int32 i]
-- --          --  store q 0 arg
-- --          --traceShowM "************************"
-- --          --traceShowM struct
-- --          --traceShowM "************************"
-- --          pure struct
-- --          -- error "FOO"
-- --    Just (Left (fun, args)) -> do
-- --      xs <- traverse emitBody args
-- --      emitCall (t, fun) (xs <> as)
-- --
--       --call fun (zip as (repeat []))
--
-- --    Just (Left ast) -> do
-- --      traceShowM "///////////"
-- --      traceShowM t
-- --      traceShowM (typeOf ast :: MonoType)
-- --      traceShowM ast
-- --
-- --      let t1 = typeOf ast :: MonoType
-- --          Right sub = runTypeChecker' (freeIndex [t, t1]) mempty (unify t t1)
-- --          ast1 = apply sub ast
-- --
-- --      traceShowM sub
-- --      traceShowM ast1
-- --      traceShowM as
-- --
-- --      emitBody ast1
-- --
--       --name <- uniqueName_ "$h"
--       --x <- emitBody ast
--
-- --    case runTypeChecker' (freeIndex [t0, t1]) mempty (unify t0 t1) of
-- --      Right sub -> do
-- --
-- --    x ->
-- --      error (show x)
-- --    Just (t0, op@(LocalReference PointerType {..} _)) -> do
-- --      a <- bitcast op charPtr
-- --      undefined
-- --      h <- loadFromOffset 0 op
-- --      x <- inttoptr (ConstantOperand (Int 32 9991)) charPtr -- TODO: TEMP
-- --      -- TEMP
-- --      --zz <- inttoptr (ConstantOperand (Int 32 123)) charPtr
-- --      call h (zip (a : as) (repeat []))
-- --    Just (t0, op) -> do
-- --      as' <- traverse foo1 (zip3 as (unwindType t0) (unwindType t))
-- --      let tys = argTypes t
-- --          (ts1, ts2) = splitAt (length args) (llvmType <$> tys)
-- --      case compare (arity t) (length args) of
-- --        EQ -> do
-- --          r <- call op (zip as' (repeat []))
-- --          foo2 (r, returnTypeOf t0, returnTypeOf t)
-- --          -- TODO return?
-- --        GT -> mdo
-- --          let sty = StructureType False (LLVM.typeOf g : LLVM.typeOf op : ts1)
-- --          name <- freshName "fun"
-- --          g <-
-- --            function
-- --              name
-- --              (zip (charPtr : ts2) (repeat "a"))
-- --              (llvmType (returnTypeOf t0))
-- --              (\(v:vs) -> do
-- --                s <- bitcast v (ptr sty)
-- --                h <- loadFromOffset 1 s
-- --                an <-
-- --                  forM [2 .. length ts1 + 1] $ \n ->
-- --                    loadFromOffset (fromIntegral n) s
-- --                r <- call h (zip (an <> vs) (repeat []))
-- --                ret r)
-- --          struct <- malloc sty
-- --          storeAtOffs 0 struct g
-- --          storeAtOffs 1 struct op
-- --          forM_ (as `zip` [2 ..]) $ \(a, i) ->
-- --            storeAtOffs i struct a
-- --          bitcast struct (ptr (StructureType False [LLVM.typeOf g]))
-- --        LT -> error "Implementation error"
-- --    _ -> error ("Function not in scope: '" <> show fun <> "'")
-- --
-- --foo1 :: (Operand, Type, Type) -> CodeGen Operand
-- --foo1 = \case
-- --  (op, s, t) | isTCon VarT s && t `elem` [tInt32, tInt64] ->
-- --    inttoptr op charPtr
-- ----  (op@(ConstantOperand Int {}), t) | tOpaque == t -> do
-- ----    inttoptr op charPtr
-- --  (op, _, _) -> pure op
-- --
-- --foo2 :: (Operand, Type, Type) -> CodeGen Operand
-- --foo2 = \case
-- --  (op, s, t) | isTCon VarT s && t `elem` [tInt32, tInt64] ->
-- --    ptrtoint op (llvmType t)
-- ----  (op@(ConstantOperand Int {}), t) | tOpaque == t -> do
-- ----    inttoptr op charPtr
-- --  (op, _, _) -> pure op
-- --
-- ----foo2 :: (Operand, Type) -> CodeGen Operand
-- ----foo2 = \case
-- ----  (op, t) | charPtr == LLVM.typeOf op && t `elem` [tInt32, tInt64] ->
-- ----    ptrtoint op (llvmType t)
-- ----  (op, _) -> pure op
-- --
-- --
-- ----forEachDef ::
-- ----     (Monad m)
-- ----  => Map Name (Definition Ast)
-- ----  -> (Name -> Type -> Definition Ast -> m b)
-- ----  -> m [b]
-- ----forEachDef map f = forM (Map.toList map) g
-- ----  where
-- ----    g (name, def) = f name (Pong.Lang.typeOf def) def
-- ----
-- ----buildProgram :: Name -> Program -> LLVM.Module
-- ----buildProgram mod Program {..} =
-- ----  buildModule (llvmRep mod) $ do
-- ----    env <-
-- ----      Env.fromList . concat <$$> forEachDef definitions $ \name ->
-- ----        \case
-- ----          Data _ css -> do
-- ----            t1 <- typedef (llvmRep name) (Just (StructureType False [i8]))
-- ----            concat <$$> forM (sortOn consName css `zip` [1 ..]) $ \(Constructor tcon fields, i) -> do
-- ----              let ts = llvmType <$> fields
-- ----                  con = llvmRep (name <> "__" <> tcon)
-- ----              td <-
-- ----                typedef con
-- ----                  (Just (StructureType False (i8 : ts)))
-- ----              f <-
-- ----                function con
-- ----                  (zip ts (repeat "a"))
-- ----                  (ptr t1)
-- ----                  (\args -> do
-- ----                    op <- malloc td
-- ----                    storeAtOffs 0 op (int8 i) -- constructor #
-- ----                    forM_ (args `zip` [1 ..]) $ \(arg, j) ->
-- ----                      storeAtOffs j op arg
-- ----                    bitcast op (ptr t1) >>= ret)
-- ----              pure [(tcon, f)]
-- ----          Function Signature {..} -> do
-- ----            pure
-- ----              [ (name
-- ----              , functionRef
-- ----                  (llvmRep name)
-- ----                  (llvmType (fst body))
-- ----                  (llvmType . fst <$> arguments)) ]
-- ----          External sig -> do
-- ----            op <- emitExtern name sig
-- ----            pure [(name, op)]
-- ----          Constant _ -> pure []
-- ----    void <$$> forEachDef definitions $ \name ->
-- ----      \case
-- ----        --Function Signature {..} ->
-- ----        --  void $
-- ----        --  function
-- ----        --    (llvmRep name)
-- ----        --    (arguments <#> llvmType *** llvmRep)
-- ----        --    (llvmType (fst body))
-- ----        --    $ \ops -> do
-- ----        --          let args = zip (arguments <#> snd) ops
-- ----        --          flip runReaderT (Env.inserts args env) $ getCodeGen $
-- ----        --            emitBody (snd body) >>=
-- ----        --            ret
-- ----        _ ->
-- ----          pure ()
-- ----
-- ----emitBody :: Ast -> CodeGen Operand
-- ----emitBody =
-- ----  cata $ \case
-- ----    ELit prim ->
-- ----      emitPrim prim
-- ----    EIf expr1 expr2 expr3 ->
-- ----      mdo ifOp <- expr1
-- ----          condBr ifOp thenBlock elseBlock
-- ----          thenBlock <- block `named` "then"
-- ----          thenOp <- expr2
-- ----          br mergeBlock
-- ----          elseBlock <- block `named` "else"
-- ----          elseOp <- expr3
-- ----          br mergeBlock
-- ----          mergeBlock <- block `named` "ifcont"
-- ----          phi [(thenOp, thenBlock), (elseOp, elseBlock)]
-- ----    EOp2 op expr1 expr2 -> do
-- ----      a <- expr1
-- ----      b <- expr2
-- ----      emitOp2Instr op a b
-- ----    EVar (t, name) ->
-- ----      Env.askLookup name >>= \case
-- ----        Just op | isTCon ArrT t -> do
-- ----          emitCall (t, name) []
-- ----        Just op -> pure op
-- ----        _ ->
-- ----          error ("Not in scope: '" <> show name <> "'")
-- ----    ECase expr cs -> emitCase expr (sortOn fst cs)
-- ----    ECall () fun args -> emitCall fun args
-- ----
-- ----emitCall :: Label Type -> [CodeGen Operand] -> CodeGen Operand
-- ----emitCall (t, fun) args = do
-- ----  as <- sequence args
-- ----  Env.askLookup fun >>= \case
-- ----    Just op@(LocalReference PointerType {..} _) -> do
-- ----      a <- bitcast op charPtr
-- ----      h <- loadFromOffset 0 op
-- ----      -- TEMP
-- ----      --zz <- inttoptr (ConstantOperand (Int 32 123)) charPtr
-- ----      call h (zip (a : as) (repeat []))
-- ----      --pure zz
-- ----    Just op -> do
-- ----      let tys = argTypes t
-- ----          (ts1, ts2) = splitAt (length args) (llvmType <$> tys)
-- ----      as111 <- traverse downCast (zip3 as (llvmArgTypes op) tys)
-- ----      case compare (arity t) (length args) of
-- ----        EQ -> do
-- ----          r <- call op (zip as111 (repeat []))
-- ----          traceShowM "******************************"
-- ----          traceShowM "******************************"
-- ----          traceShowM r
-- ----          traceShowM op
-- ----          traceShowM (returnTypeOf t)
-- ----          upCast (r, llvmRetType op, returnTypeOf t)
-- ----          --zz <- inttoptr (ConstantOperand (Int 32 999)) charPtr
-- ----          --pure zz
-- ----          -- TODO
-- ----        GT -> mdo
-- ----         let sty = StructureType False (LLVM.typeOf g : LLVM.typeOf op : ts1)
-- ----         name <- freshName "fun"
-- ----         g <-
-- ----           function
-- ----             name
-- ----             (zip (charPtr : ts2) (repeat "a"))
-- ----             (llvmRetType op)
-- ----             (\(v:vs) -> do
-- ----               s <- bitcast v (ptr sty)
-- ----               h <- loadFromOffset 1 s
-- ----               an <-
-- ----                 forM [2 .. length ts1 + 1] $ \n ->
-- ----                   loadFromOffset (fromIntegral n) s
-- ----               r <- call h (zip (an <> vs) (repeat []))
-- ----               -- TEMP
-- ----               --zz <- inttoptr (ConstantOperand (Int 32 456)) charPtr
-- ----               --q <- upCast (r, llvmRetType op, returnTypeOf t)
-- ----               --upCast undefined -- (r, llvmRetType op, returnTypeOf t)
-- ----               ret r
-- ----               )
-- ----               -- ret r)
-- ----         struct <- malloc sty
-- ----         storeAtOffs 0 struct g
-- ----         storeAtOffs 1 struct op
-- ----         forM_ (as `zip` [2 ..]) $ \(a, i) ->
-- ----           storeAtOffs i struct a
-- ----         bitcast struct (ptr (StructureType False [LLVM.typeOf g]))
-- ----        LT -> error "Implementation error"
-- ----    _ -> error ("Function not in scope: '" <> show fun <> "'")
-- ----
-- ----downCast :: (MonadIRBuilder m) => (Operand, LLVM.Type, Type) -> m Operand
-- ----downCast = \case
-- ----  (op, llvmt, t) | charPtr == llvmt && (t `elem` [tInt32, tInt64]) -> do
-- ----    inttoptr op charPtr
-- ----  (op, _, _) -> pure op
-- ----
-- ----upCast :: (MonadIRBuilder m) => (Operand, LLVM.Type, Type) -> m Operand
-- ----upCast = \case
-- ----  (op, llvmt, t) | charPtr == llvmt && (t `elem` [tInt32, tInt64]) -> do
-- ----    ptrtoint op (llvmType t)
-- ----  (op, _, _) -> pure op
-- ----
-- ----emitCase :: CodeGen Operand -> [([Label Type], CodeGen Operand)] -> CodeGen Operand
-- ----emitCase expr cs = mdo
-- ----  dt <- expr
-- ----  m <- loadFromOffset 0 dt
-- ----  let names = blocks <#> snd
-- ----  switch
-- ----    m
-- ----    (last names)  -- Default case
-- ----    [ (Int 8 (fromIntegral i), block)
-- ----    | (i, block) <- zip [1 ..] (init names)
-- ----    ]
-- ----  blocks <-
-- ----    forM cs $ \(con:fields, body) -> do
-- ----      blk <- block `named` "case"
-- ----      r <-
-- ----        if null fields
-- ----          then body
-- ----          else do
-- ----            Env env <- ask
-- ----            let argTys = argTypes (fst con)
-- ----                llvmTys = llvmArgTypes (env ! snd con)
-- ----            dn <- bitcast dt (ptr (StructureType False (i8 : llvmTys)))
-- ----            ops <-
-- ----              forM (zip3 [1..] llvmTys argTys) $ \(i, llvmt, t) -> do
-- ----                op <- loadFromOffset i dn
-- ----                upCast (op, llvmt, t)
-- ----            local (Env.inserts (zip (fields <#> snd) ops)) body
-- ----      br end
-- ----      cb <- currentBlock
-- ----      pure ((r, cb), blk)
-- ----  end <- block `named` "end"
-- ----  phi (blocks <#> fst)
-- ----
-- ----emitExtern :: Name -> Signature a -> ModuleBuilder Operand
-- ----emitExtern name Signature{ .. } =
-- ----  extern
-- ----    (llvmRep name)
-- ----    (llvmType . fst <$> arguments)
-- ----    (llvmType (fst body))
-- --malloc :: (MonadIRBuilder m) => LLVM.Type -> m Operand
-- --malloc ty = do
-- --  size <- zext (ConstantOperand (sizeof ty)) i64
-- --  m <- allocate size
-- --  bitcast m (ptr ty)
-- --  where
-- --    allocate size = call (functionRef "gc_malloc" charPtr [i64]) [(size, [])]
-- --
-- --storeAtOffs ::
-- --     (MonadModuleBuilder m, MonadIRBuilder m)
-- --  => Integer
-- --  -> Operand
-- --  -> Operand
-- --  -> m ()
-- --storeAtOffs offs op val = do
-- --  p <- gep op [int32 0, int32 offs]
-- --  store p 0 val
-- --
-- --loadFromOffset ::
-- --     (MonadModuleBuilder m, MonadIRBuilder m) => Integer -> Operand -> m Operand
-- --loadFromOffset offs op = do
-- --  p <- gep op [int32 0, int32 offs]
-- --  load p 0

forEachDef
  :: Map (Label Scheme) (Definition MonoType Ast)
  -> (Label Scheme -> MonoType -> Definition MonoType Ast -> ModuleBuilder a)
  -> ModuleBuilder [a]
forEachDef m f = forM (Map.toList m) (\(name, def) -> f name (typeOf def) def)

buildProgram :: Name -> Program MonoType Ast -> LLVM.Module
buildProgram name (Program defs) = do
  buildModule (llvmRep name) $ do
    env <-
      Env.fromList . concat <$$> forEachDef defs $ \(_, name) t ->
        \case
          Extern args t1 -> do
            op <-
              extern
                (llvmRep name)
                (llvmType <$> args)
                (llvmType t1)
            pure [ ( name, Right (t, op) ) ] 
          Constant (t1, _) -> do
            pure
              [ ( name
                , Right
                  ( t
                  -- , ref (llvmRep name) (PointerType (llvmType t1) "")
                  --, ref (llvmRep name) (llvmType t1)
                  , functionRef
                      (llvmRep name)
                      (llvmType t1)
                      []
                  )
                )
              ]
          Function args (t1, _) ->
            pure
              [ ( name
                , Right
                  ( t
                  , functionRef
                      (llvmRep name)
                      (llvmType t1)
                      (llvmType . fst <$> toList args)
                  )
                )
              ]
    forEachDef defs $ \(_, name) t ->
      \case
        Constant (t1, body) -> do
          void $
            function
              (llvmRep name)
              []
              (llvmType t1)
              (\ops -> do
                runCodeGen
                  env
                  (Program defs)
                  (emitBody body >>= ret)
              )
        Function args (t1, body) ->
          void $
            function
              (llvmRep name)
              (toList args <&> llvmType *** llvmRep)
              (llvmType t1)
              (\ops -> do
                runCodeGen
                  (Env.inserts [(n, Right (t, op)) | (op, (t, n)) <- zip ops (toList args)] env)
                  (Program defs)
                  (emitBody body >>= ret)
              )
        _ ->
          pure ()
--    forEachDef defs $ \(_, name) t ->
--      \case
--        Function args (t1, body) ->
--          void $
--            function
--              (llvmRep name)
--              (toList args <&> llvmType *** llvmRep)
--              (llvmType t1)
--              (\ops -> do
--                runCodeGen
--                  (Env.inserts [(n, Right (t, op)) | (op, (t, n)) <- zip ops (toList args)] env)
--                  (Program defs)
--                  (emitBody body >>= ret)
--                --runReaderT
--                --  (getCodeGen $ emitBody body >>= ret)
--                --  (Env.inserts [(n, (t, op)) | (op, (t, n)) <- zip ops (toList args)] env))
--                )
--        _ ->
--          undefined
--          pure ()
--    undefined

-- --runCodeGen :: (Monad m) => StateT (Int, Program MonoType Ast)  (ReaderT CodeGenEnv m) a -> CodeGenEnv -> Program MonoType Ast -> IRBuilderT ModuleBuilder ()
-- --runCodeGen a env prog = yy
-- --  where
-- --    yy = runReaderT zz env
-- ----    zz :: ReaderT CodeGenEnv m (Int, Program MonoType Ast)
-- --    zz = execStateT a (1, prog)

uniqueName_ :: Name -> CodeGen Name
uniqueName_ prefix = do
  n <- gets fst
  modify (first succ)
  pure (prefix <> showt n)

-- __uniqueName_ :: (MonadState (Int, Program MonoType Ast) m) => Name -> IRBuilderT m Name
-- __uniqueName_ prefix = do
--   n <- gets fst
--   modify (first succ)
--   pure (prefix <> showt n)
--
-- forEachDef
--   :: (Monad m)
--   => Map (ProgKey MonoType) (Definition MonoType a)
--   -> (Label MonoType -> MonoType -> Definition MonoType a -> m e)
--   -> m [e]
-- forEachDef = undefined
--
-- --forEachDef
-- --  :: (Monad m)
-- --  => Map Name (Definition MonoType a)
-- --  -> (Name -> MonoType -> Definition MonoType a -> m e)
-- --  -> m [e]
-- --forEachDef map f =
-- --  forM (Map.toList map) (\(name, def) -> f name (typeOf def) def)

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

deriving instance (MonadState (Int, Program MonoType Ast)) CodeGen

deriving instance MonadFix CodeGen

deriving instance MonadIRBuilder CodeGen

deriving instance MonadModuleBuilder CodeGen

-- ----llvmFunType :: Operand -> LLVM.Type
-- ----llvmFunType =
-- ----  \case
-- ----    ConstantOperand (GlobalReference PointerType {..} _) -> pointerReferent
-- ----    LocalReference functionType _ -> functionType
-- ----
-- ----llvmArgTypes :: Operand -> [LLVM.Type]
-- ----llvmArgTypes = argumentTypes <<< llvmFunType
-- ----
-- ----llvmRetType :: Operand -> LLVM.Type
-- ----llvmRetType = resultType <<< llvmFunType
