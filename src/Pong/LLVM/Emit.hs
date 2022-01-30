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
import Debug.Trace
import LLVM.AST.Attribute (ParameterAttribute)
import Pong.LLVM hiding (void)
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
    TVar {} -> error "Implementation error"
    TChar {} -> error "Not implemented" -- TODO
    TString {} -> error "Not implemented" -- TODO
    TData name -> ptr (namedReference name)
    TOpaque -> charPtr
    ty@TArr {} -> ptr (StructureType False [ptr funTy])
      where types = llvmType <$> unwindType (embed ty)
            funTy =
              FunctionType
                { argumentTypes = charPtr : init types
                , resultType = last types
                , isVarArg = False
                }

emitLit :: Literal -> CodeGen Operand
emitLit =
  pure <<< ConstantOperand <<< \case
    LBool True -> Int 1 1
    LBool False -> Int 1 0
    LInt32 n -> Int 32 (toInteger n)
    LInt64 n -> Int 64 (toInteger n)
    LFloat f -> Float (Single f)
    LDouble d -> Float (Double d)
    LChar c -> error "Not implemented" -- TODO
    LString s -> error "Not implemented" -- TODO
    LUnit -> Struct Nothing False []

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

refOp :: LLVM.Name -> LLVM.Type -> Operand
refOp name ty = ConstantOperand (GlobalReference ty name)

functionRef :: LLVM.Name -> LLVM.Type -> [LLVM.Type] -> Operand
functionRef name rty argtys = refOp name (ptr (FunctionType rty argtys False))

forEachDef :: Monad m => Map k a -> (k -> a -> m b) -> m [b]
forEachDef map = forM (Map.toList map) . uncurry 

buildProgram :: Name -> Program -> LLVM.Module
buildProgram name Program {..} =
  buildModule (llvmRep name) $ do
    env <-
      Env.fromList . concat <$$> forEachDef definitions $ \name ->
        \case 
          Data _ css -> do
            t1 <- typedef (llvmRep name) (Just (StructureType False [i8]))
            concat <$$> forM (sortOn consName css `zip` [1 ..]) $ \(Constructor tycon fields, i) -> do
              let ts = llvmType <$> fields
                  con = llvmRep (name <> "__" <> tycon)
              td <-
                typedef con
                  (Just (StructureType False (i8 : ts)))
              f <-
                function con
                  (zip ts (repeat "a"))
                  (ptr t1)
                  (\args -> do 
                    op <- malloc td
                    storeAtOffs 0 op (int8 i)
                    forM_ (args `zip` [1 ..]) $ \(arg, j) -> 
                      storeAtOffs j op arg
                    bitcast op (ptr t1) >>= ret)
              pure [(tycon, f)]
          Function Signature {..} -> do
            pure 
              [ (name
              , functionRef
                  (llvmRep name)
                  (llvmType (fst body))
                  (llvmType . fst <$> arguments)) ]
          External sig -> do
            op <- emitExtern name sig
            pure [(name, op)]
          Constant _ -> pure []
    void <$$> forEachDef definitions $ \name ->
      \case 
        Function Signature {..} ->
          void $
          function
            (llvmRep name)
            (arguments <#> llvmType *** llvmRep)
            (llvmType (fst body))
            $ \ops -> do
                  let args = zip (arguments <#> snd) ops
                  flip runReaderT (Env.inserts args env) $ getCodeGen $ 
                    emitBody (snd body) >>=
                    ret
        _ ->
          pure ()

emitBody :: Ast -> CodeGen Operand
emitBody =
  cata $ \case
    ELit lit -> 
      emitLit lit
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
        Just (ConstantOperand (GlobalReference PointerType {..} _)) ->
          emitCall (t, name) []
        Just op -> pure op
        _ -> 
          error ("Not in scope: '" <> show name <> "'")
    ECase expr cs -> emitCase expr (sortOn fst cs)
    ECall () fun args -> emitCall fun args

emitCall :: Label Type -> [CodeGen Operand] -> CodeGen Operand
emitCall (t, fun) args = do
  as <- sequence args
  Env.askLookup fun >>= \case
    Just op@(LocalReference PointerType {..} _) -> do
      undefined
    Just op -> do
      let tys = argTypes t
          (ts1, ts2) = splitAt (length args) (llvmType <$> tys)
      case compare (arity t) (length args) of
        GT -> error "Implementation error"
        EQ -> do
          call op =<< foo as tys
          -- TODO
          --r <- call op =<< foo as tys
          --pure r
        _ -> mdo 
          let sty = StructureType False (LLVM.typeOf g : LLVM.typeOf op : ts1)
          name <- freshUnName
          g <- 
            function
              name
              (zip (charPtr : ts2) (repeat "a"))
              (llvmType (returnTypeOf t))
              (\(v:vs) -> do
                s <- bitcast v (ptr sty)
                p1 <- gep s [int32 0, int32 1]
                h <- load p1 0
                an <-
                  forM [2 .. length ts1 + 1] $ \n -> do
                    q <- gep s [int32 0, int32 (fromIntegral n)]
                    load q 0
                r <- call h (zip (an <> vs) (repeat []))
                ret r)
          s <- malloc sty
          storeAtOffs 0 s g
          storeAtOffs 1 s op
          forM_ (zip as [2 ..]) $ \(a, i) -> 
            storeAtOffs i s a
          bitcast s (ptr (StructureType False [LLVM.typeOf g]))
    _ -> error ("Function not in scope: '" <> show fun <> "'")

foo :: [Operand] -> [Type] -> CodeGen [(Operand, [ParameterAttribute])]
foo ops tys = traverse foo2 (zip ops tys)

foo2 :: (Operand, Type) -> CodeGen (Operand, [ParameterAttribute])
foo2 (op, ty) = do
  pure (op, [])

emitCase :: CodeGen Operand -> [([Label t], CodeGen Operand)] -> CodeGen Operand
emitCase expr cs = do
  blocks <-
    forM cs $ \(con:fields, body) -> do
      blk <- block `named` "case"
      r <-
        if null fields
          then body
          else do
            Env env <- ask
            undefined
      undefined
  undefined
  end <- block `named` "end"
  phi (blocks <#> fst)

--emitBody :: Body -> CodeGen Operand
--emitBody =
--  cata $ \case
----    BCase expr cs -- /
----     -> emitCase expr (sortOn fst cs)
----    BCall fun args -> emitCall fun args
--
--emitCall :: Name -> [CodeGen Operand] -> CodeGen Operand
--emitCall fun args = do
--  as <- sequence args
--  find fun >>= \case
--    Just op@(LocalReference PointerType {..} _) -> do
--      a <- bitcast op charPtr
--      p <- gep op [int32 0, int32 0]
--      h <- load p 0
--      call h (zip (a : as) (repeat []))
--    Just op -> do
--      let ts = argTypes op
--          (ts1, ts2) = splitAt (length args) ts
--      case compare (length ts) (length args) of
--        EQ -> call op (as `zip` repeat [])
--        _ ->
--          mdo 
--          let sTy =
--                    StructureType False (LLVM.typeOf g : LLVM.typeOf op : ts1)
--              --name <- freshName "anon"
--              name <- freshUnName
--              g <-
--                function
--                  name
--                  (zip (charPtr : ts2) (repeat "a"))
--                  (retType op)
--                  (\(v:vs) -> do
--                     s <- bitcast v (ptr sTy)
--                     p1 <- gep s [int32 0, int32 1]
--                     h <- load p1 0
--                     an <-
--                       forM [2 .. length ts1 + 1] $ \n -> do
--                         q <- gep s [int32 0, int32 (fromIntegral n)]
--                         load q 0
--                     r <- call h (zip (an <> vs) (repeat []))
--                     ret r)
--              s <- alloca sTy Nothing 0
--              p0 <- gep s [int32 0, int32 0]
--              p1 <- gep s [int32 0, int32 1]
--              store p0 0 g
--              store p1 0 op
--              forM_ (zip as [2 ..]) $ \(a, i) -> do
--                an <- gep s [int32 0, int32 (fromIntegral i)]
--                store an 0 a
--              bitcast s (ptr (StructureType False [LLVM.typeOf g]))
--    _ -> error ("Function not in scope: '" <> show fun <> "'")
--
--funType :: Operand -> LLVM.Type
--funType =
--  \case
--    ConstantOperand (GlobalReference PointerType {..} _) -> pointerReferent
--    LocalReference functionType _ -> functionType
--
--argTypes :: Operand -> [LLVM.Type]
--argTypes = argumentTypes <<< funType
--
--retType :: Operand -> LLVM.Type
--retType = resultType <<< funType

--emitCase :: CodeGen Operand -> [(Names, CodeGen Operand)] -> CodeGen Operand
--emitCase expr cs =
--  mdo dt <- expr
--      a0 <- gep dt [int32 0, int32 0]
--      m <- load a0 0
--      let names = blocks <#> snd
--      switch
--        m
--        (last names)
--        [ (Int 8 (fromIntegral i), block)
--        | (i, block) <- zip [1 ..] (init names)
--        ]
--      blocks <-
--        forM cs $ \(con:fields, body) -> do
--          blk <- block `named` "case"
--          r <-
--            if null fields
--              then body
--              else do
--                Env env <- ask
--                let argTys = argTypes (env ! con)
--                dn <- bitcast dt (ptr (StructureType False (i8 : argTys)))
--                ops <-
--                  forM [1 .. length argTys] $ \i -> do
--                    pi <- gep dn [int32 0, int32 (fromIntegral i)]
--                    load pi 0
--                local (Env.inserts (zip fields ops)) body
--          br end
--          cb <- currentBlock
--          pure ((r, cb), blk)
--      end <- block `named` "end"
--      phi (blocks <#> fst--)

emitExtern :: Name -> Signature a -> ModuleBuilder Operand
emitExtern name Signature{ .. } =
  extern
    (llvmRep name)
    (llvmType . fst <$> arguments)
    (llvmType (fst body))

malloc :: (MonadIRBuilder m) => LLVM.Type -> m Operand
malloc op = do
  size <- zext (ConstantOperand (sizeof op)) i64
  m <- allocate size
  bitcast m (ptr op)
    where
      allocate size = call (functionRef "gc_malloc" charPtr [i64]) [(size, [])]

storeAtOffs :: (MonadModuleBuilder m, MonadIRBuilder m) => Integer -> Operand -> Operand -> m ()
storeAtOffs offs op val = do
  p <- gep op [int32 0, int32 offs]
  store p 0 val
