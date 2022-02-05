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

llvmFunType :: Operand -> LLVM.Type
llvmFunType =
  \case
    ConstantOperand (GlobalReference PointerType {..} _) -> pointerReferent
    LocalReference functionType _ -> functionType

llvmArgTypes :: Operand -> [LLVM.Type]
llvmArgTypes = argumentTypes <<< llvmFunType

llvmRetType :: Operand -> LLVM.Type
llvmRetType = resultType <<< llvmFunType

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

forEachDef :: (Monad m) => Map k a -> (k -> a -> m b) -> m [b]
forEachDef map = forM (Map.toList map) . uncurry 

buildProgram :: Name -> Program -> LLVM.Module
buildProgram mod Program {..} =
  buildModule (llvmRep mod) $ do
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
                    storeAtOffs 0 op (int8 i) -- constructor #
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
        --Function Signature {..} ->
        --  void $
        --  function
        --    (llvmRep name)
        --    (arguments <#> llvmType *** llvmRep)
        --    (llvmType (fst body))
        --    $ \ops -> do
        --          let args = zip (arguments <#> snd) ops
        --          flip runReaderT (Env.inserts args env) $ getCodeGen $ 
        --            emitBody (snd body) >>=
        --            ret
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
        Just op | isTCon ArrT t -> do
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
      a <- bitcast op charPtr
      h <- loadFromOffset 0 op
      -- TEMP
      --zz <- inttoptr (ConstantOperand (Int 32 123)) charPtr
      call h (zip (a : as) (repeat []))
      --pure zz
    Just op -> do
      let tys = argTypes t
          (ts1, ts2) = splitAt (length args) (llvmType <$> tys)
      as111 <- traverse downCast (zip3 as (llvmArgTypes op) tys)
      case compare (arity t) (length args) of
        EQ -> do
          r <- call op (zip as111 (repeat []))
          traceShowM "******************************"
          traceShowM "******************************"
          traceShowM r
          traceShowM op
          traceShowM (returnTypeOf t)
          upCast (r, llvmRetType op, returnTypeOf t)
          --zz <- inttoptr (ConstantOperand (Int 32 999)) charPtr
          --pure zz
          -- TODO
        GT -> mdo 
         let sty = StructureType False (LLVM.typeOf g : LLVM.typeOf op : ts1)
         name <- freshName "fun"
         g <- 
           function
             name
             (zip (charPtr : ts2) (repeat "a"))
             (llvmRetType op)
             (\(v:vs) -> do
               s <- bitcast v (ptr sty)
               h <- loadFromOffset 1 s
               an <-
                 forM [2 .. length ts1 + 1] $ \n -> 
                   loadFromOffset (fromIntegral n) s
               r <- call h (zip (an <> vs) (repeat []))
               -- TEMP
               --zz <- inttoptr (ConstantOperand (Int 32 456)) charPtr
               --q <- upCast (r, llvmRetType op, returnTypeOf t)
               --upCast undefined -- (r, llvmRetType op, returnTypeOf t)
               ret r
               )
               -- ret r)
         struct <- malloc sty
         storeAtOffs 0 struct g
         storeAtOffs 1 struct op
         forM_ (as `zip` [2 ..]) $ \(a, i) -> 
           storeAtOffs i struct a
         bitcast struct (ptr (StructureType False [LLVM.typeOf g]))
        LT -> error "Implementation error"
    _ -> error ("Function not in scope: '" <> show fun <> "'")

downCast :: (MonadIRBuilder m) => (Operand, LLVM.Type, Type) -> m Operand
downCast = \case
  (op, llvmt, t) | charPtr == llvmt && (t `elem` [tInt32, tInt64]) -> do
    inttoptr op charPtr 
  (op, _, _) -> pure op

upCast :: (MonadIRBuilder m) => (Operand, LLVM.Type, Type) -> m Operand
upCast = \case
  (op, llvmt, t) | charPtr == llvmt && (t `elem` [tInt32, tInt64]) -> do
    ptrtoint op (llvmType t)
  (op, _, _) -> pure op

emitCase :: CodeGen Operand -> [([Label Type], CodeGen Operand)] -> CodeGen Operand
emitCase expr cs = mdo 
  dt <- expr
  m <- loadFromOffset 0 dt
  let names = blocks <#> snd
  switch
    m
    (last names)  -- Default case
    [ (Int 8 (fromIntegral i), block)
    | (i, block) <- zip [1 ..] (init names)
    ]
  blocks <-
    forM cs $ \(con:fields, body) -> do
      blk <- block `named` "case"
      r <-
        if null fields
          then body
          else do
            Env env <- ask
            let argTys = argTypes (fst con)
                llvmTys = llvmArgTypes (env ! snd con)
            dn <- bitcast dt (ptr (StructureType False (i8 : llvmTys)))
            ops <-
              forM (zip3 [1..] llvmTys argTys) $ \(i, llvmt, t) -> do
                op <- loadFromOffset i dn
                upCast (op, llvmt, t)
            local (Env.inserts (zip (fields <#> snd) ops)) body
      br end
      cb <- currentBlock
      pure ((r, cb), blk)
  end <- block `named` "end"
  phi (blocks <#> fst)

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

loadFromOffset :: (MonadModuleBuilder m, MonadIRBuilder m) => Integer -> Operand -> m Operand
loadFromOffset offs op = do
  p <- gep op [int32 0, int32 offs]
  load p 0
