{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Pong.Test.Drivers where

import Control.Monad.Writer
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (State, gets, modify, put, evalStateT)
import Data.Either (fromRight)
import Data.Map.Strict ((!))
import Data.Tuple.Extra (snd3, second)
import Data.Void
import Debug.Trace
import LLVM.Context (Context, withContext)
import LLVM.Module (File(..), withModuleFromAST, writeObjectToFile)
import LLVM.Target (withHostTargetMachineDefault, withTargetOptions)
import Pong.Compiler
import Pong.Data
import Pong.LLVM.Emit
import Pong.Lang
import Pong.TypeChecker
import Pong.Util
import System.IO.Temp
import System.Process
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified LLVM.AST.Type as LLVM
import qualified Pong.Util.Env as Env

-- let 
--   plus3 = 
--     lam(x, y, z) => x + y + z
--   in
--     let
--       f = 
--         plus3
--       in
--         let 
--           g =
--             f(5)
--           in
--             let
--               h =
--                 g(6)
--               in
--                 h(7)

-- 
-- plus3(x, y, z) = x + y + z
--
-- let
--   f = 
--     plus3
--   in
--     let 
--       g =
--         (\x y -> f(5, x, y))
--       in
--         let
--           h =
--             (\x -> g(6, x))
--           in
--             h(7)

--class B a where
--  gork :: [(Name, Name)] -> a -> a
--
--instance B (Expr Type Type () a2) where
--  gork = undefined
--
--instance B (Definition (Label Type) (Expr Type Type () a2)) where
--  gork = undefined
--
--instance B a => B [a] where
--  gork = undefined

type TestCase input result = String -> input -> result -> SpecWith ()

--xx1 :: Expr Type Type () a2 -> (Expr Type Type () a2, [(Name, Definition (Label Type) (Expr Type Type () a2))])
--xx1 input = runWriter (evalStateT (liftLambdas input) 0)
--
--iso :: (Ord a, Eq a) => [a] -> [a] -> Bool
--iso xs ys = Set.fromList xs == Set.fromList ys
--
--runFreeTest :: (Eq t) => String -> Expr t a0 a1 a2 a3 -> [Label t] -> SpecWith ()
--runFreeTest description input expected = it description $ free input == expected
--
--runUnwindTypeTest :: TestCase Type [Type]
--runUnwindTypeTest description input expected =
--  it description $ unwindType input == expected
--
--runFoldTypeTest :: TestCase (Type, [Type]) Type
--runFoldTypeTest description (t, ts) expected =
--  it description $ foldType t ts == expected
--
--runTypeOfTest :: (Typed a) => TestCase a Type
--runTypeOfTest description input expected =
--  it description $ typeOf input == expected
--
--runArityTest :: (HasArity a) => TestCase a Int
--runArityTest description input expected =
--  it description $ arity input == expected
--
--runIsTConTest :: TestCase (TCon, Type) Bool
--runIsTConTest description (con, expr) expected =
--  it description $ isTCon con expr == expected
--
--runIsConTest :: TestCase (Con, Expr t a0 a1 a2 a3) Bool
--runIsConTest description (con, expr) expected =
--  it description $ isCon con expr == expected
--
--runReturnTypeOfTest :: (Typed a) => TestCase a Type
--runReturnTypeOfTest description input expected =
--  it description $ returnTypeOf input == expected
--
--runTypeCheckerTest :: TestCase (Expr t () () () Void, TypeEnv) (Either TypeError (Expr Type () () () Void))
--runTypeCheckerTest description (input, env) expected =
--  it description $ runCheck env input == expected
--
--runConvertLetBindingsTest :: TestCase (Expr Type () () () Void) PreAst
--runConvertLetBindingsTest description input expected =
--  it description $ convertLetBindings input == expected
--
--runCombineLambdasTest :: TestCase (Expr Type () () () Void) (Expr Type () () () Void)
--runCombineLambdasTest description input expected =
--  it description $ combineLambdas input == expected
--
--runConvertClosuresTest :: TestCase PreAst (Expr Type Void () () Void)
--runConvertClosuresTest description input expected =
--  it description $ runReader (convertClosures input) mempty == expected
--
--runModifyFunDefsTest1 :: TestCase Program [Name]
--runModifyFunDefsTest1 description input expected =
--  it description $
--  expected `iso`
--  evalCompiler
--    (do put input
--        mapDefinitionsM
--          (\s -> do
--             modify (insertDefinition "new" (Constant (LInt32 5)))
--             pure s)
--        programNames)
--    mempty
--
--runModifyFunDefsTest2 :: TestCase Program Ast
--runModifyFunDefsTest2 description input expected =
--  it description $
--  expected ==
--  evalCompiler
--    (do put input
--        mapDefinitionsM
--          (\case
--             Function (Signature _ (ty, _)) ->
--               pure (Function (Signature [] (ty, lit (LInt32 1))))
--             d -> pure d)
--        defs <- gets definitions
--        let Function Signature {..} = defs ! "foo"
--        pure (snd body))
--    mempty
--
--runUniqueNameTest :: String -> SpecWith ()
--runUniqueNameTest description =
--  it description $
--  flip evalCompiler mempty $ do
--    a <- uniqueName "foo"
--    b <- uniqueName "foo"
--    pure (a /= b)
--
--runCompileExpressionTest1 :: TestCase (SourceExpr t, TypeEnv) Type
--runCompileExpressionTest1 description (input, env) expected = 
--  it description $ do
--    typeOf (definitions (execCompiler body env) ! "def_0") == expected
--  where
--    body = do
--      e <- preprocess . fromRight (error "Implementation error") =<< typeCheck input
--      compileAst e
--
--runFillParamsTest :: TestCase (SourceExpr t, TypeEnv) Type
--runFillParamsTest description (input, env) expected =
--  it description $ t == expected
--  where
--    Function (Signature _ (t, _)) =
--      definitions (execCompiler body env) ! "def_0"
--    body = do
--      e <- preprocess . fromRight (error "Implementation error") =<< typeCheck input
--      compileAst e
--      mapDefinitionsM fillParams
--
--runCompileProgramTest :: TestCase [(Name, Definition TypedExpr)] [(Name, Definition Ast)]
--runCompileProgramTest description input expected =
--  it description $ definitions (toProgram input) == Map.fromList expected
--
--runLlvmTypeTest :: TestCase Type LLVM.Type
--runLlvmTypeTest description input expected =
--  it description $ llvmType input == expected
--
--runEndToEndCompilerTest :: TestCase [(Name, Definition (SourceExpr ()))] String 
--runEndToEndCompilerTest description definitions expected = do
--  result <- runIO $ flip runContT id $ do
--    (file, _) <- ContT (withTempFile "." "obj" . curry)
--    context <- ContT withContext
--    module_ <- ContT (withModuleFromAST context (buildProgram "Main" program))
--    machine <- ContT withHostTargetMachineDefault
--    liftIO $ do
--      writeObjectToFile machine (File file) module_
--      callProcess "clang" ["-o", ".build/test-exec", "memory.c", file, "-lgc"]  
--      pure (snd3 <$> readProcessWithExitCode ".build/test-exec" [] [])
--  it description $ result == expected
--    where
--  program = execCompiler (compileSource definitions) (getEnv definitions)
