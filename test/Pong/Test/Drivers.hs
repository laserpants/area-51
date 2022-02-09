{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Pong.Test.Drivers where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (gets, modify, put)
import Data.Either (fromRight)
import Data.Map.Strict ((!))
import Data.Tuple.Extra (snd3)
import Data.Void
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
import GHC.IO.Handle.Types

type TestCase input result = String -> input -> result -> SpecWith ()

iso :: (Ord a, Eq a) => [a] -> [a] -> Bool
iso xs ys = Set.fromList xs == Set.fromList ys

runFreeTest :: (Eq t) => String -> Expr t a0 a1 a2 a3 -> [Label t] -> SpecWith ()
runFreeTest description input expected = it description $ free input == expected

runUnwindTypeTest :: TestCase Type [Type]
runUnwindTypeTest description input expected =
  it description $ unwindType input == expected

runFoldTypeTest :: TestCase (Type, [Type]) Type
runFoldTypeTest description (t, ts) expected =
  it description $ foldType t ts == expected

runTypeOfTest :: (Typed a) => TestCase a Type
runTypeOfTest description input expected =
  it description $ typeOf input == expected

runArityTest :: (HasArity a) => TestCase a Int
runArityTest description input expected =
  it description $ arity input == expected

runIsTConTest :: TestCase (TCon, Type) Bool
runIsTConTest description (con, expr) expected =
  it description $ isTCon con expr == expected

runIsConTest :: TestCase (Con, Expr t a0 a1 a2 a3) Bool
runIsConTest description (con, expr) expected =
  it description $ isCon con expr == expected

runReturnTypeOfTest :: (Typed a) => TestCase a Type
runReturnTypeOfTest description input expected =
  it description $ returnTypeOf input == expected

runTypeCheckerTest :: TestCase (Expr t () () () Void, TypeEnv) (Either TypeError (Expr Type () () () Void))
runTypeCheckerTest description (input, env) expected =
  it description $ runCheck env input == expected

runConvertLetBindingsTest :: TestCase (Expr Type () () () Void) PreAst
runConvertLetBindingsTest description input expected =
  it description $ convertLetBindings input == expected

runCombineLambdasTest :: TestCase (Expr Type () () () Void) (Expr Type () () () Void)
runCombineLambdasTest description input expected =
  it description $ combineLambdas input == expected

runConvertClosuresTest :: TestCase PreAst (Expr Type Void () () Void)
runConvertClosuresTest description input expected =
  it description $ runReader (convertClosures input) mempty == expected

runModifyFunDefsTest1 :: TestCase Program [Name]
runModifyFunDefsTest1 description input expected =
  it description $
  expected `iso`
  evalCompiler
    (do put input
        mapDefinitionsM
          (\s -> do
             modify (insertDefinition "new" (Constant (LInt32 5)))
             pure s)
        programNames)
    mempty

runModifyFunDefsTest2 :: TestCase Program Ast
runModifyFunDefsTest2 description input expected =
  it description $
  expected ==
  evalCompiler
    (do put input
        mapDefinitionsM
          (\case
             Function (Signature _ (ty, _)) ->
               pure (Function (Signature [] (ty, lit (LInt32 1))))
             d -> pure d)
        defs <- gets definitions
        let Function Signature {..} = defs ! "foo"
        pure (snd body))
    mempty

runUniqueNameTest :: String -> SpecWith ()
runUniqueNameTest description =
  it description $
  flip evalCompiler mempty $ do
    a <- uniqueName "foo"
    b <- uniqueName "foo"
    pure (a /= b)

-- --runCompileAstessionTest1 :: TestCase (Expr (), TypeEnv) Type
-- --runCompileAstessionTest1 description (input, env) expected = do
-- --  it description $
-- --    typeOf (definitions (execCompiler body env) ! "def_0") == expected
-- --  where
-- --    body = do
-- --      e <- typeCheck input
-- --      compileAst (fromRight (error "Implementation error") e)

runFillParamsTest :: TestCase (SourceExpr t, TypeEnv) Type
runFillParamsTest description (input, env) expected =
  it description $ t == expected
  where
    Function (Signature _ (t, _)) =
      definitions (execCompiler body env) ! "def_0"
    body = do
      e <- fromRight (error "Implementation error") <$> typeCheck input
      f <- preprocess e
      compileAst f
      mapDefinitionsM fillParams

-- --runCompileProgramTest ::
-- --     TestCase [(Name, Definition (Expr ()))] [(Name, Definition Body)]
-- --runCompileProgramTest description input expected =
-- --  it description $ definitions (compileProgram input) == Map.fromList expected
-- --
runLlvmTypeTest :: TestCase Type LLVM.Type
runLlvmTypeTest description input expected =
  it description $ llvmType input == expected

runX :: [(Name, Definition (SourceExpr ()))] -> IO String
runX definitions = 
  flip runContT id $ do
    (file, _) <- ContT (withTempFile "." "obj" . curry)
    context <- ContT withContext
    module_ <- ContT (withModuleFromAST context (buildProgram "Main" prog))
    machine <- ContT withHostTargetMachineDefault
    liftIO $ do
      writeObjectToFile machine (File file) module_
      callProcess "clang" ["-o", ".build/test-exec", "memory.c", file, "-lgc"]  
      pure (snd3 <$> readProcessWithExitCode ".build/test-exec" [] [])
  where
    prog = execCompiler (compileSource definitions) (getEnv definitions)
