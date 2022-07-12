{-# LANGUAGE OverloadedStrings #-}

module Pong.TestHelpers where

import Control.Monad ((>=>))
import Data.Either.Extra (mapLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TextLazy
import GHC.IO.Handle
import LLVM.Pretty
import Pong.Data
import Pong.LLVM.Emit
import Pong.Lang
import Pong.Read
import Pong.Tree
import Pong.Type
import Pong.Util
import System.Directory
import System.Exit
import System.IO.Unsafe
import System.Process hiding (env)
import Test.Hspec
import Text.Megaparsec

typeCheck :: TypeChecker a -> Either TypeError a
typeCheck = evalTypeChecker 1 mempty

runUnify :: MonoType -> MonoType -> Either TypeError Substitution
runUnify t1 t2 = evalTypeChecker (freeIndex [t1, t2]) mempty (unifyTypes t1 t2)

runUnifyRows :: MonoType -> MonoType -> Either TypeError Substitution
runUnifyRows r1 r2 = evalTypeChecker (freeIndex [tRec r1, tRec r2]) mempty (unifyRows r1 r2)

lookupDef :: (Scheme, Name) -> Module t a -> Maybe (Definition t a)
lookupDef defn (Module p) = Map.lookup defn p

runInferModuleWithEnv ::
  TypeEnv ->
  Module () SourceExpr ->
  Either TypeError (Module MonoType TypedExpr)
runInferModuleWithEnv env = runTypeChecker 1 env . inferModule <&> fst

parseAndAnnotateWithEnv ::
  TypeEnv ->
  Text ->
  Either CompilerError (Module MonoType TypedExpr)
parseAndAnnotateWithEnv env =
  mapLeft ParserError . parseModule
    >=> mapLeft TypeError . runInferModuleWithEnv env

compileSourceWithEnv :: TypeEnv -> Text -> Module MonoType Ast
compileSourceWithEnv env input =
  case parseAndAnnotateWithEnv env input of
    Left e -> error (show e)
    Right p -> transformModule p

testHoistModule :: Module MonoType TypedExpr -> Module MonoType TypedExpr
testHoistModule p = moduleFor p (const hoistTopLambdas)

testMonomorphizeModule :: Module MonoType TypedExpr -> Module MonoType TypedExpr
testMonomorphizeModule p = runTransform (moduleForM p (const (traverse monomorphizeLets)))

{-# NOINLINE projectDir #-}
projectDir :: String
projectDir = unsafePerformIO getCurrentDirectory

-- unsafeReadFile :: String -> Text
-- unsafeReadFile = pack . unsafePerformIO . readFile
--
-- readProjectFile :: String -> Text
-- readProjectFile path = unsafeReadFile (projectDir <> "/" <> path)

{-# INLINE mainSig #-}
mainSig :: Label Scheme
mainSig = (Scheme (tUnit ~> tInt), "main")

runTestParser :: (Eq a) => Parser a -> Text -> a -> SpecWith ()
runTestParser parser input expect =
  it (unpack input) (runParser parser "" input == Right expect)

runTestParser_ :: (Eq a) => Parser a -> Text -> Either ParserError a -> Bool
runTestParser_ parser input expect = runParser parser "" input == expect

{-# INLINE passIt #-}
passIt :: Example a => String -> a -> SpecWith (Arg a)
passIt = it . ("OK ✔ " <>)

{-# INLINE failIt #-}
failIt :: Example a => String -> a -> SpecWith (Arg a)
failIt = it . ("OK ✗ " <>)

emitModule :: Module MonoType Ast -> IO (ExitCode, String)
emitModule prog = compileBinary >> exec
  where
    compileBinary = do
      let mdul = ppll (buildModule_ "Main" prog)
          echo = proc "echo" [TextLazy.unpack mdul]
      (_, Just stdoutHandle, _, _) <- createProcess echo{std_out = CreatePipe}
      (_, _, _, procHandle) <-
        createProcess
          (proc "clang" ["runtime.c", "-O0", "-g", "-xir", "-lgc", "-Wno-override-module", "-o", "tmp/out", "-"])
            { std_in = UseHandle stdoutHandle
            , cwd = Just projectDir
            }
      waitForProcess procHandle
    exec = do
      (_, Just stdoutHandle, _, procHandle) <-
        createProcess
          (proc "tmp/out" [])
            { cwd = Just projectDir
            , std_out = CreatePipe
            }
      outp <- hGetContents stdoutHandle
      exc <- waitForProcess procHandle
      pure (exc, takeWhile (/= '\n') outp)
