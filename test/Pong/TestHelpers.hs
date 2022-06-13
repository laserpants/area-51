{-# LANGUAGE OverloadedStrings #-}

module Pong.TestHelpers where

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
import System.Process
import Test.Hspec
import Text.Megaparsec

typeCheck :: TypeChecker a -> Either TypeError a
typeCheck = evalTypeChecker 1 mempty

runUnify :: MonoType -> MonoType -> Either TypeError Substitution
runUnify t1 t2 = evalTypeChecker (freeIndex [t1, t2]) mempty (unifyTypes t1 t2)

runUnifyRows :: Row MonoType Int -> Row MonoType Int -> Either TypeError Substitution
runUnifyRows r1 r2 = evalTypeChecker (freeIndex [tRec r1, tRec r2]) mempty (unifyRows r1 r2)

runInferProgramWithEnv ::
  TypeEnv ->
  Program () SourceExpr ->
  Either TypeError (Program MonoType TypedExpr)
runInferProgramWithEnv env = runTypeChecker 1 env . inferProgram <&> fst

testHoistProgram :: Program MonoType TypedExpr -> Program MonoType TypedExpr
testHoistProgram p = programFor p (const hoistTopLambdas)

testMonomorphizeProgram :: Program MonoType TypedExpr -> Program MonoType TypedExpr
testMonomorphizeProgram p = runTransform (programForM p (const (traverse monomorphizeLets)))

{-# NOINLINE projectDir #-}
projectDir :: String
projectDir = unsafePerformIO getCurrentDirectory

unsafeReadFile :: String -> Text
unsafeReadFile = pack . unsafePerformIO . readFile

readProjectFile :: String -> Text
readProjectFile path = unsafeReadFile (projectDir <> "/" <> path)

{-# INLINE mainSig #-}
mainSig :: Label Scheme
mainSig = (Scheme (tUnit ~> tInt), "main")

runTestParser :: (Eq a) => Parser a -> Text -> a -> SpecWith ()
runTestParser parser input expect =
  it (unpack input) (runParser parser "" input == Right expect)

{-# INLINE passIt #-}
passIt :: Example a => String -> a -> SpecWith (Arg a)
passIt = it . ("OK ✔ " <>)

{-# INLINE failIt #-}
failIt :: Example a => String -> a -> SpecWith (Arg a)
failIt = it . ("OK ✗ " <>)

emitModule :: Program MonoType Ast -> IO (ExitCode, String)
emitModule prog = compileBinary >> exec
  where
    compileBinary = do
      let mdul = ppll (buildProgram "Main" prog)
          echo = proc "echo" [TextLazy.unpack mdul]
      (_, Just stdoutHandle, _, _) <- createProcess echo{std_out = CreatePipe}
      (_, _, _, procHandle) <-
        createProcess
          (proc "clang" ["memory.c", "-xir", "-lgc", "-Wno-override-module", "-o", "tmp/out", "-"])
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
