{-# LANGUAGE OverloadedStrings #-}

module Pong.LLVM.EmitTests where

import Pong.TestData.TheFatalAuction
import Pong.TestData.ThePanamaHat
import Pong.TestHelpers
import Pong.Tree
import System.Exit
import System.IO.Unsafe
import Test.Hspec

llvmEmitTests :: SpecWith ()
llvmEmitTests =
  describe "Pong.LLVM.Emit" $ do
    describe "- emitModule" $ do
      let runTest msg prog res = do
            r <- runIO $ emitModule $ compileSource prog
            passIt msg (res == r)
      -------------------------------------------------------------------------
      runTest "1" program50 (ExitSuccess, "5")
      -------------------------------------------------------------------------
      runTest "2" program51 (ExitSuccess, "7")
      -------------------------------------------------------------------------
      runTest "3" program52 (ExitSuccess, "9")
      -------------------------------------------------------------------------
      passIt "4" (unsafePerformIO (emitModule (transformProgram program302)) == (ExitSuccess, "401"))
      -------------------------------------------------------------------------
      passIt "5" (unsafePerformIO (emitModule (transformProgram program306)) == (ExitSuccess, "402"))
      -------------------------------------------------------------------------
      passIt "6" (unsafePerformIO (emitModule (compileSource program400)) == (ExitSuccess, "401"))
      -------------------------------------------------------------------------
      passIt "7" (unsafePerformIO (emitModule (compileSource program500)) == (ExitSuccess, "402"))
      -------------------------------------------------------------------------
      passIt "8" (unsafePerformIO (emitModule (compileSource program501)) == (ExitSuccess, "4"))
      -------------------------------------------------------------------------
      passIt "9" (unsafePerformIO (emitModule (compileSource program600)) == (ExitSuccess, "1"))
      -------------------------------------------------------------------------
      passIt "10" (unsafePerformIO (emitModule (compileSource program601)) == (ExitSuccess, "2"))
