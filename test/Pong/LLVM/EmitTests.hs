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
      it "4" (unsafePerformIO (emitModule (transformProgram program302)) == (ExitSuccess, "401"))
