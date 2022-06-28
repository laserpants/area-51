{-# LANGUAGE OverloadedStrings #-}

module Pong.LLVM.EmitTests where

import Pong.TestData.MysteriousSetOfBooks
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
      -------------------------------------------------------------------------
      passIt "11" (unsafePerformIO (emitModule (compileSource program602)) == (ExitSuccess, "50"))
      -------------------------------------------------------------------------
      passIt "12" (unsafePerformIO (emitModule (compileSource program603)) == (ExitSuccess, "121"))
      -------------------------------------------------------------------------
      passIt "13" (unsafePerformIO (emitModule (compileSource program604)) == (ExitSuccess, "6"))
      -------------------------------------------------------------------------
      passIt "14" (unsafePerformIO (emitModule (compileSource program20)) == (ExitFailure 100, ""))
      -------------------------------------------------------------------------
      passIt "15" (unsafePerformIO (emitModule (compileSource program24)) == (ExitFailure 5, ""))
      -------------------------------------------------------------------------
      passIt "16" (unsafePerformIO (emitModule (compileSource program25)) == (ExitFailure 10, ""))
      -------------------------------------------------------------------------
      passIt "17" (unsafePerformIO (emitModule (compileSource program26)) == (ExitFailure 99, ""))
      -------------------------------------------------------------------------
      passIt "18" (unsafePerformIO (emitModule (compileSource program27)) == (ExitFailure 2, ""))
      -------------------------------------------------------------------------
      passIt "19" (unsafePerformIO (emitModule (compileSource program28)) == (ExitFailure 1, ""))
      -------------------------------------------------------------------------
      passIt "20" (unsafePerformIO (emitModule (compileSource program29)) == (ExitFailure 1, ""))
      -------------------------------------------------------------------------
      passIt "21" (unsafePerformIO (emitModule (compileSource program30)) == (ExitFailure 1, ""))
      -------------------------------------------------------------------------
      passIt "22" (unsafePerformIO (emitModule (compileSource program31)) == (ExitFailure 2, ""))
      -------------------------------------------------------------------------
      passIt "23" (unsafePerformIO (emitModule (compileSource program32)) == (ExitFailure 1, ""))
      -------------------------------------------------------------------------
      passIt "24" (unsafePerformIO (emitModule (compileSource program33)) == (ExitFailure 111, ""))
      -------------------------------------------------------------------------
      passIt "25" (unsafePerformIO (emitModule (compileSource program37)) == (ExitFailure 2, ""))
      -------------------------------------------------------------------------
      passIt "26" (unsafePerformIO (emitModule (compileSource program38)) == (ExitFailure 111, ""))
