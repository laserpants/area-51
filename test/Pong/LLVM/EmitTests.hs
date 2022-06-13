{-# LANGUAGE OverloadedStrings #-}

module Pong.LLVM.EmitTests where

import Control.Arrow ((<<<), (>>>))
import Pong.LLVM.Emit
import Pong.Read
import Pong.TestData.JackOfClubs
import Pong.TestData.TheFatalAuction
import Pong.TestHelpers
import Pong.Tree
import System.Exit
import Test.Hspec

llvmEmitTests :: SpecWith ()
llvmEmitTests =
  describe "Pong.LLVM.Emit" $ do
    describe "- buildProgram" $ do
      let runTest =
            runIO <<< compileModule <<< compileSource
      -------------------------------------------------------------------------
      r <- runTest program50
      it "1" ((ExitSuccess, "5") == r)
      -------------------------------------------------------------------------
      r <- runTest program51
      it "2" ((ExitSuccess, "7") == r)
      -------------------------------------------------------------------------
      r <- runTest program52
      it "2" ((ExitSuccess, "9") == r)
