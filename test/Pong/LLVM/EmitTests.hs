{-# LANGUAGE OverloadedStrings #-}

module Pong.LLVM.EmitTests where

import Pong.Read
import Pong.TestData.JackOfClubs
import Test.Hspec

llvmEmitTests :: SpecWith ()
llvmEmitTests =
  describe "Pong.LLVM.Emit" $ do
    pure ()
