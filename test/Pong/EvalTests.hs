{-# LANGUAGE OverloadedStrings #-}

module Pong.EvalTests where

import Data.Either.Extra (fromRight)
import Pong.Data
import Pong.Eval
import Pong.Lang
import Pong.Read
import Pong.TestData.GoAwayDixieGillian
import Pong.TestData.JackOfClubs
import Pong.TestData.MysteriousSetOfBooks
import Pong.TestData.ShirtMixUpAtTheLaundry
import Pong.TestHelpers
import Pong.Tree
import Pong.Util
import Test.Hspec

evalTests :: SpecWith ()
evalTests =
  describe "Pong.Eval" $ do
    describe "- evalProgram" $ do
      -------------------------------------------------------------------------
      it "1" (Just (PrimValue (PInt 100)) == evalProgram (transformProgram program1) mainSig)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program =
            transformProgram (fromRight emptyProgram (parseAndAnnotate program2))

      it "2" (Just (PrimValue (PInt 5)) == evalProgram program mainSig)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program =
            transformProgram (fromRight emptyProgram (parseAndAnnotate program3))

      it "3" (Just (PrimValue (PInt 1)) == evalProgram program mainSig)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSource program4

      it "4" (Just (PrimValue (PInt 14)) == evalProgram program mainSig)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSource program20

      it "5" (Just (PrimValue (PInt 100)) == evalProgram program mainSig)
