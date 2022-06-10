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
import Pong.TestData.TheFatalAuction
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
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSource program21

      it "6" (Just (PrimValue (PInt 101)) == evalProgram program mainSig)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSource program22

      it "7" (Just (PrimValue (PInt 102)) == evalProgram program mainSig)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSource program23

      it "8" (Just (PrimValue (PInt 1)) == evalProgram program mainSig)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSource program44

      it "9" (Just (PrimValue (PInt 4)) == evalProgram program mainSig)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSource program46

      it "10" (Just (PrimValue (PInt 5)) == evalProgram program mainSig)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSource program48

      it "11" (Just (PrimValue (PInt 5)) == evalProgram program mainSig)
