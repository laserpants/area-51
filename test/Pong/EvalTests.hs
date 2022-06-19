{-# LANGUAGE OverloadedStrings #-}

module Pong.EvalTests where

import Data.Either.Extra (fromRight)
import Pong.Data
import Pong.Eval
import Pong.Lang
import Pong.TestData.GoAwayDixieGillian
import Pong.TestData.JackOfClubs
import Pong.TestData.MysteriousSetOfBooks
import Pong.TestData.ShirtMixUpAtTheLaundry
import Pong.TestData.TheFatalAuction
import Pong.TestData.ThePanamaHat
import Pong.TestHelpers
import Pong.Tree
import qualified Pong.Util.Env as Env
import Test.Hspec

evalTests :: SpecWith ()
evalTests =
  describe "Pong.Eval" $ do
    describe "- evalProgram" $ do
      -------------------------------------------------------------------------
      do
        v <- runIO $ evalProgram (transformProgram program1) mainSig
        it "1" (Just (PrimValue (PInt 100)) == v)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program =
            transformProgram (fromRight emptyProgram (parseAndAnnotate program2))
       in do
            v <- runIO $ evalProgram program mainSig
            it "2" (Just (PrimValue (PInt 5)) == v)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program =
            transformProgram (fromRight emptyProgram (parseAndAnnotate program3))
       in do
            v <- runIO $ evalProgram program mainSig
            it "3" (Just (PrimValue (PInt 1)) == v)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSource program4
       in do
            v <- runIO $ evalProgram program mainSig
            it "4" (Just (PrimValue (PInt 14)) == v)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSource program20
       in do
            v <- runIO $ evalProgram program mainSig
            it "5" (Just (PrimValue (PInt 100)) == v)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSource program21
       in do
            v <- runIO $ evalProgram program mainSig
            it "6" (Just (PrimValue (PInt 101)) == v)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSource program22
       in do
            v <- runIO $ evalProgram program mainSig
            it "7" (Just (PrimValue (PInt 102)) == v)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSource program23
       in do
            v <- runIO $ evalProgram program mainSig
            it "8" (Just (PrimValue (PInt 1)) == v)
      --      -------------------------------------------------------------------------
      --      let program :: Program MonoType Ast
      --          program = compileSource program44
      --       in do
      --            v <- runIO $ evalProgram program mainSig
      --            it "9" (Just (PrimValue (PInt 4)) == v)
      --      -------------------------------------------------------------------------
      --      let program :: Program MonoType Ast
      --          program = compileSource program46
      --       in do
      --            v <- runIO $ evalProgram program mainSig
      --            it "10" (Just (PrimValue (PInt 5)) == v)
      --      -------------------------------------------------------------------------
      --      let program :: Program MonoType Ast
      --          program = compileSource program48
      --       in do
      --            v <- runIO $ evalProgram program mainSig
      --            it "11" (Just (PrimValue (PInt 5)) == v)
      -------------------------------------------------------------------------
      do
        v <- runIO $ evalProgram program217 mainSig
        it "12" (Just (PrimValue (PInt 401)) == v)
      -------------------------------------------------------------------------
      do
        v <- runIO $ evalProgram program271 mainSig
        it "13" (Just (PrimValue (PInt 5)) == v)
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program = compileSourceWithEnv env program212
          env =
            Env.fromList
              [
                ( "Nil"
                , Right (Scheme (tCon "List" [tVar "a"]))
                )
              ,
                ( "Cons"
                , Right (Scheme (tVar "a" ~> tCon "List" [tVar "a"] ~> tCon "List" [tVar "a"]))
                )
              ]
       in do
            v <- runIO $ evalProgram program mainSig
            it "14" (Just (PrimValue (PInt 4)) == v)
      -------------------------------------------------------------------------
      do
        v <- runIO $ evalProgram program217 mainSig
        it "15" (Just (PrimValue (PInt 401)) == v)
      -------------------------------------------------------------------------
      do
        v <- runIO $ runEval mempty (eval expr305)
        it "16" (PrimValue (PInt 401) == v)
      -------------------------------------------------------------------------
      do
        v <- runIO $ runEval mempty (eval expr306)
        it "17" (PrimValue (PInt 402) == v)
      -------------------------------------------------------------------------
      do
        v <- runIO $ evalProgram (compileProgram program305) mainSig
        it "18" (Just (PrimValue (PInt 402)) == v)
