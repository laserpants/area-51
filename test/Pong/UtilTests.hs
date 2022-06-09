{-# LANGUAGE OverloadedStrings #-}

module Pong.UtilTests where

import Control.Monad.State
import Pong.Read
import Pong.TestData.JackOfClubs
import Pong.Util
import Test.Hspec

utilTests :: SpecWith ()
utilTests =
  describe "Pong.Util" $ do
    describe "- without" $ do
      -------------------------------------------------------------------------
      it "1" ([1, 2, 3, 4, 5] `without` [2, 4, 6] == [1, 3, 5])
      -------------------------------------------------------------------------
      it "2" (null $ [] `without` [2, 4, 6])

    describe "- getAndModify" $ do
      -------------------------------------------------------------------------
      let prog = do
            put 1
            getAndModify (+ 1)

      it "1" (runState prog 0 == (1, 2))

    describe "- varSequence" $ do
      -------------------------------------------------------------------------
      it "1" (varSequence "foo" [1, 2, 3] == [(1, "foo0"), (2, "foo1"), (3, "foo2")])

    describe "- (<$$>)" $ do
      -------------------------------------------------------------------------
      it "1" (((+1) <$$> [((), 1), ((), 2), ((), 42)]) == [((),2),((),3),((),43)])

