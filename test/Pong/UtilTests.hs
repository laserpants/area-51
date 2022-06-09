{-# LANGUAGE OverloadedStrings #-}

module Pong.UtilTests where

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
      pure ()

    describe "- varSequence" $ do
      -------------------------------------------------------------------------
      it "1" (varSequence "foo" [1, 2, 3] == [(1, "foo0"), (2, "foo1"), (3, "foo2")])
