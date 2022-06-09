{-# LANGUAGE OverloadedStrings #-}

module Pong.ReadTests where

import Pong.Read
import Pong.TestData.JackOfClubs
import Test.Hspec

readTests :: SpecWith ()
readTests =
  describe "Pong.Read" $ do
    describe "- parseProgram" $ do
      -------------------------------------------------------------------------
      it "1" (parseProgram program4 == Right program5)
