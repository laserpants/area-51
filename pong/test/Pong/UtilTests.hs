{-# LANGUAGE OverloadedStrings #-}

module Pong.UtilTests where

import Control.Monad.State (put, runState)
import Pong.Util
import Test.Hspec

utilTests :: SpecWith ()
utilTests =
  describe "Pong.Util" $ do
    describe "- without" $ do
      -------------------------------------------------------------------------
      it "1" ([1, 2, 3, 4, 5] `without` [2 :: Int, 4, 6] == [1, 3, 5])
      -------------------------------------------------------------------------
      it "2" (null $ [] `without` [2 :: Int, 4, 6])

    describe "- getAndModify" $ do
      -------------------------------------------------------------------------
      let prog = do
            put 1
            getAndModify (+ 1)

      it "1" (runState prog 0 == (1 :: Int, 2))

    describe "- varSequence" $ do
      -------------------------------------------------------------------------
      it "1" $
        let result =
              [ (0, "foo0")
              , (1, "foo1")
              , (2, "foo2")
              ]
         in varSequence "foo" [0 :: Int, 1, 2] == result

    describe "- (<$$>)" $ do
      -------------------------------------------------------------------------
      it "1" $
        let result =
              [ ((), 2)
              , ((), 3)
              , ((), 43)
              ]
         in (((+ 1) <$$> [((), 1 :: Int), ((), 2), ((), 42)]) == result)
