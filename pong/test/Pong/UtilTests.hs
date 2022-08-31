{-# LANGUAGE OverloadedStrings #-}

module Pong.UtilTests where

import Control.Monad.State (State, put, runState)
import Pong.Util
import Test.Hspec

utilTests :: SpecWith ()
utilTests =
  describe "Pong.Util" $ do
    describe "- without" $ do
      it "[1, 2, 3, 4, 5] `without` [2, 4, 6]" $
        let input, result :: [Int]
            input =
              [1, 2, 3, 4, 5]
            result =
              [1, 3, 5]
         in (input `without` [2, 4, 6] == result)

      it "[1, 2, 3, 4, 5] `without` []" $
        let input, result :: [Int]
            input =
              [1, 2, 3, 4, 5]
            result =
              [1, 2, 3, 4, 5]
         in (input `without` [] == result)

      it "[1, 2, 3, 4, 5] `without` [1, 1, 1]" $
        let input, result :: [Int]
            input =
              [1, 2, 3, 4, 5]
            result =
              [2, 3, 4, 5]
         in (input `without` [1, 1, 1] == result)

      it "[1, 1, 1, 1, 2, 4] `without` [1, 2, 3]" $
        let input, result :: [Int]
            input =
              [1, 1, 1, 1, 2, 4]
            result =
              [4]
         in (input `without` [1, 2, 3] == result)

      it "[] `without` [2, 4, 6]" $
        let input, result :: [Int]
            input =
              []
            result =
              []
         in (input `without` [2, 4, 6] == result)

    describe "- withoutLabels" $
      it "withoutLabels [\"a\", \"b\"] [(1, \"a\"), (2, \"b\"), (3, \"c\")]" $
        let input, result :: [(Int, Text)]
            input =
              [(1, "a"), (2, "b"), (3, "c")]
            result =
              [(3, "c")]
         in (withoutLabels ["a", "b"] input == result)

    describe "- getAndModify" $
      it "put 1 >> getAndModify succ" $
        let sample :: State Int Int
            sample = do
              put 1
              getAndModify succ
            result :: (Int, Int)
            result =
              (1, 2)
         in (runState sample 0 == result)

    describe "- varSequence" $
      it "varSequence \"foo\" [0, 1, 2]" $
        let input :: [Int]
            input =
              [0, 1, 2]
            result :: [(Int, Text)]
            result =
              [ (0, "foo0")
              , (1, "foo1")
              , (2, "foo2")
              ]
         in varSequence "foo" input == result

    describe "- (<$$>)" $
      it "succ <$$> [((), 1), ((), 2), ((), 42)]" $
        let input, result :: [((), Int)]
            input =
              [ ((), 1)
              , ((), 2)
              , ((), 42)
              ]
            result =
              [ ((), 2)
              , ((), 3)
              , ((), 43)
              ]
         in ((succ <$$> input) == result)
