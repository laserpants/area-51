{-# LANGUAGE OverloadedStrings #-}

module Pong.TreeTests where

import Data.List.NonEmpty (fromList)
import Pong.Data
import Pong.Lang
import Pong.TestData.AnEnvelopeForJohnStJohn
import Pong.TestData.GeraniumPlant
import Pong.TestData.GoAwayDixieGillian
import Pong.TestData.JackOfClubs
import Pong.TestData.TheFatalAuction
import Pong.TestHelpers
import Pong.Tree
import Pong.Util
import Test.Hspec hiding (after, before)

treeTests :: SpecWith ()
treeTests =
  describe "Pong.Tree" $ do
    describe "- hoistTopLambdas" $ do
      -------------------------------------------------------------------------
      let def :: Definition MonoType TypedExpr
          def =
            Function
              (fromList [(tInt, "x")])
              (tInt ~> tInt, eLam () [(tInt, "y")] (eVar (tInt, "y")))

          result :: Definition MonoType TypedExpr
          result =
            Function
              (fromList [(tInt, "x"), (tInt, "y")])
              (tInt, eVar (tInt, "y"))
       in it "1" (hoistTopLambdas def == result)
      -------------------------------------------------------------------------
      it "2" (testHoistProgram program6 == program7)

    describe "- combineLambdas" $ do
      -------------------------------------------------------------------------
      let before :: TypedExpr
          before =
            eLam
              ()
              [(tInt, "a")]
              ( eLam
                  ()
                  [(tInt, "b")]
                  (eVar (tInt, "b"))
              )

          after :: TypedExpr
          after =
            eLam
              ()
              [(tInt, "a"), (tInt, "b")]
              (eVar (tInt, "b"))
       in it "1" (combineLambdas before == after)
      -------------------------------------------------------------------------
      let before :: TypedExpr
          before =
            eLam
              ()
              [(tInt, "x")]
              ( eLam
                  ()
                  [(tInt, "y")]
                  ( eLam
                      ()
                      [(tInt, "z")]
                      (eLit (PInt 5))
                  )
              )

          after :: TypedExpr
          after =
            eLam
              ()
              [(tInt, "x"), (tInt, "y"), (tInt, "z")]
              (eLit (PInt 5))
       in it "2" (combineLambdas before == after)
      -------------------------------------------------------------------------
      let before :: TypedExpr
          before =
            eLam
              ()
              [(tInt, "a")]
              ( eLam
                  ()
                  [(tInt, "b")]
                  ( eLam
                      ()
                      [(tInt, "c")]
                      ( eLet
                          (tInt ~> tInt ~> tInt, "f")
                          ( eLam
                              ()
                              [(tInt, "a")]
                              ( eLam
                                  ()
                                  [(tInt, "b")]
                                  (eVar (tInt, "a"))
                              )
                          )
                          ( eLam
                              ()
                              [(tInt, "a")]
                              ( eLam
                                  ()
                                  [(tInt, "b")]
                                  (eVar (tInt, "a"))
                              )
                          )
                      )
                  )
              )

          after :: TypedExpr
          after =
            eLam
              ()
              [(tInt, "a"), (tInt, "b"), (tInt, "c")]
              ( eLet
                  (tInt ~> tInt ~> tInt, "f")
                  (eLam () [(tInt, "a"), (tInt, "b")] (eVar (tInt, "a")))
                  (eLam () [(tInt, "a"), (tInt, "b")] (eVar (tInt, "a")))
              )
       in it "3" (combineLambdas before == after)

    describe "- parseAndAnnotate" $ do
      -------------------------------------------------------------------------
      let input :: Text
          input =
            "def main(a : unit) : int =\
            \  let\
            \    r =\
            \      { a = 100\
            \      , b = true\
            \      , c = 3\
            \      }\
            \    in\
            \      letr\
            \        { a = x | q } =\
            \          r\
            \        in\
            \          x\
            \"
          -- "

          program :: Program MonoType TypedExpr
          program =
            program1
       in it "1" (parseAndAnnotate input == Right program)

    describe "- canonical" $ do
      -------------------------------------------------------------------------
      it "1" (expr1 /= expr2)
      -------------------------------------------------------------------------
      it "2" (canonical expr1 == expr2)
      -------------------------------------------------------------------------
      it "3" (canonical expr1 == canonical expr2)
      -------------------------------------------------------------------------
      it "4" (expr1 `isIsomorphicTo` expr2)

    describe "- monomorphizeLets" $ do
      -------------------------------------------------------------------------
      it "1" (testMonomorphizeProgram program6 == program8)
      -------------------------------------------------------------------------
      it "2" (transform1 program6 == program9)

    describe "- compileProgram" $ do
      -------------------------------------------------------------------------
      it "1" (compileProgram program8 == program10)
      -------------------------------------------------------------------------
      it "2" (compileProgram fragment6 == fragment7)
      -------------------------------------------------------------------------
      it "3" (compileProgram fragment4 == fragment5)
      -------------------------------------------------------------------------
      it "4" (compileProgram fragment3 == fragment9)
      -------------------------------------------------------------------------
      it "5" (compileProgram fragment1 == fragment2)

    describe "- normalizeProgramDefs" $ do
      -------------------------------------------------------------------------
      it "1" (normalizeProgramDefs fragment7 == fragment8)

    describe "- compileSource" $ do
      passIt "1" (compileSource program4 == program11)
      passIt "2" (canonical (compileSource program44) == program45)
      passIt "3" (canonical (compileSource program46) == program47)
      passIt "4" (canonical (compileSource program48) == program49)

    describe "- isPolymorphic" $ do
      passIt "int -> int -> '0" (isPolymorphic (tInt ~> tInt ~> tVar 0 :: MonoType))
      passIt "'0" (isPolymorphic (tVar 0 :: MonoType))
      passIt "(int -> int -> '0) -> int -> int -> '0" (isPolymorphic ((tInt ~> tInt ~> tVar 0) ~> tInt ~> tInt ~> tVar 0 :: MonoType))
      passIt "(int -> int -> '0) -> int -> int -> int" (isPolymorphic ((tInt ~> tInt ~> tVar 0) ~> tInt ~> tInt ~> tInt :: MonoType))
      failIt "int -> int -> bool" (not $ isPolymorphic (tInt ~> tInt ~> tBool))
      failIt "bool" (not $ isPolymorphic tBool)

    describe "- appArgs" $ do
      it "TODO" True
