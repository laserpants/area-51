{-# LANGUAGE OverloadedStrings #-}

module Pong.TreeTests where

import Data.List.NonEmpty (fromList)
import Pong.Lang
import Pong.TestData.AnEnvelopeForJohnStJohn
import Pong.TestData.GeraniumPlant
import Pong.TestData.GoAwayDixieGillian
import Pong.TestData.JackOfClubs
-- import Pong.TestData.TheFatalAuction
import Pong.TestData.ThePanamaHat
import Pong.TestHelpers
import Pong.Tree
import Pong.Util
import Test.Hspec hiding (after, before)

{- HLINT ignore "Use null" -}

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
      -- TODO
      --      it "2" (testHoistProgram program6 == program7)
      --
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
        -------------------------------------------------------------------------
        describe "- parseAndAnnotate" $ do
          -------------------------------------------------------------------------
          let input :: Text
              input =
                "\
                \module Main\
                \\r\n\
                \func main(a : unit) : int =\
                \  let\
                \    r =\
                \      { a = 100\
                \      , b = true\
                \      , c = 3\
                \      }\
                \    in\
                \      field\
                \        { a = x | q } =\
                \          r\
                \        in\
                \          x\
                \"
              -- "
              program :: ModuleDefs MonoType TypedExpr
              program = program1
           in it
                "1"
                (parseAndAnnotate input == Right (Module "Main" program))

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
          it "1" (testMonomorphizeModuleDefs program6 == program8)
          -------------------------------------------------------------------------
          it "2" (transform1 program6 == program9)
          -------------------------------------------------------------------------
          it "3" (transform1 program205 == program207)

        describe "- transform2" $ do
          -------------------------------------------------------------------------
          it "1" (transform2 program303 == program304)

        describe "- compileProgram" $ do
          -- TODO
          -- -------------------------------------------------------------------------
          --      it "1" (compileDefs program8 == program10)
          -------------------------------------------------------------------------
          it "2" (compileDefs fragment6 == fragment7)
          -------------------------------------------------------------------------
          it "3" (compileDefs fragment4 == fragment5)
          -------------------------------------------------------------------------
          it "4" (compileDefs fragment3 == fragment9)
          -------------------------------------------------------------------------
          it "5" (compileDefs fragment1 == fragment2)
          -------------------------------------------------------------------------
          it "6" (compileDefs program206 == program217)
          -------------------------------------------------------------------------
          it "7" (compileDefs program210 == program271)

        describe "- normalizeDef" $ do
          -------------------------------------------------------------------------
          let before :: Definition MonoType Ast
              before =
                Function
                  (fromList [(tInt, "x")])
                  (tInt ~> tInt, eVar (tInt ~> tInt, "foo"))

          let after :: Definition MonoType Ast
              after =
                Function
                  (fromList [(tInt, "x"), (tInt, "$v0")])
                  (tInt, eCall () (tInt ~> tInt, "foo") [eVar (tInt, "$v0")])

          passIt "1" (normalizeDef before == after)

        describe "- normalizeDef" $ do
          -------------------------------------------------------------------------
          it "1" ((normalizeDef <$> fragment7) == fragment8)

        --    describe "- compileSource" $ do
        --      it "TODO" True
        --    -------------------------------------------------------------------------
        --    -- TODO
        --    --  passIt "1" (compileSource program4 == program11)
        --    -------------------------------------------------------------------------
        --    -- TODO
        --    --      passIt "2" (canonical (compileSource program44) == program45)
        --    -------------------------------------------------------------------------
        --    -- TODO
        --    --      passIt "3" (canonical (compileSource program46) == program47)
        --    -------------------------------------------------------------------------
        --    -- TODO
        --    --      passIt "4" (canonical (compileSource program48) == program49)

        describe "- containsTVar" $ do
          describe "- Pass" $ do
            passIt "int -> int -> '0" (containsTVar (tInt ~> tInt ~> tVar 0 :: MonoType))
            passIt "'0" (containsTVar (tVar 0 :: MonoType))
            passIt "(int -> int -> '0) -> int -> int -> '0" (containsTVar ((tInt ~> tInt ~> tVar 0) ~> tInt ~> tInt ~> tVar 0 :: MonoType))
            passIt "(int -> int -> '0) -> int -> int -> int" (containsTVar ((tInt ~> tInt ~> tVar 0) ~> tInt ~> tInt ~> tInt :: MonoType))

          describe "- Fail" $ do
            failIt "int -> int -> bool" (not $ containsTVar (tInt ~> tInt ~> tBool))
            failIt "bool" (not $ containsTVar tBool)

    -- describe "- appArgs" $ do
    --  it "TODO" True

    describe "- exclude" $ do
      -------------------------------------------------------------------------
      it "1" (exclude [(tInt, "a"), (tInt, "b") :: Label MonoType, (tInt, "c")] ["a", "b"] == [(tInt, "c")])
      -------------------------------------------------------------------------
      it "2" (exclude [(tInt, "a"), (tInt, "b") :: Label MonoType, (tInt, "c")] [] == [(tInt, "a"), (tInt, "b"), (tInt, "c")])
      -------------------------------------------------------------------------
      it "3" (exclude [(tInt, "a"), (tInt, "b") :: Label MonoType, (tInt, "c")] ["a", "b", "c"] == [])
      -------------------------------------------------------------------------
      it "4" (exclude [(tInt, "a"), (tInt, "b") :: Label MonoType, (tInt, "b"), (tInt, "c")] ["a", "b", "c"] == [])
      -------------------------------------------------------------------------
      it "5" (exclude [(tInt, "a"), (tBool, "b") :: Label MonoType, (tInt, "b"), (tInt, "c")] ["a", "b", "c"] == [])
