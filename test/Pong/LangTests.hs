{-# LANGUAGE OverloadedStrings #-}

module Pong.LangTests where

import Data.Function ((&))
import Data.List.NonEmpty (fromList, toList)
import Pong.Data
import Pong.Lang
import Pong.TestData.AnEnvelopeForJohnStJohn
import Pong.TestData.JackOfClubs
import Test.Hspec

langTests :: SpecWith ()
langTests =
  describe "Pong.Lang" $ do
    describe "- freeVars" $ do
      -------------------------------------------------------------------------
      it "1" (freeVars (eVar (tInt, "x") :: TypedExpr) == [(tInt, "x")])
      -------------------------------------------------------------------------
      it "2" (freeVars (eLit (PInt 5) :: TypedExpr) & null)
      -------------------------------------------------------------------------
      it "3" (freeVars (eLam () [(tInt, "x")] (eVar (tInt, "x")) :: TypedExpr) & null)
      -------------------------------------------------------------------------
      it "4" (freeVars (eLam () [(tInt, "x")] (eVar (tInt, "y")) :: TypedExpr) == [(tInt, "y")])
      -------------------------------------------------------------------------
      it "5" (freeVars (eLam () [((), "x")] (eVar ((), "y")) :: SourceExpr) == [((), "y")])
      -------------------------------------------------------------------------
      it "6" (freeVars (eLam () [((), "x")] (eApp () (eVar ((), "f")) [eVar ((), "y")]) :: SourceExpr) == [((), "f"), ((), "y")])
      -------------------------------------------------------------------------
      it "7" (freeVars (eLam () [((), "x")] (eApp () (eVar ((), "f")) [eVar ((), "x")]) :: SourceExpr) == [((), "f")])
      -------------------------------------------------------------------------
      it "8" (freeVars (eLet ((), "f") (eVar ((), "foo")) (eLam () [((), "x")] (eApp () (eVar ((), "f")) [eVar ((), "x")])) :: SourceExpr) == [((), "foo")])
      -------------------------------------------------------------------------
      it "9" (freeVars (eOp2 ((), OAdd) (eVar ((), "x")) (eVar ((), "y")) :: SourceExpr) == [((), "x"), ((), "y")])
      -------------------------------------------------------------------------
      it "10" (freeVars (eApp () (eVar ((), "f")) [eLit (PInt 5)] :: SourceExpr) == [((), "f")])
      -------------------------------------------------------------------------
      it "11" (freeVars (eIf (eVar ((), "x")) (eVar ((), "y")) (eVar ((), "z")) :: SourceExpr) == [((), "x"), ((), "y"), ((), "z")])
      -------------------------------------------------------------------------
      it "12" (freeVars (eIf (eVar ((), "x")) (eVar ((), "y")) (eVar ((), "y")) :: SourceExpr) == [((), "x"), ((), "y")])
      -------------------------------------------------------------------------
      it "13" (freeVars (ePat (eVar ((), "xs")) [([((), "Cons"), ((), "x"), ((), "ys")], eVar ((), "x"))] :: SourceExpr) == [((), "xs")])
      -------------------------------------------------------------------------
      it "14" (freeVars (ePat (eVar ((), "xs")) [([((), "Cons"), ((), "x"), ((), "ys")], eVar ((), "y"))] :: SourceExpr) == [((), "xs"), ((), "y")])
      -------------------------------------------------------------------------
      it "15" (freeVars (ePat (eVar ((), "xs")) [([((), "Cons"), ((), "x"), ((), "ys")], eVar ((), "x")), ([((), "Nil")], eVar ((), "y"))] :: SourceExpr) == [((), "xs"), ((), "y")])

    {- HLINT ignore "Use typeRep -}

    describe "- typeOf" $ do
      describe "Prim" $ do
        -----------------------------------------------------------------------
        it "1" (typeOf (PInt 1) == tInt)
        -----------------------------------------------------------------------
        it "2" (typeOf (PBool True) == tBool)

      describe "Expr" $ do
        -----------------------------------------------------------------------
        it "1" (typeOf (eLit (PInt 1) :: TypedExpr) == tInt)
        -----------------------------------------------------------------------
        it "2" (typeOf (eLit (PBool True) :: TypedExpr) == tBool)
        -----------------------------------------------------------------------
        it "3" (typeOf (eVar (tInt, "x") :: TypedExpr) == tInt)

      describe "Definition" $ do
        -----------------------------------------------------------------------
        let def :: Definition MonoType TypedExpr
            def =
              Function
                (fromList [(tInt, "x")])
                (tInt ~> tInt, eLam () [(tInt, "y")] (eVar (tInt, "y")))

        it "1" (typeOf def == (tInt ~> tInt ~> tInt))

    describe "- free" $ do
      -------------------------------------------------------------------------
      it "1" (free expr1 == [3, 7])
      -------------------------------------------------------------------------
      it "2" (free program6 == [0, 1])

    describe "- foldRow" $ do
      it "TODO" True

    describe "- foldRow1" $ do
      it "TODO" True

    describe "- unwindRow" $ do
      it "TODO" True

    describe "- restrictRow" $ do
      it "TODO" True

    describe "- freeIndex" $ do
      it "TODO" True

    describe "- returnType" $ do
      it "int -> bool" (returnType (tInt ~> tBool :: MonoType) == tBool)

    describe "- argTypes" $ do
      it "unit -> int -> bool" (argTypes (tUnit ~> tInt ~> tBool :: MonoType) == [tUnit, tInt])
      it "(unit -> unit) -> int -> bool" (argTypes ((tUnit ~> tUnit) ~> tInt ~> tBool :: MonoType) == [(tUnit ~> tUnit), tInt])

    describe "- foldType" $ do
      it "1" (foldType tBool [tUnit, tInt] == (tUnit ~> tInt ~> tBool :: MonoType))

    describe "- foldType1" $ do
      it "1" (foldType1 [tUnit, tInt, tBool] == (tUnit ~> tInt ~> tBool :: MonoType))

    describe "- unwindType" $ do
      it "1" (unwindType (tUnit ~> tInt ~> tBool :: MonoType) == [tUnit, tInt, tBool])
      it "2" (unwindType (tUnit :: MonoType) == [tUnit])
