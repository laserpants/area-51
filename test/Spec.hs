{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Data.Either.Extra (fromRight)
import Data.Function ((&))
import Data.List.NonEmpty (fromList, toList)
import qualified Data.Map.Strict as Map
import Data.Tuple.Extra (first, second)
import Debug.Trace
import Pong.Data
import Pong.Eval
import Pong.Lang
import Pong.ReadTests
import Pong.TestData.AnEnvelopeForJohnStJohn
import Pong.TestData.GoAwayDixieGillian
import Pong.TestData.JackOfClubs
import Pong.TestData.ShirtMixUpAtTheLaundry
import Pong.TestHelpers
import Pong.Tree
import Pong.TreeTests
import Pong.Type
import Pong.TypeTests
import Pong.Util
import qualified Pong.Util.Env as Env
import Test.Hspec

evalTests :: SpecWith ()
evalTests =
  describe "Pong.Eval" $ do
    describe "- evalProgram" $ do
      -------------------------------------------------------------------------
      it "1" (Just (PrimValue (PInt 100)) == evalProgram (transformProgram program1) (Scheme (tUnit ~> tInt), "main"))
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program =
            transformProgram (fromRight emptyProgram (parseAndAnnotate program2))

      it "2" (Just (PrimValue (PInt 5)) == evalProgram program (Scheme (tUnit ~> tInt), "main"))
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program =
            transformProgram (fromRight emptyProgram (parseAndAnnotate program3))

      it "3" (Just (PrimValue (PInt 1)) == evalProgram program (Scheme (tUnit ~> tInt), "main"))

llvmEmitTests :: SpecWith ()
llvmEmitTests =
  describe "Pong.LLVM.Emit" $ do
    pure ()

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

utilEnvTests :: SpecWith ()
utilEnvTests =
  describe "Pong.Util.Env" $ do
    pure ()

utilPrettyTests :: SpecWith ()
utilPrettyTests =
  describe "Pong.Util.Pretty" $ do
    pure ()

main :: IO ()
main =
  hspec $ do
    evalTests
    llvmEmitTests
    langTests
    readTests
    treeTests
    typeTests
    utilTests
    utilEnvTests
    utilPrettyTests
