{-# LANGUAGE OverloadedStrings #-}

module Pong.ReadTests where

import Data.Either.Extra (isLeft)
import Data.List.NonEmpty (fromList)
import Pong.Data
import Pong.Lang
import Pong.Read
import Pong.TestData.JackOfClubs
import Pong.TestData.ThePanamaHat
import Pong.TestHelpers
import Test.Hspec
import Text.Megaparsec

readTests :: SpecWith ()
readTests =
  describe "Pong.Read" $ do
    describe "- prim" $ do
      -------------------------------------------------------------------------
      it "()" (runParser prim "" "()" == Right PUnit)
      -------------------------------------------------------------------------
      it "( )" (runParser prim "" "( )" == Right PUnit)
      -------------------------------------------------------------------------
      it "(a)" (isLeft (runParser prim "" "(a)"))
      -------------------------------------------------------------------------
      it "5" (runParser prim "" "5" == Right (PInt 5))
      -------------------------------------------------------------------------
      it "4.5" (runParser prim "" "4.5" == Right (PDouble 4.5))
      -------------------------------------------------------------------------
      it "4.5f" (runParser prim "" "4.5f" == Right (PFloat 4.5))
      -------------------------------------------------------------------------
      it "'a'" (runParser prim "" "'a'" == Right (PChar 'a'))
      -------------------------------------------------------------------------
      it "\"foo\"" (runParser prim "" "\"foo\"" == Right (PString "foo"))
      -------------------------------------------------------------------------
      it "true" (runParser prim "" "true" == Right (PBool True))
      -------------------------------------------------------------------------
      it "false" (runParser prim "" "false" == Right (PBool False))
      -------------------------------------------------------------------------
      it "blue" (isLeft (runParser prim "" "blue"))
      -------------------------------------------------------------------------
      it "$" (isLeft (runParser prim "" "$"))

    describe "- expr" $ do
      describe "Literals" $ do
        -------------------------------------------------------------------------
        runTestParser expr "()" (eLit PUnit)
        -------------------------------------------------------------------------
        runTestParser expr "(())" (eLit PUnit)
        -------------------------------------------------------------------------
        runTestParser expr "((()))" (eLit PUnit)
        -------------------------------------------------------------------------
        runTestParser expr "( )" (eLit PUnit)
        -------------------------------------------------------------------------
        runTestParser expr "(( ))" (eLit PUnit)
        -------------------------------------------------------------------------
        runTestParser expr "5" (eLit (PInt 5))

      describe "Function application" $ do
        runTestParser expr "f(5)" (eApp () (eVar ((), "f")) [eLit (PInt 5)])
        -------------------------------------------------------------------------
        runTestParser expr "(f)(5)" (eApp () (eVar ((), "f")) [eLit (PInt 5)])
        -------------------------------------------------------------------------
        runTestParser expr "(f(5))" (eApp () (eVar ((), "f")) [eLit (PInt 5)])
        -------------------------------------------------------------------------
        runTestParser expr "f(())" (eApp () (eVar ((), "f")) [eLit PUnit])
        -------------------------------------------------------------------------
        runTestParser expr "f(( ))" (eApp () (eVar ((), "f")) [eLit PUnit])
        -------------------------------------------------------------------------
        runTestParser expr "f( () )" (eApp () (eVar ((), "f")) [eLit PUnit])
        -------------------------------------------------------------------------
        runTestParser expr "f (())" (eApp () (eVar ((), "f")) [eLit PUnit])
        -------------------------------------------------------------------------
        runTestParser expr "(f(()))" (eApp () (eVar ((), "f")) [eLit PUnit])

      describe "If statements" $ do
        runTestParser expr "if foo(1) then 1 else 2" (eIf (eApp () (eVar ((), "foo")) [eLit (PInt 1)]) (eLit (PInt 1)) (eLit (PInt 2)))
        -------------------------------------------------------------------------
        runTestParser expr "if foo(()) then 1 else 2" (eIf (eApp () (eVar ((), "foo")) [eLit PUnit]) (eLit (PInt 1)) (eLit (PInt 2)))
        -------------------------------------------------------------------------
        runTestParser expr "(if foo(()) then 1 else 2)" (eIf (eApp () (eVar ((), "foo")) [eLit PUnit]) (eLit (PInt 1)) (eLit (PInt 2)))
        -------------------------------------------------------------------------
        runTestParser expr "if (foo(())) then 1 else 2" (eIf (eApp () (eVar ((), "foo")) [eLit PUnit]) (eLit (PInt 1)) (eLit (PInt 2)))

      describe "Records" $ do
        -------------------------------------------------------------------------
        runTestParser expr "{ a = 5 }" (eRec (rExt "a" (eLit (PInt 5)) rNil))
        -------------------------------------------------------------------------
        runTestParser expr "{ a = 5, b = \"foo\" }" (eRec (rExt "a" (eLit (PInt 5)) (rExt "b" (eLit (PString "foo")) rNil)))
        -------------------------------------------------------------------------
        runTestParser expr "{ a = 5, b = \"foo\" | r }" (eRec (rExt "a" (eLit (PInt 5)) (rExt "b" (eLit (PString "foo")) (rVar ((), "r")))))

      describe "Lambda expressions" $ do
        -------------------------------------------------------------------------
        runTestParser expr "lam(x) => x" (eLam () [((), "x")] (eVar ((), "x")))
        -------------------------------------------------------------------------
        runTestParser expr "(lam(x) => x)" (eLam () [((), "x")] (eVar ((), "x")))

    describe "- def" $ do
      -------------------------------------------------------------------------
      runTestParser
        def
        "def foo(x : int, y : int) : int = 5"
        (
          ( Scheme (tInt ~> tInt ~> tInt)
          , "foo"
          )
        , Function (fromList [((), "x"), ((), "y")]) ((), eLit (PInt 5))
        )

    describe "- parseProgram" $ do
      -------------------------------------------------------------------------
      it "1" (parseProgram program4 == Right program5)
      -------------------------------------------------------------------------
      it "2" (parseProgram program200 == Right program201)
