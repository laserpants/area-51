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
      describe "- Pass" $ do
        -------------------------------------------------------------------------
        passIt "()" (runParser prim "" "()" == Right PUnit)
        -------------------------------------------------------------------------
        passIt "( )" (runParser prim "" "( )" == Right PUnit)
        -------------------------------------------------------------------------
        passIt "5" (runParser prim "" "5" == Right (PInt 5))
        -------------------------------------------------------------------------
        passIt "4.5" (runParser prim "" "4.5" == Right (PDouble 4.5))
        -------------------------------------------------------------------------
        passIt "4.5f" (runParser prim "" "4.5f" == Right (PFloat 4.5))
        -------------------------------------------------------------------------
        passIt "'a'" (runParser prim "" "'a'" == Right (PChar 'a'))
        -------------------------------------------------------------------------
        passIt "\"foo\"" (runParser prim "" "\"foo\"" == Right (PString "foo"))
        -------------------------------------------------------------------------
        passIt "true" (runParser prim "" "true" == Right (PBool True))
        -------------------------------------------------------------------------
        passIt "false" (runParser prim "" "false" == Right (PBool False))

      describe "- Fail" $ do
        -------------------------------------------------------------------------
        failIt "(a)" (isLeft (runParser prim "" "(a)"))
        -------------------------------------------------------------------------
        failIt "blue" (isLeft (runParser prim "" "blue"))
        -------------------------------------------------------------------------
        failIt "$" (isLeft (runParser prim "" "$"))

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
        -------------------------------------------------------------------------
        runTestParser expr "f()" (eApp () (eVar ((), "f")) [eLit PUnit])
        -------------------------------------------------------------------------
        runTestParser expr "(f())" (eApp () (eVar ((), "f")) [eLit PUnit])

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
      -------------------------------------------------------------------------
      it "3" (parseProgram program203 == Right program204)
      -------------------------------------------------------------------------
      it "4" (parseProgram program208 == Right program209)
