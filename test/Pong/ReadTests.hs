{-# LANGUAGE OverloadedStrings #-}

module Pong.ReadTests where

import Data.Either.Extra (isLeft, isRight)
import Pong.Data
import Pong.Lang
import Pong.Read
import Pong.TestData.JackOfClubs
import Pong.TestHelpers
import Pong.Util
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
      runTestParser expr "f(5)" (eApp () (eVar ((), "f")) [eLit (PInt 5)])
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
      it "12" (runParser expr "" "(f(()))" == Right (eApp () (eVar ((), "f")) [eLit PUnit]))
      -------------------------------------------------------------------------
      it "13" (runParser expr "" "if foo(1) then 1 else 2" == Right (eIf (eApp () (eVar ((), "foo")) [eLit (PInt 1)]) (eLit (PInt 1)) (eLit (PInt 2))))
      -------------------------------------------------------------------------
      it "14" (runParser expr "" "if foo(()) then 1 else 2" == Right (eIf (eApp () (eVar ((), "foo")) [eLit PUnit]) (eLit (PInt 1)) (eLit (PInt 2))))
      -------------------------------------------------------------------------
      it "15" (runParser expr "" "(if foo(()) then 1 else 2)" == Right (eIf (eApp () (eVar ((), "foo")) [eLit PUnit]) (eLit (PInt 1)) (eLit (PInt 2))))
      -------------------------------------------------------------------------
      it "16" (runParser expr "" "if (foo(())) then 1 else 2" == Right (eIf (eApp () (eVar ((), "foo")) [eLit PUnit]) (eLit (PInt 1)) (eLit (PInt 2))))
      -------------------------------------------------------------------------
      it "17" (runParser expr "" "5" == Right (eLit (PInt 5)))
      -------------------------------------------------------------------------
      it "18" (runParser expr "" "{ a = 5 }" == Right (eRec (rExt "a" (eLit (PInt 5)) rNil)))
      -------------------------------------------------------------------------
      it "19" (runParser expr "" "{ a = 5, b = \"foo\" }" == Right (eRec (rExt "a" (eLit (PInt 5)) (rExt "b" (eLit (PString "foo")) rNil))))
      -------------------------------------------------------------------------
      it "20" (runParser expr "" "{ a = 5, b = \"foo\" | r }" == Right (eRec (rExt "a" (eLit (PInt 5)) (rExt "b" (eLit (PString "foo")) (rVar ((), "r"))))))

    describe "- parseProgram" $ do
      -------------------------------------------------------------------------
      it "1" (parseProgram program4 == Right program5)
