{-# LANGUAGE OverloadedStrings #-}

module Pong.ReadTests where

import Pong.Data
import Pong.Lang
import Pong.Read
import Pong.TestData.JackOfClubs
import Test.Hspec
import Text.Megaparsec hiding (token)

readTests :: SpecWith ()
readTests =
  describe "Pong.Read" $ do
    describe "- prim" $ do
      -------------------------------------------------------------------------
      it "1" (runParser prim "" "()" == Right PUnit)
      -------------------------------------------------------------------------
      it "2" (runParser prim "" "( )" == Right PUnit)

    describe "- expr" $ do
      -------------------------------------------------------------------------
      it "3" (runParser expr "" "()" == Right (eLit PUnit))
      -------------------------------------------------------------------------
      it "4" (runParser expr "" "( )" == Right (eLit PUnit))
      -------------------------------------------------------------------------
      it "5" (runParser expr "" "f(5)" == Right (eApp () (eVar ((), "f")) [eLit (PInt 5)]))
      -------------------------------------------------------------------------
      it "6" (runParser expr "" "f(())" == Right (eApp () (eVar ((), "f")) [eLit PUnit]))
      -------------------------------------------------------------------------
      it "7" (runParser expr "" "if foo(1) then 1 else 2" == Right (eIf (eApp () (eVar ((), "foo")) [eLit (PInt 1)]) (eLit (PInt 1)) (eLit (PInt 2))))
      -------------------------------------------------------------------------
      it "8" (runParser expr "" "if foo(()) then 1 else 2" == Right (eIf (eApp () (eVar ((), "foo")) [eLit PUnit]) (eLit (PInt 1)) (eLit (PInt 2))))

    describe "- parseProgram" $ do
      -------------------------------------------------------------------------
      it "1" (parseProgram program4 == Right program5)
