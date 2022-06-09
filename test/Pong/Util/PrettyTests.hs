{-# LANGUAGE OverloadedStrings #-}

module Pong.Util.PrettyTests where

import Pong.Data
import Pong.Lang
import Pong.Read
import Pong.Util.Pretty
import Pong.TestData.JackOfClubs
import Test.Hspec
import Prettyprinter
import Prettyprinter.Render.Text

utilPrettyTests :: SpecWith ()
utilPrettyTests =
  describe "Pong.Util.Pretty" $ do
    describe "- Prim" $ do
      -------------------------------------------------------------------------
      it "bool(true)" (show (pretty (PBool True)) == "true")
      -------------------------------------------------------------------------
      it "bool(false)" (show (pretty (PBool False)) == "false")
      -------------------------------------------------------------------------
      it "int(123)" (show (pretty (PInt 123)) == "123")
      -------------------------------------------------------------------------
      it "float(4.1)" (show (pretty (PFloat 4.1)) == "4.1")
      -------------------------------------------------------------------------
      it "char('a')" (show (pretty (PChar 'a')) == "'a'")
      -------------------------------------------------------------------------
      it "string(\"foo\")" (show (pretty (PString "foo")) == "\"foo\"")

    describe "- Type" $ do
      -------------------------------------------------------------------------
      it "unit" (show (pretty (tUnit :: MonoType)) == "unit")
