{-# LANGUAGE OverloadedStrings #-}

module Pong.Util.PrettyTests where

-- import Pong.Data
-- import Pong.Lang
-- import Pong.Util
-- import Pong.Util.Pretty ()
-- import Prettyprinter
-- import Test.Hspec
--
-- utilPrettyTests :: SpecWith ()
-- utilPrettyTests =
--  describe "Pong.Util.Pretty" $ do
--    describe "- Prim" $ do
--      -------------------------------------------------------------------------
--      it "true" (show (pretty (PBool True)) == "true")
--      -------------------------------------------------------------------------
--      it "false" (show (pretty (PBool False)) == "false")
--      -------------------------------------------------------------------------
--      it "123" (show (pretty (PInt 123)) == "123")
--      -------------------------------------------------------------------------
--      it "4.1" (show (pretty (PDouble 4.1)) == "4.1")
--      -------------------------------------------------------------------------
--      it "4.1f" (show (pretty (PFloat 4.1)) == "4.1f")
--      -------------------------------------------------------------------------
--      it "'a'" (show (pretty (PChar 'a')) == "'a'")
--      -------------------------------------------------------------------------
--      it "\"foo\"" (show (pretty (PString "foo")) == "\"foo\"")
--      -------------------------------------------------------------------------
--      it "()" (show (pretty PUnit) == "()")
--
--    describe "- Type" $ do
--      -------------------------------------------------------------------------
--      it "unit" (show (pretty (tUnit :: MonoType)) == "unit")
--      -------------------------------------------------------------------------
--      it "'0" (show (pretty (tVar 0 :: MonoType)) == "'0")
--      -------------------------------------------------------------------------
--      it "a" (show (pretty (tVar "a" :: Type Name)) == "a")
--      -------------------------------------------------------------------------
--      it "int -> int" (show (pretty (tInt ~> tInt :: MonoType)) == "int -> int")
--      -------------------------------------------------------------------------
--      it "int -> int -> int" (show (pretty (tInt ~> tInt ~> tInt :: MonoType)) == "int -> int -> int")
--      -------------------------------------------------------------------------
--      it "(int -> int) -> int" (show (pretty ((tInt ~> tInt) ~> tInt :: MonoType)) == "(int -> int) -> int")
--      -------------------------------------------------------------------------
--      it "{ a : int }" (show (pretty (tRec (rExt "a" tInt rNil :: Row MonoType Int))) == "{ a : int }")
--      -------------------------------------------------------------------------
--      it "{ a : int, on : bool }" (show (pretty (tRec (rExt "a" tInt (rExt "on" tBool rNil) :: Row MonoType Int))) == "{ a : int, on : bool }")
--      -------------------------------------------------------------------------
--      it "{ a : int, on : bool | '0 }" (show (pretty (tRec (rExt "a" tInt (rExt "on" tBool (rVar 0)) :: Row MonoType Int))) == "{ a : int, on : bool | '0 }")
--
--    describe "- Row" $ do
--      -------------------------------------------------------------------------
--      it "a : int" (show (pretty (rExt "a" tInt rNil :: Row MonoType Int)) == "a : int")
--      -------------------------------------------------------------------------
--      it "a : int, on : bool" (show (pretty (rExt "a" tInt (rExt "on" tBool rNil) :: Row MonoType Int)) == "a : int, on : bool")
--      -------------------------------------------------------------------------
--      it "a : int, on : bool | '0" (show (pretty (rExt "a" tInt (rExt "on" tBool (rVar 0)) :: Row MonoType Int)) == "a : int, on : bool | '0")
