{-# LANGUAGE OverloadedStrings #-}

module Pong.LangTests where

-- import Data.Either.Extra (fromRight')
import Data.Function ((&))
import Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
import Pong.Data
import Pong.Lang
-- import qualified Pong.Read as Pong
-- import Pong.TestData.AnEnvelopeForJohnStJohn
-- import Pong.TestData.JackOfClubs
-- import Pong.TestData.ThePanamaHat
import Pong.TestHelpers
import Test.Hspec

-- import Text.Megaparsec (runParser)

langTests :: SpecWith ()
langTests =
  describe "Pong.Lang" $ do
    describe "- normalizeRows" $ do
      let r1, r2 :: MonoType
          r1 = rExt "foo" tInt (rExt "baz" tBool rNil)
          r2 = rExt "baz" tBool (rExt "foo" tInt rNil)

      passIt "1" $ normalizeRows r1 == r2

    --    describe "- mapRow" $ do
    --      let from :: Row TypedExpr Int
    --          from = rExt "key" (eVar (tInt, "x")) rNil
    --
    --          to :: Row TypedExpr Int
    --          to = rExt "key" (eVar (tInt, "y")) rNil
    --       in passIt "#1" (mapRow (substituteVar "x" "y") from == to)
    --
    --      let from :: Row TypedExpr Int
    --          from = rExt "one" (eVar (tInt, "x")) (rExt "two" (eVar (tInt, "x")) rNil)
    --
    --          to :: Row TypedExpr Int
    --          to = rExt "one" (eVar (tInt, "y")) (rExt "two" (eVar (tInt, "y")) rNil)
    --       in passIt "#1" (mapRow (substituteVar "x" "y") from == to)

    describe "- mapRowM" $ do
      it "TODO" True

    describe "- bimapRow" $ do
      it "TODO" True

    describe "- foldRow" $ do
      it "1" (foldRow rNil (Map.fromList []) == (rNil :: MonoType))
      it "2" (foldRow (tVar 0) (Map.fromList [("a", [tInt, tBool, tUnit]), ("b", [tBool])]) == (rExt "a" tInt (rExt "a" tBool (rExt "a" tUnit (rExt "b" tBool (tVar 0)))) :: MonoType))

    describe "- foldRow1" $ do
      -------------------------------------------------------------------------
      it "1" (foldRow1 (Map.fromList []) == (rNil :: MonoType))
      it "2" (foldRow1 (Map.fromList [("a", [tInt]), ("b", [tBool])]) == (rExt "a" tInt (rExt "b" tBool rNil) :: MonoType))
      it "3" (foldRow1 (Map.fromList [("a", [tInt, tBool, tUnit]), ("b", [tBool])]) == (rExt "a" tInt (rExt "a" tBool (rExt "a" tUnit (rExt "b" tBool rNil))) :: MonoType))

    describe "- unwindRow" $ do
      -------------------------------------------------------------------------
      it "1" (unwindRow rNil == (Map.fromList [], rNil :: MonoType))
      it "2" (unwindRow (rExt "a" tInt (rExt "b" tBool rNil) :: MonoType) == (Map.fromList [("a", [tInt]), ("b", [tBool])], rNil :: MonoType))
      it "3" (unwindRow (rExt "a" tInt (rExt "a" tBool (rExt "a" tUnit (rExt "b" tBool rNil))) :: MonoType) == (Map.fromList [("a", [tInt, tBool, tUnit]), ("b", [tBool])], rNil :: MonoType))

    describe "- restrictRow" $ do
      it "1" (restrictRow "id" (rExt "id" tInt rNil :: MonoType) == (tInt, rNil))
      it "2" (restrictRow "id" (rExt "id" tInt (tVar 0) :: MonoType) == (tInt, tVar 0))
      it "3" (restrictRow "id" (rExt "id" tInt (rExt "id" tString (tVar 0)) :: MonoType) == (tInt, rExt "id" tString (tVar 0)))

    describe "- freeVars" $ do
      -------------------------------------------------------------------------
      it "x               ->  x" (freeVars (eVar (tInt, "x") :: TypedExpr) == [(tInt, "x")])
      it "5               ->  -" (freeVars (eLit (PInt 5) :: TypedExpr) & null)
      it "lam(x) => x     ->  -" (freeVars (eLam () [(tInt, "x")] (eVar (tInt, "x")) :: TypedExpr) & null)
      it "lam(x) => y     ->  y" (freeVars (eLam () [(tInt, "x")] (eVar (tInt, "y")) :: TypedExpr) == [(tInt, "y")])
      it "lam(x) => y     ->  y" (freeVars (eLam () [((), "x")] (eVar ((), "y")) :: SourceExpr) == [((), "y")])
      it "lam(x) => f(y)  ->  f, y" (freeVars (eLam () [((), "x")] (eApp () (eVar ((), "f")) [eVar ((), "y")]) :: SourceExpr) == [((), "f"), ((), "y")])
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
      -------------------------------------------------------------------------
      let expr1 :: TypedExpr
          expr1 =
            eLet
              (tVar 0 ~> tVar 0, "id")
              (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
              ( eLet
                  (tVar 1 ~> tVar 1 ~> tVar 1, "add")
                  ( eLam
                      ()
                      [(tVar 1, "x")]
                      ( eLam
                          ()
                          [(tVar 1, "y")]
                          ( eOp2
                              (tVar 1 ~> tVar 1 ~> tVar 1, OAdd)
                              (eVar (tVar 1, "x"))
                              (eVar (tVar 1, "y"))
                          )
                      )
                  )
                  ( eLet
                      (tInt ~> tInt, "add2")
                      (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 2)])
                      ( eOp2
                          oAddInt
                          ( eApp
                              tInt
                              (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id")) [eVar (tInt ~> tInt, "add2")])
                              [ eApp tInt (eVar (tInt ~> tInt, "id")) [eLit (PInt 3)]
                              ]
                          )
                          (eApp tInt (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 4), eLit (PInt 5)])
                      )
                  )
              )
       in it "16" (freeVars expr1 == [])

    {- HLINT ignore "Use typeRep -}

    describe "- typeOf" $ do
      describe "Prim" $ do
        -----------------------------------------------------------------------
        passIt "1 : int" (typeOf (PInt 1) == tInt)
        -----------------------------------------------------------------------
        passIt "true : bool" (typeOf (PBool True) == tBool)

      describe "Expr" $ do
        -----------------------------------------------------------------------
        passIt "1 : int" (typeOf (eLit (PInt 1) :: TypedExpr) == tInt)
        -----------------------------------------------------------------------
        passIt "true : bool" (typeOf (eLit (PBool True) :: TypedExpr) == tBool)
        -----------------------------------------------------------------------
        passIt "x : int" (typeOf (eVar (tInt, "x") :: TypedExpr) == tInt)
        -----------------------------------------------------------------------
        passIt "lam(x) => x : int -> int" (typeOf (eLam () [(tInt, "x")] (eVar (tInt, "x")) :: TypedExpr) == (tInt ~> tInt))
        -----------------------------------------------------------------------
        passIt "lam(x) => x : '0 -> '0" (typeOf (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")) :: TypedExpr) == (tVar 0 ~> tVar 0))
      --      -- TODO
      --      --        -----------------------------------------------------------------------
      --      --        passIt "#6" (typeOf expr35 == TODO)
      --      describe "Row" $ do
      --        -- TODO
      --        --        -----------------------------------------------------------------------
      --        --        passIt "#1" (typeOf row36 == TODO)
      --        it "TODO" True

      describe "Definition" $ do
        -----------------------------------------------------------------------
        let func :: Definition MonoType TypedExpr
            func =
              Function
                (fromList [(tInt, "x")])
                (tInt ~> tInt, eLam () [(tInt, "y")] (eVar (tInt, "y")))

        passIt
          "fun : int -> int -> int ; func fun(x : int) : int -> int = lam(y) => y"
          (typeOf func == (tInt ~> tInt ~> tInt))

        describe "- free" $ do
          -------------------------------------------------------------------------
          let expr :: TypedExpr
              expr =
                eLet
                  (tVar 3 ~> tVar 3, "id")
                  (eLam () [(tVar 3, "x")] (eVar (tVar 3, "x")))
                  ( eLet
                      (tVar 7 ~> tVar 7 ~> tVar 7, "add")
                      ( eLam
                          ()
                          [(tVar 7, "x")]
                          ( eLam
                              ()
                              [(tVar 7, "y")]
                              ( eOp2
                                  (tVar 7 ~> tVar 7 ~> tVar 7, OAdd)
                                  (eVar (tVar 7, "x"))
                                  (eVar (tVar 7, "y"))
                              )
                          )
                      )
                      ( eLet
                          (tInt ~> tInt, "add2")
                          (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 2)])
                          ( eOp2
                              oAddInt
                              ( eApp
                                  tInt
                                  (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id")) [eVar (tInt ~> tInt, "add2")])
                                  [ eApp tInt (eVar (tInt ~> tInt, "id")) [eLit (PInt 3)]
                                  ]
                              )
                              (eApp tInt (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 4), eLit (PInt 5)])
                          )
                      )
                  )
           in it "1" (free expr == [3, 7])
        -------------------------------------------------------------------------
        --      it "2" (free program6 == [0, 1])
        --
        --    describe "- freeIndex" $ do
        --      it "TODO" True

        describe "- returnType" $ do
          it "int -> bool" (returnType (tInt ~> tBool :: MonoType) == tBool)

        describe "- argTypes" $ do
          it "unit -> int -> bool" (argTypes (tUnit ~> tInt ~> tBool :: MonoType) == [tUnit, tInt])
          it "(unit -> unit) -> int -> bool" (argTypes ((tUnit ~> tUnit) ~> tInt ~> tBool :: MonoType) == [tUnit ~> tUnit, tInt])

        describe "- foldType" $ do
          it "1" (foldType tBool [tUnit, tInt] == (tUnit ~> tInt ~> tBool :: MonoType))

        describe "- foldType1" $ do
          it "1" (foldType1 [tUnit, tInt, tBool] == (tUnit ~> tInt ~> tBool :: MonoType))

    describe "- unwindType" $ do
      it "unit -> int -> bool" (unwindType (tUnit ~> tInt ~> tBool :: MonoType) == [tUnit, tInt, tBool])
      it "unit" (unwindType (tUnit :: MonoType) == [tUnit])
      it "unit -> (int -> unit) -> bool" (unwindType (tUnit ~> (tInt ~> tUnit) ~> tBool :: MonoType) == [tUnit, tInt ~> tUnit, tBool])

    {- HLINT ignore "Use null" -}

    describe "- untag" $ do
      describe "- x" $ do
        -------------------------------------------------------------------------
        passIt "x" (untag (eVar (tInt, "x") :: TypedExpr) == [tInt])
        passIt "C" (untag (eCon (tCon "C" [], "C") :: TypedExpr) == [tCon "C" []])
        passIt "5" (untag (eLit (PInt 5) :: TypedExpr) == [])

      describe "- 5" $ do
        -------------------------------------------------------------------------
        passIt "5" (untag (eLit (PInt 5) :: TypedExpr) == [])

      describe "- if" $ do
        -------------------------------------------------------------------------
        let expr :: TypedExpr
            expr =
              eIf
                (eVar (tBool, "x"))
                (eVar (tInt, "a"))
                (eVar (tInt, "b"))
         in passIt "if x then a else b" (untag expr == [tBool, tInt])

--    describe "- substituteVar" $ do
--      -------------------------------------------------------------------------
--      let from :: SourceExpr
--          from = fromRight' (runParser Pong.expr "" expr101)
--
--          to :: SourceExpr
--          to = fromRight' (runParser Pong.expr "" expr102)
--       in it "1" (substituteVar "y" "z" from == to)
--      -------------------------------------------------------------------------
--      let expr :: SourceExpr
--          expr = fromRight' (runParser Pong.expr "" expr103)
--       in it "2" (substituteVar "y" "z" expr == expr)
--      -------------------------------------------------------------------------
--      let from :: SourceExpr
--          from = fromRight' (runParser Pong.expr "" expr104)
--
--          to :: SourceExpr
--          to = fromRight' (runParser Pong.expr "" expr105)
--       in it "3" (substituteVar "y" "z" from == to)
--      -------------------------------------------------------------------------
--      let from :: SourceExpr
--          from = fromRight' (runParser Pong.expr "" expr113)
--
--          to :: SourceExpr
--          to = fromRight' (runParser Pong.expr "" expr114)
--       in it "4" (substituteVar "y" "z" from == to)
--      -------------------------------------------------------------------------
--      let expr :: SourceExpr
--          expr = fromRight' (runParser Pong.expr "" expr106)
--       in it "5" (substituteVar "y" "z" expr == expr)
