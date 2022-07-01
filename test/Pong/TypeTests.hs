{-# LANGUAGE OverloadedStrings #-}

module Pong.TypeTests where

-- import Pong.TestData.JackOfClubs
-- import Pong.TestData.MysteriousSetOfBooks
-- import Pong.TestData.ThePanamaHat

-- import qualified Data.Map.Strict as Map
import Pong.Data
import Pong.Lang
import Pong.TestData.AnEnvelopeForJohnStJohn
import Pong.TestHelpers
import Pong.Tree
import Pong.Type
-- import Pong.Util
-- import qualified Pong.Util.Env as Env
-- import Pong.Util.Pretty ()
-- import Prettyprinter
import Test.Hspec

typeTests :: SpecWith ()
typeTests =
  describe "Pong.Type" $ do
    describe "- tagExpr" $ do
      -------------------------------------------------------------------------
      let source :: SourceExpr
          source =
            eLet
              ((), "z")
              ( eLam
                  ()
                  [((), "f")]
                  ( eLam
                      ()
                      [((), "g")]
                      ( eLam
                          ()
                          [((), "b")]
                          ( eApp
                              ()
                              (eApp () (eVar ((), "f")) [eLam () [((), "x")] (eVar ((), "x"))])
                              [eApp () (eVar ((), "g")) [eVar ((), "b")]]
                          )
                      )
                  )
              )
              ( eApp
                  ()
                  (eVar ((), "z"))
                  [ eLam () [((), "x")] (eVar ((), "x"))
                  , eLam () [((), "x")] (eVar ((), "x"))
                  , eLit (PInt 1)
                  ]
              )

          tagged :: TaggedExpr
          tagged =
            eLet
              (1, "z")
              ( eLam
                  ()
                  [(2, "f")]
                  ( eLam
                      ()
                      [(3, "g")]
                      ( eLam
                          ()
                          [(4, "b")]
                          ( eApp
                              5
                              (eApp 6 (eVar (7, "f")) [eLam () [(8, "x")] (eVar (9, "x"))])
                              [eApp 10 (eVar (11, "g")) [eVar (12, "b")]]
                          )
                      )
                  )
              )
              ( eApp
                  13
                  (eVar (14, "z"))
                  [ eLam () [(15, "x")] (eVar (16, "x"))
                  , eLam () [(17, "x")] (eVar (18, "x"))
                  , eLit (PInt 1)
                  ]
              )
       in it "1" (Right tagged == typeCheck (tagExpr source))

    describe "- inferExpr" $ do
      -------------------------------------------------------------------------
      let tagged :: TaggedExpr
          tagged =
            eLet
              (1, "id")
              (eLam () [(2, "x")] (eVar (3, "x")))
              (eApp 4 (eApp 5 (eVar (6, "id")) [eVar (7, "id")]) [eLit (PInt 1)])

          typed :: TypedExpr
          typed =
            eLet
              (tVar 2 ~> tVar 2, "id")
              (eLam () [(tVar 2, "x")] (eVar (tVar 2, "x")))
              ( eApp
                  tInt
                  ( eApp
                      (tInt ~> tInt)
                      (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id"))
                      [eVar (tInt ~> tInt, "id")]
                  )
                  [eLit (PInt 1)]
              )
          i = succ (maximum (untag tagged))
       in it "1" (Right typed == evalTypeChecker i mempty (applySubstitution =<< inferExpr tagged))
      -------------------------------------------------------------------------
      let source :: SourceExpr
          source =
            eLet
              ((), "id")
              (eLam () [((), "x")] (eVar ((), "x")))
              ( eLet
                  ((), "f")
                  ( eLam
                      ()
                      [((), "x")]
                      (eLam () [((), "y")] (eOp2 ((), OAdd) (eVar ((), "x")) (eVar ((), "y"))))
                  )
                  ( eLet
                      ((), "g")
                      (eApp () (eVar ((), "f")) [eLit (PInt 2)])
                      ( eOp2
                          ((), OAdd)
                          ( eApp
                              ()
                              (eApp () (eVar ((), "id")) [eVar ((), "g")])
                              [eApp () (eVar ((), "id")) [eLit (PInt 3)]]
                          )
                          (eApp () (eVar ((), "f")) [eLit (PInt 4), eLit (PInt 5)])
                      )
                  )
              )

          typed :: TypedExpr
          typed =
            eLet
              (tVar 2 ~> tVar 2, "id")
              (eLam () [(tVar 2, "x")] (eVar (tVar 2, "x")))
              ( eLet
                  (tVar 6 ~> tVar 6 ~> tVar 6, "f")
                  ( eLam
                      ()
                      [(tVar 6, "x")]
                      ( eLam
                          ()
                          [(tVar 6, "y")]
                          (eOp2 (tVar 6 ~> tVar 6 ~> tVar 6, OAdd) (eVar (tVar 6, "x")) (eVar (tVar 6, "y")))
                      )
                  )
                  ( eLet
                      (tInt ~> tInt, "g")
                      ( eApp
                          (tInt ~> tInt)
                          (eVar (tInt ~> tInt ~> tInt, "f"))
                          [eLit (PInt 2)]
                      )
                      ( eOp2
                          oAddInt
                          ( eApp
                              tInt
                              ( eApp
                                  (tInt ~> tInt)
                                  (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id"))
                                  [eVar (tInt ~> tInt, "g")]
                              )
                              [eApp tInt (eVar (tInt ~> tInt, "id")) [eLit (PInt 3)]]
                          )
                          ( eApp
                              tInt
                              (eVar (tInt ~> tInt ~> tInt, "f"))
                              [eLit (PInt 4), eLit (PInt 5)]
                          )
                      )
                  )
              )
       in it "2" (Right typed == typeCheck (applySubstitution =<< inferExpr =<< tagExpr source))
      -------------------------------------------------------------------------
      let source :: SourceExpr
          source =
            eLet
              ((), "f")
              ( eLam
                  ()
                  [((), "n")]
                  ( eIf
                      (eOp2 ((), OEq) (eLit (PInt 0)) (eVar ((), "n")))
                      (eLit (PInt 1))
                      ( eOp2
                          ((), OMul)
                          (eVar ((), "n"))
                          ( eApp
                              ()
                              (eVar ((), "f"))
                              [eOp2 ((), OSub) (eVar ((), "n")) (eLit (PInt 1))]
                          )
                      )
                  )
              )
              (eApp () (eVar ((), "f")) [eLit (PInt 5)])

          typed :: TypedExpr
          typed =
            eLet
              (tInt ~> tInt, "f")
              ( eLam
                  ()
                  [(tInt, "n")]
                  ( eIf
                      (eOp2 oEqInt (eLit (PInt 0)) (eVar (tInt, "n")))
                      (eLit (PInt 1))
                      ( eOp2
                          oMulInt
                          (eVar (tInt, "n"))
                          ( eApp
                              tInt
                              (eVar (tInt ~> tInt, "f"))
                              [eOp2 oSubInt (eVar (tInt, "n")) (eLit (PInt 1))]
                          )
                      )
                  )
              )
              (eApp tInt (eVar (tInt ~> tInt, "f")) [eLit (PInt 5)])
       in it "3" (Right typed == typeCheck (applySubstitution =<< inferExpr =<< tagExpr source))
      -------------------------------------------------------------------------
      let source :: SourceExpr
          source =
            eRes
              [((), "b"), ((), "x"), ((), "r")]
              (eExt "a" (eLit PUnit) (eExt "b" (eLit (PInt 2)) (eExt "c" (eLit (PBool True)) eNil)))
              (eVar ((), "x"))

          typed :: TypedExpr
          typed =
            eRes
              [(tInt ~> rExt "a" tUnit (rExt "c" tBool rNil) ~> rExt "a" tUnit (rExt "b" tInt (rExt "c" tBool rNil)), "b"), (tInt, "x"), (rExt "a" tUnit (rExt "c" tBool rNil), "r")]
              (eExt "a" (eLit PUnit) (eExt "b" (eLit (PInt 2)) (eExt "c" (eLit (PBool True)) eNil)))
              (eVar (tInt, "x"))
       in it "4" (Right typed == typeCheck (applySubstitution =<< inferExpr =<< tagExpr source))
      -------------------------------------------------------------------------
      let source :: SourceExpr
          source =
            -- let
            --   r =
            --     Rec [ a = 1
            --         , b = true
            --         , c = 3
            --         ]
            --   in
            --     letr
            --       { a = x | q } =
            --         open r
            --       in
            --         q
            --
            --    { x = z | r }
            --
            --    Rec [ x = z | r ]
            --

            -- let
            --   r =
            --     { a = 1
            --     , b = true
            --     , c = 3
            --     }
            --   in
            --     letr
            --       { a = x | q } =
            --         r
            --       in
            --         q
            --
            eLet
              ((), "r")
              ( eExt
                  "a"
                  (eLit (PInt 1))
                  ( eExt
                      "b"
                      (eLit (PBool True))
                      ( eExt
                          "c"
                          (eLit (PInt 3))
                          eNil
                      )
                  )
              )
              ( eRes
                  [((), "a"), ((), "x"), ((), "q")]
                  (eVar ((), "r"))
                  (eVar ((), "q"))
              )

          typed :: TypedExpr
          typed =
            eLet
              ( rExt
                  "a"
                  tInt
                  ( rExt
                      "b"
                      tBool
                      ( rExt
                          "c"
                          tInt
                          rNil
                      )
                  )
              , "r"
              )
              ( eExt
                  "a"
                  (eLit (PInt 1))
                  ( eExt
                      "b"
                      (eLit (PBool True))
                      ( eExt
                          "c"
                          (eLit (PInt 3))
                          eNil
                      )
                  )
              )
              ( eRes
                  [
                    ( tInt
                        ~> rExt "b" tBool (rExt "c" tInt rNil)
                        ~> rExt "a" tInt (rExt "b" tBool (rExt "c" tInt rNil))
                    , "a"
                    )
                  , (tInt, "x")
                  ,
                    ( rExt
                        "b"
                        tBool
                        ( rExt
                            "c"
                            tInt
                            rNil
                        )
                    , "q"
                    )
                  ]
                  ( eVar
                      ( rExt
                          "a"
                          tInt
                          ( rExt
                              "b"
                              tBool
                              ( rExt
                                  "c"
                                  tInt
                                  rNil
                              )
                          )
                      , "r"
                      )
                  )
                  ( eVar
                      ( rExt
                          "b"
                          tBool
                          ( rExt
                              "c"
                              tInt
                              rNil
                          )
                      , "q"
                      )
                  )
              )
       in it "5" (Right typed == typeCheck (applySubstitution =<< inferExpr =<< tagExpr source))
      -------------------------------------------------------------------------
      let source :: SourceExpr
          source = expr0

          typed :: TypedExpr
          typed = expr1

          eq a b = Right True == (isIsomorphicTo <$> a <*> b)
       in it "6" (Right typed `eq` typeCheck (applySubstitution =<< inferExpr =<< tagExpr source))

    --    describe "- inferProgram" $ do
    --      -------------------------------------------------------------------------
    --      it "1" ((runInferProgram program5 <&> canonical) == Right program6)
    --      -------------------------------------------------------------------------
    --      let env =
    --            Env.fromList
    --              [
    --                ( "Nil"
    --                , Right (Scheme (tCon "List" [tVar "a"]))
    --                )
    --              ,
    --                ( "Cons"
    --                , Right (Scheme (tVar "a" ~> tCon "List" [tVar "a"] ~> tCon "List" [tVar "a"]))
    --                )
    --              ]
    --      -------------------------------------------------------------------------
    --      it "2" ((runInferProgramWithEnv env program201 <&> canonical) == Right program202)
    --      -------------------------------------------------------------------------
    --      it "3" ((runInferProgramWithEnv env program204 <&> canonical) == Right program205)
    --      -------------------------------------------------------------------------
    --      it "4" ((runInferProgramWithEnv env program209 <&> canonical) == Right program210)
    --      -------------------------------------------------------------------------
    --      it "5" ((runInferProgram program341 <&> canonical) == Right program3412)
    --      -------------------------------------------------------------------------
    --      it "6" ((runInferProgram program342 <&> canonical) == Right program3422)
    --    --      -------------------------------------------------------------------------
    --    --      it "7" ((runInferProgram program343 <&> canonical) == Right program3432)
    --
    --    describe "- unifyTypes" $ do
    --      -------------------------------------------------------------------------
    --      let t1 :: MonoType
    --          -- T '0 '1
    --          t1 = tCon "T" [tVar 0, tVar 1]
    --
    --          t2 :: MonoType
    --          -- T int (T int Y)
    --          t2 = tCon "T" [tInt, tCon "T" [tInt, tCon "Y" []]]
    --       in it "1" (let Right sub = runUnify t1 t2 in apply sub t1 == apply sub t2)
    --      -------------------------------------------------------------------------
    --      let t1 :: MonoType
    --          t1 = tRec (rExt "name" (tVar 0) (rVar 1))
    --
    --          t2 :: MonoType
    --          t2 = tRec (rExt "id" tInt (rExt "name" tString rNil))
    --       in it "2" (let Right sub = runUnify t1 t2 in apply sub t1 `typeEq` apply sub t2)
    --      -------------------------------------------------------------------------
    --      let t1 :: MonoType
    --          t1 = tRec (rExt "name" (tVar 0) (rVar 1)) ~> tVar 2
    --
    --          t2 :: MonoType
    --          t2 = tRec (rExt "id" tInt (rExt "name" tString rNil)) ~> tInt
    --       in it "3" (let Right sub = runUnify t1 t2 in apply sub t1 `typeEq` apply sub t2)

    describe "- unifyRows" $ do
      let passUnifyRows row1 row2 =
            passIt
              (show msg :: String)
              ( let Right sub = runUnifyRows row1 row2
                 in apply sub row1 `rowEq` apply sub row2
              )
            where
              msg :: String
              msg =
                "TODO" -- "{" <+> pretty row1 <+> "} ~ {" <+> pretty row2 <+> "}"
      let failUnifyRows row1 row2 err =
            failIt (show msg :: String) (Left err == runUnifyRows row1 row2)
            where
              msg :: String
              msg =
                "TODO" -- "{" <+> pretty row1 <+> "} /~ {" <+> pretty row2 <+> "}"
      describe "- Pass" $ do
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "name" (tVar 0) (tVar 1)

            r2 :: MonoType
            r2 = rExt "id" tInt (rExt "name" tString rNil)
         in passUnifyRows r1 r2
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "name" tString (rExt "id" tInt rNil)

            r2 :: MonoType
            r2 = rExt "id" tInt (rExt "name" tString rNil)
         in passUnifyRows r1 r2
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "name" tString (tVar 0)

            r2 :: MonoType
            r2 = rExt "name" tString (tVar 0)
         in passUnifyRows r1 r2
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "name" tString (tVar 0)

            r2 :: MonoType
            r2 = rExt "name" tString (tVar 1)
         in passUnifyRows r1 r2
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "id" tInt (rExt "pw" tString (rExt "name" tString rNil))

            r2 :: MonoType
            r2 = rExt "id" tInt (tVar 0)
         in passUnifyRows r1 r2
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "name" tString (rExt "id" tInt (rExt "shoeSize" tFloat rNil))

            r2 :: MonoType
            r2 = rExt "shoeSize" tFloat (rExt "id" tInt (rExt "name" tString rNil))
         in passUnifyRows r1 r2

        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "name" tString (rExt "shoeSize" tFloat rNil)

            r2 :: MonoType
            r2 = rExt "shoeSize" tFloat (tVar 0)
         in passUnifyRows r1 r2
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "name" tString (rExt "id" tInt (rExt "shoeSize" tFloat rNil))

            r2 :: MonoType
            r2 = rExt "shoeSize" tFloat (rExt "id" tInt (tVar 0))
         in passUnifyRows r1 r2
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "name" tString (rExt "id" tInt (rExt "shoeSize" tFloat rNil))

            r2 :: MonoType
            r2 = tVar 0
         in passUnifyRows r1 r2
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "shoeSize" tFloat (rExt "name" tString (rExt "id" tInt rNil))

            r2 :: MonoType
            r2 = rExt "shoeSize" tFloat (rExt "id" tInt (tVar 0))
         in passUnifyRows r1 r2
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = tVar 0

            r2 :: MonoType
            r2 = tVar 1
         in passUnifyRows r1 r2
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "a" tInt (tVar 0)

            r2 :: MonoType
            r2 = rExt "a" tInt (tVar 0)
         in passUnifyRows r1 r2
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "a" tInt (tVar 0)

            r2 :: MonoType
            r2 = rExt "a" tInt (tVar 1)
         in passUnifyRows r1 r2
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "a" tInt rNil

            r2 :: MonoType
            r2 = rExt "a" tInt (tVar 1)
         in passUnifyRows r1 r2
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "a" tInt rNil

            r2 :: MonoType
            r2 = rExt "a" tInt rNil
         in passUnifyRows r1 r2

      describe "- Fail" $ do
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "shoeSize" tBool (rExt "name" tString (rExt "id" tInt rNil))

            r2 :: MonoType
            r2 = rExt "shoeSize" tFloat (rExt "id" tInt (tVar 0))
         in failUnifyRows r1 r2 UnificationError
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "id" tInt (rExt "pw" tString (rExt "name" tString (tVar 0)))

            r2 :: MonoType
            r2 = rExt "id" tInt (tVar 0)
         in failUnifyRows r1 r2 UnificationError
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "pw" tString (rExt "name" tString (tVar 0))

            r2 :: MonoType
            r2 = tVar 0
         in failUnifyRows r1 r2 UnificationError
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "a" tString (tVar 0)

            r2 :: MonoType
            r2 = rExt "b" tString (tVar 0)
         in failUnifyRows r1 r2 UnificationError
        -------------------------------------------------------------------------
        let r1 :: MonoType
            r1 = rExt "name" tString (rExt "id" tInt rNil)

            r2 :: MonoType
            r2 = rExt "id" tString (rExt "name" tInt rNil)
         in failUnifyRows r1 r2 UnificationError
