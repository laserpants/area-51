{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Pong.Data
import Pong.Lang
import Pong.Type
import Test.Hspec

evalTests :: SpecWith ()
evalTests =
  describe "Pong.Eval" $ do
    pure ()

llvmEmitTests :: SpecWith ()
llvmEmitTests =
  describe "Pong.LLVM.Emit" $ do
    pure ()

langTests :: SpecWith ()
langTests =
  describe "Pong.Lang" $ do
    pure ()

readTests :: SpecWith ()
readTests =
  describe "Pong.Read" $ do
    pure ()

treeTests :: SpecWith ()
treeTests =
  describe "Pong.Tree" $ do
    pure ()

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

      let tagged :: TaggedExpr
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
      it "1" (Right tagged == typeCheck (tagExpr source))

    describe "- inferExpr" $ do
      -------------------------------------------------------------------------
      let tagged :: TaggedExpr
          tagged =
            eLet
              (1, "id")
              (eLam () [(2, "x")] (eVar (3, "x")))
              (eApp 4 (eApp 5 (eVar (6, "id")) [eVar (7, "id")]) [eLit (PInt 1)])

      let typed :: TypedExpr
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
          i =
            freeIndex (tVar . fst <$> freeVars tagged)

      it "1" (Right typed == evalTypeChecker i mempty (applySubstitution =<< inferExpr tagged))
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

      let typed :: TypedExpr
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

      it "2" (Right typed == typeCheck (applySubstitution =<< inferExpr =<< tagExpr source))
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

      let typed :: TypedExpr
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

      it "3" (Right typed == typeCheck (applySubstitution =<< inferExpr =<< tagExpr source))
      -------------------------------------------------------------------------
      let source :: SourceExpr
          source =
            eRes
              [((), "b"), ((), "x"), ((), "r")]
              (eRow (rExt "a" (eLit PUnit) (rExt "b" (eLit (PInt 2)) (rExt "c" (eLit (PBool True)) rNil))))
              (eVar ((), "x"))

      let typed :: TypedExpr
          typed =
            eRes
              [(tInt ~> tRow (rExt "a" tUnit (rExt "c" tBool rNil)) ~> tRow (rExt "a" tUnit (rExt "b" tInt (rExt "c" tBool rNil))), "b"), (tInt, "x"), (tRow (rExt "a" tUnit (rExt "c" tBool rNil)), "r")]
              (eRow (rExt "a" (eLit PUnit) (rExt "b" (eLit (PInt 2)) (rExt "c" (eLit (PBool True)) rNil))))
              (eVar (tInt, "x"))

      it "4" (Right typed == typeCheck (applySubstitution =<< inferExpr =<< tagExpr source))

    describe "- unifyTypes" $ do
      -------------------------------------------------------------------------
      let t1 :: MonoType
          t1 = tCon "Cons" [tVar 0, tVar 1]

      let t2 :: MonoType
          t2 = tCon "Cons" [tInt, tCon "Cons" [tInt, tCon "Nil" []]]

      it "1" (let Right sub = runUnify t1 t2 in apply sub t1 == t2)

utilTests :: SpecWith ()
utilTests =
  describe "Pong.Util" $ do
    pure ()

utilEnvTests :: SpecWith ()
utilEnvTests =
  describe "Pong.Util.Env" $ do
    pure ()

main :: IO ()
main =
  hspec $ do
    evalTests
    evalTests
    llvmEmitTests
    langTests
    readTests
    treeTests
    typeTests
    utilTests
    utilEnvTests

typeCheck :: TypeChecker a -> Either TypeError a
typeCheck = evalTypeChecker 1 mempty

runUnify :: MonoType -> MonoType -> Either TypeError Substitution
runUnify t1 t2 = evalTypeChecker (freeIndex [t1, t2]) mempty (unifyTypes t1 t2)
