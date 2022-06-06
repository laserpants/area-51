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
          i =
            freeIndex (tVar . fst <$> freeVars tagged)

      it "1" (Right typed == evalTypeChecker i mempty (applySubstitution =<< inferExpr tagged))

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
