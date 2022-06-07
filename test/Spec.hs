{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Data.Either.Extra (fromRight)
import Data.Function ((&))
import Data.List.NonEmpty (fromList, toList)
import qualified Data.Map.Strict as Map
import Data.Tuple.Extra (first, second)
import Debug.Trace
import Pong.Data
import Pong.Eval
import Pong.Lang
import Pong.Tree
import Pong.Type
import Pong.Util
import qualified Pong.Util.Env as Env
import Test.Hspec

program1 :: Program MonoType TypedExpr
program1 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                  ( tRec
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
                      )
                  , "r"
                  )
                  ( eRec
                      ( rExt
                          "a"
                          (eLit (PInt 100))
                          ( rExt
                              "b"
                              (eLit (PBool True))
                              ( rExt
                                  "c"
                                  (eLit (PInt 3))
                                  rNil
                              )
                          )
                      )
                  )
                  ( eRes
                      [
                        ( tInt
                            ~> tRec (rExt "b" tBool (rExt "c" tInt rNil))
                            ~> tRec
                              ( rExt "a" tInt (rExt "b" tBool (rExt "c" tInt rNil))
                              )
                        , "a"
                        )
                      , (tInt, "x")
                      ,
                        ( tRec
                            ( rExt
                                "b"
                                tBool
                                ( rExt
                                    "c"
                                    tInt
                                    rNil
                                )
                            )
                        , "q"
                        )
                      ]
                      ( eVar
                          ( tRec
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
                              )
                          , "r"
                          )
                      )
                      ( eVar (tInt, "x")
                      )
                  )
              )
          )
        ]
    )

program2 :: Text
program2 =
  "def main(a : unit) : int =\
  \  let\
  \    r =\
  \      { a = 5, b = true }\
  \    in\
  \      let\
  \        q =\
  \          { c = 1 | r }\
  \        in\
  \          letr\
  \            { a = x | s } =\
  \              q\
  \            in\
  \              x\
  \"

-- "

program3 :: Text
program3 =
  "def main(a : unit) : int =\
  \  let\
  \    r =\
  \      { a = 5, b = true }\
  \    in\
  \      let\
  \        q =\
  \          { c = 1 | r }\
  \        in\
  \          letr\
  \            { c = x | s } =\
  \              q\
  \            in\
  \              x\
  \"

-- "

program4 :: Text
program4 =
  "def main(a : unit) : int =\
  \  let\
  \    id =\
  \      lam(x) =>\
  \        x\
  \    in\
  \      let\
  \        add =\
  \          lam(x) =>\
  \            lam(y) =>\
  \              x + y\
  \        in\
  \          let\
  \            add2 =\
  \              add(2)\
  \            in\
  \              (id(add2))(id(3)) + add(4, 5)\
  \"

-- "

evalTests :: SpecWith ()
evalTests =
  describe "Pong.Eval" $ do
    describe "- evalProgram" $ do
      -------------------------------------------------------------------------
      it "1" (Just (PrimValue (PInt 100)) == evalProgram (transformProgram program1) (Scheme (tUnit ~> tInt), "main"))
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program =
            transformProgram (fromRight emptyProgram (parseAndAnnotate program2))

      it "2" (Just (PrimValue (PInt 5)) == evalProgram program (Scheme (tUnit ~> tInt), "main"))
      -------------------------------------------------------------------------
      let program :: Program MonoType Ast
          program =
            transformProgram (fromRight emptyProgram (parseAndAnnotate program3))

      it "3" (Just (PrimValue (PInt 1)) == evalProgram program (Scheme (tUnit ~> tInt), "main"))

llvmEmitTests :: SpecWith ()
llvmEmitTests =
  describe "Pong.LLVM.Emit" $ do
    pure ()

langTests :: SpecWith ()
langTests =
  describe "Pong.Lang" $ do
    describe "- freeVars" $ do
      -------------------------------------------------------------------------
      it "1" (freeVars (eVar (tInt, "x") :: TypedExpr) == [(tInt, "x")])
      -------------------------------------------------------------------------
      it "2" (freeVars (eLit (PInt 5) :: TypedExpr) & null)
      -------------------------------------------------------------------------
      it "3" (freeVars (eLam () [(tInt, "x")] (eVar (tInt, "x")) :: TypedExpr) & null)
      -------------------------------------------------------------------------
      it "4" (freeVars (eLam () [(tInt, "x")] (eVar (tInt, "y")) :: TypedExpr) == [(tInt, "y")])
      -------------------------------------------------------------------------
      it "5" (freeVars (eLam () [((), "x")] (eVar ((), "y")) :: SourceExpr) == [((), "y")])
      -------------------------------------------------------------------------
      it "6" (freeVars (eLam () [((), "x")] (eApp () (eVar ((), "f")) [eVar ((), "y")]) :: SourceExpr) == [((), "f"), ((), "y")])
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

    {- HLINT ignore "Use typeRep -}

    describe "- typeOf" $ do
      describe "Prim" $ do
        -----------------------------------------------------------------------
        it "1" (typeOf (PInt 1) == tInt)
        -----------------------------------------------------------------------
        it "2" (typeOf (PBool True) == tBool)

      describe "Expr" $ do
        -----------------------------------------------------------------------
        it "1" (typeOf (eLit (PInt 1) :: TypedExpr) == tInt)
        -----------------------------------------------------------------------
        it "2" (typeOf (eLit (PBool True) :: TypedExpr) == tBool)
        -----------------------------------------------------------------------
        it "3" (typeOf (eVar (tInt, "x") :: TypedExpr) == tInt)

      describe "Definition" $ do
        -----------------------------------------------------------------------
        let def :: Definition MonoType TypedExpr
            def =
              Function
                (fromList [(tInt, "x")])
                (tInt ~> tInt, eLam () [(tInt, "y")] (eVar (tInt, "y")))

        it "1" (typeOf def == (tInt ~> tInt ~> tInt))

readTests :: SpecWith ()
readTests =
  describe "Pong.Read" $ do
    pure ()

treeTests :: SpecWith ()
treeTests =
  describe "Pong.Tree" $ do
    describe "- hoistTopLambdas" $ do
      -------------------------------------------------------------------------
      let def :: Definition MonoType TypedExpr
          def =
            Function
              (fromList [(tInt, "x")])
              (tInt ~> tInt, eLam () [(tInt, "y")] (eVar (tInt, "y")))

      let result :: Definition MonoType TypedExpr
          result =
            Function
              (fromList [(tInt, "x"), (tInt, "y")])
              (tInt, eVar (tInt, "y"))

      it "1" (hoistTopLambdas def == result)

    describe "- combineLambdas" $ do
      -------------------------------------------------------------------------
      let before :: TypedExpr
          before =
            eLam
              ()
              [(tInt, "a")]
              ( eLam
                  ()
                  [(tInt, "b")]
                  (eVar (tInt, "b"))
              )

      let after :: TypedExpr
          after =
            eLam
              ()
              [(tInt, "a"), (tInt, "b")]
              (eVar (tInt, "b"))

      it "1" (combineLambdas before == after)
      -------------------------------------------------------------------------
      let before :: TypedExpr
          before =
            eLam
              ()
              [(tInt, "x")]
              ( eLam
                  ()
                  [(tInt, "y")]
                  ( eLam
                      ()
                      [(tInt, "z")]
                      (eLit (PInt 5))
                  )
              )

      let after :: TypedExpr
          after =
            eLam
              ()
              [(tInt, "x"), (tInt, "y"), (tInt, "z")]
              (eLit (PInt 5))

      it "2" (combineLambdas before == after)

    describe "- parseAndAnnotate" $ do
      -------------------------------------------------------------------------
      let input :: Text
          input =
            "def main(a : unit) : int =\
            \  let\
            \    r =\
            \      { a = 100\
            \      , b = true\
            \      , c = 3\
            \      }\
            \    in\
            \      letr\
            \        { a = x | q } =\
            \          r\
            \        in\
            \          x\
            \"
      -- "

      let program :: Program MonoType TypedExpr
          program =
            program1

      it "1" (parseAndAnnotate input == Right program)

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
              (eRec (rExt "a" (eLit PUnit) (rExt "b" (eLit (PInt 2)) (rExt "c" (eLit (PBool True)) rNil))))
              (eVar ((), "x"))

      let typed :: TypedExpr
          typed =
            eRes
              [(tInt ~> tRec (rExt "a" tUnit (rExt "c" tBool rNil)) ~> tRec (rExt "a" tUnit (rExt "b" tInt (rExt "c" tBool rNil))), "b"), (tInt, "x"), (tRec (rExt "a" tUnit (rExt "c" tBool rNil)), "r")]
              (eRec (rExt "a" (eLit PUnit) (rExt "b" (eLit (PInt 2)) (rExt "c" (eLit (PBool True)) rNil))))
              (eVar (tInt, "x"))

      it "4" (Right typed == typeCheck (applySubstitution =<< inferExpr =<< tagExpr source))
      -------------------------------------------------------------------------
      let source :: SourceExpr
          source =
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
              ( eRec
                  ( rExt
                      "a"
                      (eLit (PInt 1))
                      ( rExt
                          "b"
                          (eLit (PBool True))
                          ( rExt
                              "c"
                              (eLit (PInt 3))
                              rNil
                          )
                      )
                  )
              )
              ( eRes
                  [((), "a"), ((), "x"), ((), "q")]
                  (eVar ((), "r"))
                  (eVar ((), "q"))
              )

      let typed :: TypedExpr
          typed =
            eLet
              ( tRec
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
                  )
              , "r"
              )
              ( eRec
                  ( rExt
                      "a"
                      (eLit (PInt 1))
                      ( rExt
                          "b"
                          (eLit (PBool True))
                          ( rExt
                              "c"
                              (eLit (PInt 3))
                              rNil
                          )
                      )
                  )
              )
              ( eRes
                  [
                    ( tInt
                        ~> tRec (rExt "b" tBool (rExt "c" tInt rNil))
                        ~> tRec
                          ( rExt "a" tInt (rExt "b" tBool (rExt "c" tInt rNil))
                          )
                    , "a"
                    )
                  , (tInt, "x")
                  ,
                    ( tRec
                        ( rExt
                            "b"
                            tBool
                            ( rExt
                                "c"
                                tInt
                                rNil
                            )
                        )
                    , "q"
                    )
                  ]
                  ( eVar
                      ( tRec
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
                          )
                      , "r"
                      )
                  )
                  ( eVar
                      ( tRec
                          ( rExt
                              "b"
                              tBool
                              ( rExt
                                  "c"
                                  tInt
                                  rNil
                              )
                          )
                      , "q"
                      )
                  )
              )

      it "5" (Right typed == typeCheck (applySubstitution =<< inferExpr =<< tagExpr source))

    describe "- unifyTypes" $ do
      -------------------------------------------------------------------------
      let t1 :: MonoType
          t1 = tCon "Cons" [tVar 0, tVar 1]

      let t2 :: MonoType
          t2 = tCon "Cons" [tInt, tCon "Cons" [tInt, tCon "Nil" []]]

      it "1" (let Right sub = runUnify t1 t2 in apply sub t1 == apply sub t2)
      -------------------------------------------------------------------------
      let t1 :: MonoType
          t1 = tRec (rExt "name" (tVar 0) (rVar 1))

      let t2 :: MonoType
          t2 = tRec (rExt "id" tInt (rExt "name" tString rNil))

      it "2" (let Right sub = runUnify t1 t2 in apply sub t1 `typeEq` apply sub t2)
      -------------------------------------------------------------------------
      let t1 :: MonoType
          t1 = tRec (rExt "name" (tVar 0) (rVar 1)) ~> tVar 2

      let t2 :: MonoType
          t2 = tRec (rExt "id" tInt (rExt "name" tString rNil)) ~> tInt

      it "3" (let Right sub = runUnify t1 t2 in apply sub t1 `typeEq` apply sub t2)

    describe "- unifyRows" $ do
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "name" (tVar 0) (rVar 1)

      let r2 :: Row MonoType Int
          r2 = rExt "id" tInt (rExt "name" tString rNil)

      it "1" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "name" tString (rExt "id" tInt rNil)

      let r2 :: Row MonoType Int
          r2 = rExt "id" tString (rExt "name" tInt rNil)

      it "2" (let Left e = runUnifyRows r1 r2 in UnificationError == e)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "name" tString (rExt "id" tInt rNil)

      let r2 :: Row MonoType Int
          r2 = rExt "id" tInt (rExt "name" tString rNil)

      it "3" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "name" tString (rVar 0)

      let r2 :: Row MonoType Int
          r2 = rExt "name" tString (rVar 0)

      it "4" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "a" tString (rVar 0)

      let r2 :: Row MonoType Int
          r2 = rExt "b" tString (rVar 0)

      it "5" (let Left e = runUnifyRows r1 r2 in UnificationError == e)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "name" tString (rVar 0)

      let r2 :: Row MonoType Int
          r2 = rExt "name" tString (rVar 1)

      it "6" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "id" tInt (rExt "pw" tString (rExt "name" tString rNil))

      let r2 :: Row MonoType Int
          r2 = rExt "id" tInt (rVar 0)

      it "7" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "pw" tString (rExt "name" tString (rVar 0))

      let r2 :: Row MonoType Int
          r2 = rVar 0

      it "8" (let Left e = runUnifyRows r1 r2 in UnificationError == e)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "id" tInt (rExt "pw" tString (rExt "name" tString (rVar 0)))

      let r2 :: Row MonoType Int
          r2 = rExt "id" tInt (rVar 0)

      it "9" (let Left e = runUnifyRows r1 r2 in UnificationError == e)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "name" tString (rExt "id" tInt (rExt "shoeSize" tFloat rNil))

      let r2 :: Row MonoType Int
          r2 = rExt "shoeSize" tFloat (rExt "id" tInt (rExt "name" tString rNil))

      it "10" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "name" tString (rExt "shoeSize" tFloat rNil)

      let r2 :: Row MonoType Int
          r2 = rExt "shoeSize" tFloat (rVar 0)

      it "11" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "name" tString (rExt "id" tInt (rExt "shoeSize" tFloat rNil))

      let r2 :: Row MonoType Int
          r2 = rExt "shoeSize" tFloat (rExt "id" tInt (rVar 0))

      it "12" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "name" tString (rExt "id" tInt (rExt "shoeSize" tFloat rNil))

      let r2 :: Row MonoType Int
          r2 = rVar 0

      it "13" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "shoeSize" tFloat (rExt "name" tString (rExt "id" tInt rNil))

      let r2 :: Row MonoType Int
          r2 = rExt "shoeSize" tFloat (rExt "id" tInt (rVar 0))

      it "14" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "shoeSize" tBool (rExt "name" tString (rExt "id" tInt rNil))

      let r2 :: Row MonoType Int
          r2 = rExt "shoeSize" tFloat (rExt "id" tInt (rVar 0))

      it "15" (let Left e = runUnifyRows r1 r2 in UnificationError == e)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rVar 0

      let r2 :: Row MonoType Int
          r2 = rVar 1

      it "16" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "a" tInt (rVar 0)

      let r2 :: Row MonoType Int
          r2 = rExt "a" tInt (rVar 0)

      it "17" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "a" tInt (rVar 0)

      let r2 :: Row MonoType Int
          r2 = rExt "a" tInt (rVar 1)

      it "18" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "a" tInt rNil

      let r2 :: Row MonoType Int
          r2 = rExt "a" tInt (rVar 1)

      it "19" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)
      -------------------------------------------------------------------------
      let r1 :: Row MonoType Int
          r1 = rExt "a" tInt rNil

      let r2 :: Row MonoType Int
          r2 = rExt "a" tInt rNil

      it "20" (let Right sub = runUnifyRows r1 r2 in apply sub r1 `rowEq` apply sub r2)

utilTests :: SpecWith ()
utilTests =
  describe "Pong.Util" $ do
    describe "- without" $ do
      -------------------------------------------------------------------------
      it "1" ([1, 2, 3, 4, 5] `without` [2, 4, 6] == [1, 3, 5])
      -------------------------------------------------------------------------
      it "2" (null $ [] `without` [2, 4, 6])

    describe "- getAndModify" $ do
      pure ()

    describe "- varSequence" $ do
      -------------------------------------------------------------------------
      it "1" (varSequence "foo" [1, 2, 3] == [(1, "foo0"), (2, "foo1"), (3, "foo2")])

utilEnvTests :: SpecWith ()
utilEnvTests =
  describe "Pong.Util.Env" $ do
    pure ()

utilPrettyTests :: SpecWith ()
utilPrettyTests =
  describe "Pong.Util.Pretty" $ do
    pure ()

main :: IO ()
main =
  hspec $ do
    evalTests
    llvmEmitTests
    langTests
    readTests
    treeTests
    typeTests
    utilTests
    utilEnvTests
    utilPrettyTests

typeCheck :: TypeChecker a -> Either TypeError a
typeCheck = evalTypeChecker 1 mempty

runUnify :: MonoType -> MonoType -> Either TypeError Substitution
runUnify t1 t2 = evalTypeChecker (freeIndex [t1, t2]) mempty (unifyTypes t1 t2)

runUnifyRows :: Row MonoType Int -> Row MonoType Int -> Either TypeError Substitution
runUnifyRows r1 r2 = evalTypeChecker (freeIndex [tRec r1, tRec r2]) mempty (unifyRows r1 r2)
