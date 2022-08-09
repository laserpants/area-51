{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.Reader
import Control.Monad.State
import Taiyaki.Data
import Taiyaki.Data.Cons
import Taiyaki.Lang
import Taiyaki.Tree
import Taiyaki.Util
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    ---------------------------------------------------------------------------
    describe "Con" $ do
      let ty :: Type ()
          ty = con kTyp (tupleCon 2) [tInt, tInt]
       in it
            "(int, int)"
            (tApp kTyp (tApp kFun1 (tCon kFun2 (tupleCon 2)) tInt) tInt == ty)

      let ty :: Type Name
          ty = con kTyp "List" [tVar kTyp "a"]
       in it
            "List a"
            (tApp kTyp (tCon kFun1 "List") (tVar kTyp "a") == ty)

      let expr ::
            Expr
              (Type ())
              [Pattern ()]
              (Clause (Type ()) [Pattern (Type ())])
              (Clause (Type ()) [Pattern (Type ())])
              (Binding ())
          expr = con (tList tInt) "[]" []
       in it
            "[]"
            (eCon (tList tInt) "[]" == expr)

      let expr ::
            Expr
              (Type ())
              [Pattern ()]
              (Clause (Type ()) [Pattern (Type ())])
              (Clause (Type ()) [Pattern (Type ())])
              (Binding ())
          expr = con (tList tInt) "(::)" [eVar tInt "x", con (tList tInt) "[]" []]
       in it
            "x :: []"
            ( eApp
                (tList tInt)
                (eCon (tInt ~> tList tInt ~> tList tInt) "(::)")
                [eVar tInt "x", eCon (tList tInt) "[]"]
                == expr
            )
    ---------------------------------------------------------------------------
    describe "(~>)" $ do
      describe "Operator associativity" $ do
        let ty1 :: Type Name
            ty1 = tVar kTyp "a" ~> tVar kTyp "b" ~> tVar kTyp "c"
            ty2 :: Type Name
            ty2 = tArr (tVar kTyp "a") (tArr (tVar kTyp "b") (tVar kTyp "c"))
        it
          "a ~> b ~> c == a -> (b -> c)"
          (ty1 == ty2)
    ---------------------------------------------------------------------------
    describe "tApps" $ do
      let ctor, ty :: Type ()
          ctor = tCon kFun2 "C2"
          ty = tApps ctor [tInt, tBool]
       in it
            "C2 int bool ( C2 : * -> * -> * )"
            (tApp kTyp (tApp kFun1 ctor tInt) tBool == ty)

      let ctor, ty :: Type ()
          ctor = tCon kFun2 "C2"
          ty = tApps ctor []
       in it
            "C2 ( C2 : * -> * -> * )"
            (ctor == ty)

      let ctor, ty :: Type ()
          ctor = tCon kFun1 "List"
          ty = tApps ctor [tInt]
       in it
            "List int ( List : * -> * )"
            (tApp kTyp ctor tInt == ty)
    ---------------------------------------------------------------------------
    describe "tupleCon" $ do
      it "()" (tupleCon 1 == "()")
      it "(,)" (tupleCon 2 == "(,)")
      it "(,,)" (tupleCon 3 == "(,,)")
      it "(,,,)" (tupleCon 4 == "(,,,)")
    ---------------------------------------------------------------------------
    describe "Tuple" $ do
      let ty :: Type ()
          ty = tup () [tInt, tInt]
       in it
            "(int, int)"
            (tApp kTyp (tApp kFun1 (tCon kFun2 (tupleCon 2)) tInt) tInt == ty)

      let ty :: Type ()
          ty = tup () [tInt, tInt, tBool]
       in it
            "(int, int, bool)"
            ( tApp
                kTyp
                (tApp kFun1 (tApp kFun2 (tCon (kFun 3) (tupleCon 3)) tInt) tInt)
                tBool
                == ty
            )

      let expr ::
            Expr
              ()
              [Pattern ()]
              (Clause (Type ()) [Pattern (Type ())])
              (Clause (Type ()) [Pattern (Type ())])
              (Binding ())
          expr = tup () [eLit () (IInt 1), eLit () (IInt 2)]
       in it
            "(1, 2)"
            (eTup () [eLit () (IInt 1), eLit () (IInt 2)] == expr)
    ---------------------------------------------------------------------------
    describe "Row" $ do
      let expr ::
            Expr
              ()
              [Pattern ()]
              (Clause (Type ()) [Pattern (Type ())])
              (Clause (Type ()) [Pattern (Type ())])
              (Binding ())
          expr = rExt "a" (eVar () "x") rNil
       in it
            "{ a = x }"
            (expr == eExt () "a" (eVar () "x") (eNil ()))

      let pat :: Pattern ()
          pat = rExt "a" (pVar () "x") rNil
       in it
            "| { a = x }"
            (pat == pExt () "a" (pVar () "x") (pNil ()))

      let ty :: Type ()
          ty = rExt "a" tInt rNil
       in it
            "{ a : int }"
            (ty == tExt "a" tInt tNil)
      ---------------------------------------------------------------------------
      let expr ::
            Expr
              ()
              [Pattern ()]
              (Clause (Type ()) [Pattern (Type ())])
              (Clause (Type ()) [Pattern (Type ())])
              (Binding ())
          expr = rExt "a" (eVar () "x") (eVar () "y")
       in it
            "{ a = x | y }"
            (expr == eExt () "a" (eVar () "x") (eVar () "y"))

      let pat :: Pattern ()
          pat = rExt "a" (pVar () "x") (pVar () "y")
       in it
            "| { a = x | y }"
            (pat == pExt () "a" (pVar () "x") (pVar () "y"))

      let ty :: Type Int
          ty = tExt "a" tInt (tVar kTyp 0)
       in it
            "{ a : int | '0 }"
            (ty == tExt "a" tInt (tVar kTyp 0))
    ---------------------------------------------------------------------------
    describe "preprocessRecords" $ do
      let r =
            preprocessRecords
              -- { name = n, id = a }
              ( pRec
                  (tRec (tExt "name" tString (tExt "id" tInt tNil)))
                  ( pExt
                      (tExt "name" tString (tExt "id" tInt tNil))
                      "name"
                      (pVar tString "n")
                      ( pExt
                          (tExt "id" tInt tNil)
                          "id"
                          (pVar tInt "a")
                          (pNil tNil)
                      )
                  )
              )
       in it "| { name = n, id = a } : { name : string, id : int }" $
            (r :: Pattern (Type Int))
              == pTup
                (tup () [tInt, tup () [tString, tup () []]])
                [ pVar tInt "a"
                , pTup
                    (tup () [tString, tup () []])
                    [ pVar tString "n"
                    , pLit (tup () []) IUnit
                    ]
                ]
    ---------------------------------------------------------------------------
    describe "rawTuple" $ do
      let expr ::
            Expr
              (Type Int)
              [Pattern (Type Int)]
              (Clause (Type ()) [Pattern (Type ())])
              (Clause (Type ()) [Pattern (Type ())])
              (Binding ())
          expr =
            rawTuple
              (tup () [tInt, tBool])
              [eVar tInt "a", eVar tBool "b"]
       in it
            "(a, b)"
            ( eApp
                (tApp kTyp (tApp kFun1 (tCon kFun2 "(,)") tInt) tBool)
                ( eCon
                    ( tInt
                        ~> tBool
                        ~> tApp kTyp (tApp kFun1 (tCon kFun2 "(,)") tInt) tBool
                    )
                    (tupleCon 2)
                )
                [eVar tInt "a", eVar tBool "b"]
                == expr
            )
    ---------------------------------------------------------------------------
    describe "rawList" $ do
      let expr ::
            Expr
              (Type Int)
              [Pattern (Type Int)]
              (Clause (Type ()) [Pattern (Type ())])
              (Clause (Type ()) [Pattern (Type ())])
              (Binding ())
          expr =
            rawList
              (tList tInt)
              [ eLit tInt (IInt 1)
              , eLit tInt (IInt 2)
              , eLit tInt (IInt 3)
              ]
       in it
            "[1, 2, 3]"
            ( eApp
                (tList tInt)
                (eCon (tInt ~> tList tInt ~> tList tInt) "(::)")
                [ eLit
                    tInt
                    (IInt 1)
                , eApp
                    (tList tInt)
                    (eCon (tInt ~> tList tInt ~> tList tInt) "(::)")
                    [ eLit
                        tInt
                        (IInt 2)
                    , eApp
                        (tList tInt)
                        (eCon (tInt ~> tList tInt ~> tList tInt) "(::)")
                        [ eLit
                            tInt
                            (IInt 3)
                        , eCon
                            (tList tInt)
                            "[]"
                        ]
                    ]
                ]
                == expr
            )
    ---------------------------------------------------------------------------
    describe "Exhaustive pattern match checking" $ do
      runTestExhaustive
        "No patterns"
        True -- exhaustive
        [ [] :: [Pattern ()]
        ]

      describe "Literal patterns" $ do
        runTestExhaustive
          "| True \
          \| False"
          -- "
          True -- exhaustive
          [ [pLit () (IBool True)]
          , [pLit () (IBool False)]
          ]

        runTestExhaustive
          "| True"
          False -- not exhaustive
          [ [pLit () (IBool True)]
          ]

        runTestExhaustive
          "| True \
          \| _"
          -- "
          True -- exhaustive
          [ [pLit () (IBool True)]
          , [pAny ()]
          ]

        runTestExhaustive
          "| ()"
          True -- exhaustive
          [ [pLit () IUnit]
          ]

        runTestExhaustive
          "| (), ()"
          True -- exhaustive
          [ [pLit () IUnit, pLit () IUnit]
          ]

        runTestExhaustive
          "| (), _"
          True -- exhaustive
          [ [pLit () IUnit, pAny ()]
          ]

        runTestExhaustive
          "| (), 3"
          False -- not exhaustive
          [ [pLit () IUnit, pLit () (IInt 3)]
          ]

        runTestExhaustive
          "| 5"
          False -- not exhaustive
          [ [pLit () (IInt 5)]
          ]

        runTestExhaustive
          "| 5 \
          \| 4"
          -- "
          False -- not exhaustive
          [ [pLit () (IInt 5)]
          , [pLit () (IInt 4)]
          ]

        runTestExhaustive
          "| 5 \
          \| x"
          -- "
          True -- exhaustive
          [ [pLit () (IInt 5)]
          , [pVar () "x"]
          ]

        runTestExhaustive
          "| 5, 5 \
          \| x, y"
          -- "
          True -- exhaustive
          [ [pLit () (IInt 5), pLit () (IInt 5)]
          , [pVar () "x", pVar () "y"]
          ]

        runTestExhaustive
          "| 5, 5 \
          \| x, 0"
          -- "
          False -- not exhaustive
          [ [pLit () (IInt 5), pLit () (IInt 5)]
          , [pVar () "x", pLit () (IInt 0)]
          ]

        runTestExhaustive
          "| \"x\" \
          \| \"y\""
          -- "
          False -- not exhaustive
          [ [pLit () (IString "x")]
          , [pLit () (IString "y")]
          ]

        runTestExhaustive
          "| \"x\" \
          \| \"y\" \
          \| _"
          -- "
          True -- exhaustive
          [ [pLit () (IString "x")]
          , [pLit () (IString "y")]
          , [pAny ()]
          ]

      describe "List literals" $ do
        runTestExhaustive
          "| [_, _, _]"
          False -- not exhaustive
          [ [pList () [pAny (), pAny (), pAny ()]]
          ]

        runTestExhaustive
          "| [_, _, _] \
          \| []"
          -- "
          False -- not exhaustive
          [ [pList () [pAny (), pAny (), pAny ()]]
          , [pList () []]
          ]

        runTestExhaustive
          "| [_, _, _] \
          \| _"
          -- "
          True -- exhaustive
          [ [pList () [pAny (), pAny (), pAny ()]]
          , [pAny ()]
          ]

        runTestExhaustive
          "| [1, 2]"
          False -- not exhaustive
          [ [pList () [pLit () (IInt 1), pLit () (IInt 2)]]
          ]

        runTestExhaustive
          "| [x, 2]"
          False -- not exhaustive
          [ [pList () [pVar () "x", pLit () (IInt 2)]]
          ]

        runTestExhaustive
          "| [x, 2] \
          \| _"
          -- "
          True -- exhaustive
          [ [pList () [pVar () "x", pLit () (IInt 2)]]
          , [pAny ()]
          ]

        runTestExhaustive
          "| [x, y]"
          False -- not exhaustive
          [ [pList () [pVar () "x", pVar () "y"]]
          ]

        runTestExhaustive
          "| x :: ys \
          \| []"
          -- "
          True -- exhaustive
          [ [pCon () "(::)" [pVar () "x", pVar () "ys"]]
          , [pList () []]
          ]

        runTestExhaustive
          "| [] \
          \| [x] \
          \| [x, y] \
          \| x :: y :: ys"
          -- "
          True -- exhaustive
          [ [pList () []]
          , [pList () [pVar () "x"]]
          , [pList () [pVar () "x", pVar () "y"]]
          , [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
          ]

      describe "Or-patterns" $ do
        runTestExhaustive
          "| False"
          False -- not exhaustive
          [ [pLit () (IBool False)]
          ]

        runTestExhaustive
          "| False or True"
          True -- exhaustive
          [ [pOr () (pLit () (IBool False)) (pLit () (IBool True))]
          ]

        runTestExhaustive
          "| [] or (_ :: _)"
          True -- exhaustive
          [ [pOr () (pList () []) (pCon () "(::)" [pAny (), pAny ()])]
          ]

        runTestExhaustive
          "| (_ :: _) or []"
          True -- exhaustive
          [ [pOr () (pCon () "(::)" [pAny (), pAny ()]) (pCon () "[]" [])]
          ]

        runTestExhaustive
          "| 1 or 2"
          False -- not exhaustive
          [ [pOr () (pLit () (IInt 1)) (pLit () (IInt 1))]
          ]

        runTestExhaustive
          "| [_] or []"
          False -- not exhaustive
          [ [pOr () (pList () [pAny ()]) (pCon () "[]" [])]
          ]

        runTestExhaustive
          "| [_] or [] \
          \| _ :: _"
          -- "
          True -- exhaustive
          [ [pOr () (pList () [pAny ()]) (pCon () "[]" [])]
          , [pCon () "(::)" [pAny (), pAny ()]]
          ]

      describe "As-patterns" $ do
        runTestExhaustive
          "| (x :: ys) as xs \
          \| []"
          -- "
          True -- exhaustive
          [ [pAs () "xs" (pCon () "(::)" [pVar () "x", pVar () "ys"])]
          , [pList () []]
          ]

        runTestExhaustive
          "| (x :: ys) as xs"
          False -- not exhaustive
          [ [pAs () "xs" (pCon () "(::)" [pVar () "x", pVar () "ys"])]
          ]

        runTestExhaustive
          "| True"
          True -- exhaustive
          [ [pAs () "foo" (pAny ())]
          ]

        runTestExhaustive
          "| [] \
          \| [x] as xs \
          \| [x, y] as xs \
          \| (x :: y :: ys) as xs"
          -- "
          True -- exhaustive
          [ [pList () []]
          , [pAs () "xs" (pList () [pVar () "x"])]
          , [pAs () "xs" (pList () [pVar () "x", pVar () "y"])]
          , [pAs () "xs" (pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]])]
          ]

      describe "Constructed value patterns" $ do
        runTestExhaustive
          "| _ :: _ :: _ :: []"
          False -- not exhaustive
          [ [pCon () "(::)" [pAny (), pCon () "(::)" [pAny (), pCon () "(::)" [pAny (), pCon () "[]" []]]]]
          ]

        runTestExhaustive
          "| [] \
          \| x :: y :: ys \
          \| z :: zs"
          -- "
          True -- exhaustive
          [ [pCon () "[]" []]
          , [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
          , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
          ]

        runTestExhaustive
          "| x :: y :: ys \
          \| [] \
          \| z :: zs"
          -- "
          True -- exhaustive
          [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
          , [pCon () "[]" []]
          , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
          ]

        runTestExhaustive
          "| x :: y :: ys \
          \| z :: zs"
          -- "
          False -- not exhaustive
          [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
          , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
          ]

        runTestExhaustive
          "| x :: y :: ys \
          \| z :: zs \
          \| _ :: _"
          -- "
          False -- not exhaustive
          [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
          , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
          , [pCon () "(::)" [pAny (), pAny ()]]
          ]

        runTestExhaustive
          "| x :: y :: ys \
          \| z :: zs \
          \| _ :: _ \
          \| []"
          -- "
          True -- exhaustive
          [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
          , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
          , [pCon () "(::)" [pAny (), pAny ()]]
          , [pCon () "[]" []]
          ]

        runTestExhaustive
          "| x :: y :: ys \
          \| [] \
          \| z :: []"
          -- "
          True -- exhaustive
          [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
          , [pCon () "[]" []]
          , [pCon () "(::)" [pVar () "z", pCon () "[]" []]]
          ]

        runTestExhaustive
          "| x :: y :: ys \
          \| []"
          -- "
          False -- not exhaustive
          [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
          , [pCon () "[]" []]
          ]

        runTestExhaustive
          "| []"
          False -- not exhaustive
          [ [pCon () "[]" []]
          ]

        runTestExhaustive
          "| _"
          True -- exhaustive
          [ [pAny ()]
          ]

        runTestExhaustive
          "| [] \
          \| _"
          -- "
          True -- exhaustive
          [ [pCon () "[]" []]
          , [pAny ()]
          ]

        runTestExhaustive
          "| x :: ys \
          \| []"
          -- "
          True -- exhaustive
          [ [pCon () "(::)" [pVar () "x", pVar () "ys"]]
          , [pCon () "[]" []]
          ]

        runTestExhaustive
          "| x :: ys \
          \| x"
          -- "
          True -- exhaustive
          [ [pCon () "(::)" [pVar () "x", pVar () "ys"]]
          , [pVar () "x"]
          ]

        runTestExhaustive
          "| x :: ys, 2 \
          \| [], _"
          -- "
          False -- not exhaustive
          [ [pCon () "(::)" [pVar () "x", pVar () "ys", pLit () (IInt 2)]]
          , [pCon () "[]" [], pAny ()]
          ]

        runTestExhaustive
          "| x :: xs, true \
          \| x :: xs, false \
          \| [], _"
          -- "
          True -- exhaustive
          [ [pCon () "(::)" [pVar () "x", pVar () "xs"], pLit () (IBool True)]
          , [pCon () "(::)" [pVar () "x", pVar () "xs"], pLit () (IBool False)]
          , [pCon () "[]" [], pAny ()]
          ]

        runTestExhaustive
          "| x :: xs, true \
          \| 3 :: xs, false \
          \| [], _"
          -- "
          False -- not exhaustive
          [ [pCon () "(::)" [pVar () "x", pVar () "xs"], pLit () (IBool True)]
          , [pCon () "(::)" [pLit () (IInt 3), pVar () "xs"], pLit () (IBool False)]
          , [pCon () "[]" [], pAny ()]
          ]

      describe "Tuple patterns" $ do
        runTestExhaustive
          "| (1, 2)"
          False -- not exhaustive
          [ [pTup () [pLit () (IInt 1), pLit () (IInt 2)]]
          ]

        runTestExhaustive
          "| (_, _)"
          True -- exhaustive
          [ [pTup () [pAny (), pAny ()]]
          ]

        runTestExhaustive
          "| (1, 2) \
          \| (_, _)"
          -- "
          True -- exhaustive
          [ [pTup () [pLit () (IInt 1), pLit () (IInt 2)]]
          , [pTup () [pAny (), pAny ()]]
          ]

        runTestExhaustive
          "| ((), ())"
          True -- exhaustive
          [ [pTup () [pLit () IUnit, pLit () IUnit]]
          ]

        runTestExhaustive
          "| ((), _)"
          True -- exhaustive
          [ [pTup () [pLit () IUnit, pAny ()]]
          ]

        runTestExhaustive
          "| ((), 3)"
          False -- not exhaustive
          [ [pTup () [pLit () IUnit, pLit () (IInt 3)]]
          ]

        runTestExhaustive
          "| (5, 5) \
          \| (x, y)"
          -- "
          True -- exhaustive
          [ [pTup () [pLit () (IInt 5), pLit () (IInt 5)]]
          , [pTup () [pVar () "x", pVar () "y"]]
          ]

        runTestExhaustive
          "| (5, 5) \
          \| (x, 0)"
          -- "
          False -- not exhaustive
          [ [pTup () [pLit () (IInt 5), pLit () (IInt 5)]]
          , [pTup () [pVar () "x", pLit () (IInt 0)]]
          ]

        runTestExhaustive
          "| (x :: ys, 2) \
          \| ([], _)"
          -- "
          False -- not exhaustive
          [ [pTup () [pCon () "(::)" [pVar () "x", pVar () "ys", pLit () (IInt 2)]]]
          , [pTup () [pCon () "[]" [], pAny ()]]
          ]

        runTestExhaustive
          "| (x :: xs, true) \
          \| (x :: xs, false) \
          \| ([], _)"
          -- "
          True -- exhaustive
          [ [pTup () [pCon () "(::)" [pVar () "x", pVar () "xs"], pLit () (IBool True)]]
          , [pTup () [pCon () "(::)" [pVar () "x", pVar () "xs"], pLit () (IBool False)]]
          , [pTup () [pCon () "[]" [], pAny ()]]
          ]

        runTestExhaustive
          "| (x :: xs, true) \
          \| (3 :: xs, false) \
          \| ([], _)"
          -- "
          False -- not exhaustive
          [ [pTup () [pCon () "(::)" [pVar () "x", pVar () "xs"], pLit () (IBool True)]]
          , [pTup () [pCon () "(::)" [pLit () (IInt 3), pVar () "xs"], pLit () (IBool False)]]
          , [pTup () [pCon () "[]" [], pAny ()]]
          ]

        runTestExhaustive
          "| (x :: xs, true) \
          \| (3 :: xs, false) \
          \| ([], _) \
          \| _"
          -- "
          True -- exhaustive
          [ [pTup () [pCon () "(::)" [pVar () "x", pVar () "xs"], pLit () (IBool True)]]
          , [pTup () [pCon () "(::)" [pLit () (IInt 3), pVar () "xs"], pLit () (IBool False)]]
          , [pTup () [pCon () "[]" [], pAny ()]]
          , [pAny ()]
          ]

        runTestExhaustive
          "| (x, (y, z))"
          True -- exhaustive
          [ [pTup () [pVar () "x", pTup () [pVar () "y", pVar () "z"]]]
          ]

        runTestExhaustive
          "| (false, false) \
          \| (false, true) \
          \| (true, false) \
          \| (true, true)"
          -- "
          True -- exhaustive
          [ [pTup () [pLit () (IBool False), pLit () (IBool False)]]
          , [pTup () [pLit () (IBool False), pLit () (IBool True)]]
          , [pTup () [pLit () (IBool True), pLit () (IBool False)]]
          , [pTup () [pLit () (IBool True), pLit () (IBool True)]]
          ]

        runTestExhaustive
          "| (false, false) \
          \| (false, true) \
          \| (true, false)"
          -- "
          False -- not exhaustive
          [ [pTup () [pLit () (IBool False), pLit () (IBool False)]]
          , [pTup () [pLit () (IBool False), pLit () (IBool True)]]
          , [pTup () [pLit () (IBool True), pLit () (IBool False)]]
          ]

        runTestExhaustive
          "| (false, false, false) \
          \| (false, false, true) \
          \| (false, true, false) \
          \| (false, true, true) \
          \| (true, false, false) \
          \| (true, false, true) \
          \| (true, true, false) \
          \| (true, true, true)"
          -- "
          True -- exhaustive
          [ [pTup () [pLit () (IBool False), pLit () (IBool False), pLit () (IBool False)]]
          , [pTup () [pLit () (IBool False), pLit () (IBool False), pLit () (IBool True)]]
          , [pTup () [pLit () (IBool False), pLit () (IBool True), pLit () (IBool False)]]
          , [pTup () [pLit () (IBool False), pLit () (IBool True), pLit () (IBool True)]]
          , [pTup () [pLit () (IBool True), pLit () (IBool False), pLit () (IBool False)]]
          , [pTup () [pLit () (IBool True), pLit () (IBool False), pLit () (IBool True)]]
          , [pTup () [pLit () (IBool True), pLit () (IBool True), pLit () (IBool False)]]
          , [pTup () [pLit () (IBool True), pLit () (IBool True), pLit () (IBool True)]]
          ]

      describe "Record patterns" $ do
        runTestExhaustive
          "| { a = _ }"
          True -- exhaustive
          [ [pRec () (pExt () "a" (pAny ()) (pNil ()))]
          ]

        runTestExhaustive
          "| { a = 5 }"
          False -- not exhaustive
          [ [pRec () (pExt () "a" (pLit () (IInt 5)) (pNil ()))]
          ]

        runTestExhaustive
          "| { x = 3, y = 4 } \
          \| { x = 6, y = 7 }"
          -- "
          False -- not exhaustive
          [ [pRec () (pExt () "x" (pLit () (IInt 3)) (pExt () "y" (pLit () (IInt 4)) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IInt 6)) (pExt () "y" (pLit () (IInt 7)) (pNil ())))]
          ]

        runTestExhaustive
          "| { x = x }"
          True -- exhaustive
          [ [pRec () (pExt () "x" (pVar () "x") (pNil ()))]
          ]

        runTestExhaustive
          "| { x = x }"
          True -- exhaustive
          [ [pRec () (pExt () "x" (pVar () "x") (pCon () "{}" []))]
          ]

        runTestExhaustive
          "| { x = true }"
          False -- not exhaustive
          [ [pRec () (pExt () "x" (pLit () (IBool True)) (pNil ()))]
          ]

        runTestExhaustive
          "| { x = 3, y = 4 } \
          \| { x = 6, y = 7 } \
          \| { x = _, y = 7 } \
          \| { x = x, y = _ }"
          -- "
          True -- exhaustive
          [ [pRec () (pExt () "x" (pLit () (IInt 3)) (pExt () "y" (pLit () (IInt 4)) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IInt 6)) (pExt () "y" (pLit () (IInt 7)) (pNil ())))]
          , [pRec () (pExt () "x" (pAny ()) (pExt () "y" (pLit () (IInt 7)) (pNil ())))]
          , [pRec () (pExt () "x" (pVar () "x") (pExt () "y" (pAny ()) (pNil ())))]
          ]

        runTestExhaustive
          "| { x = _ }"
          True -- exhaustive
          [ [pRec () (pExt () "x" (pAny ()) (pNil ()))]
          ]

        runTestExhaustive
          "| { x = _, y = a }"
          True -- exhaustive
          [ [pRec () (pExt () "x" (pAny ()) (pExt () "y" (pVar () "a") (pNil ())))]
          ]

        runTestExhaustive
          "| { x = 3, y = { a = 3 } } \
          \| { x = 6, y = { a = 4 } } \
          \| { x = _, y = { a = 5 } } \
          \| { x = x, y = { a = _ } }"
          -- "
          True -- exhaustive
          [ [pRec () (pExt () "x" (pLit () (IInt 3)) (pExt () "y" (pRec () (pExt () "a" (pLit () (IInt 3)) (pNil ()))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IInt 6)) (pExt () "y" (pRec () (pExt () "a" (pLit () (IInt 4)) (pNil ()))) (pNil ())))]
          , [pRec () (pExt () "x" (pAny ()) (pExt () "y" (pRec () (pExt () "a" (pLit () (IInt 5)) (pNil ()))) (pNil ())))]
          , [pRec () (pExt () "x" (pVar () "x") (pExt () "y" (pRec () (pExt () "a" (pAny ()) (pNil ()))) (pNil ())))]
          ]

        runTestExhaustive
          "| { x = 3, y = { a = 3 } } \
          \| { x = 6, y = { a = 4 } } \
          \| { x = _, y = { a = 5 } } \
          \| { x = x, y = _ }"
          -- "
          True -- exhaustive
          [ [pRec () (pExt () "x" (pLit () (IInt 3)) (pExt () "y" (pRec () (pExt () "a" (pLit () (IInt 3)) (pNil ()))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IInt 6)) (pExt () "y" (pRec () (pExt () "a" (pLit () (IInt 4)) (pNil ()))) (pNil ())))]
          , [pRec () (pExt () "x" (pAny ()) (pExt () "y" (pRec () (pExt () "a" (pLit () (IInt 5)) (pNil ()))) (pNil ())))]
          , [pRec () (pExt () "x" (pVar () "x") (pExt () "y" (pAny ()) (pNil ())))]
          ]

        runTestExhaustive
          "| { x = 3, y = { a = 3 } } \
          \| { x = 6, y = { a = 4 } } \
          \| { x = _, y = { a = 5 } } \
          \| { x = x | q }"
          -- "
          True -- exhaustive
          [ [pRec () (pExt () "x" (pLit () (IInt 3)) (pExt () "y" (pRec () (pExt () "a" (pLit () (IInt 3)) (pNil ()))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IInt 6)) (pExt () "y" (pRec () (pExt () "a" (pLit () (IInt 4)) (pNil ()))) (pNil ())))]
          , [pRec () (pExt () "x" (pAny ()) (pExt () "y" (pRec () (pExt () "a" (pLit () (IInt 5)) (pNil ()))) (pNil ())))]
          , [pRec () (pExt () "x" (pVar () "x") (pVar () "q"))]
          ]

        runTestExhaustive
          "| { x = 3, y = { a = 3 } } \
          \| { x = 6, y = { a = 4 } } \
          \| { x = _, y = { a = 5 } } \
          \| z"
          -- "
          True -- exhaustive
          [ [pRec () (pExt () "x" (pLit () (IInt 3)) (pExt () "y" (pRec () (pExt () "a" (pLit () (IInt 3)) (pNil ()))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IInt 6)) (pExt () "y" (pRec () (pExt () "a" (pLit () (IInt 4)) (pNil ()))) (pNil ())))]
          , [pRec () (pExt () "x" (pAny ()) (pExt () "y" (pRec () (pExt () "a" (pLit () (IInt 5)) (pNil ()))) (pNil ())))]
          , [pVar () "z"]
          ]

        runTestExhaustive
          "| { x = false, y = false } \
          \| { x = false, y = true } \
          \| { x = true, y = false } \
          \| { x = true, y = true }"
          -- "
          True -- exhaustive
          [ [pRec () (pExt () "x" (pLit () (IBool False)) (pExt () "y" (pLit () (IBool False)) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool False)) (pExt () "y" (pLit () (IBool True)) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool True)) (pExt () "y" (pLit () (IBool False)) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool True)) (pExt () "y" (pLit () (IBool True)) (pNil ())))]
          ]

        runTestExhaustive
          "| { x = false, y = false } \
          \| { x = true, y = false }"
          -- "
          False -- not exhaustive
          [ [pRec () (pExt () "x" (pLit () (IBool False)) (pExt () "y" (pLit () (IBool False)) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool True)) (pExt () "y" (pLit () (IBool False)) (pNil ())))]
          ]

        runTestExhaustive
          "| { x = false, y = false } \
          \| { x = true, y = _ }"
          -- "
          False -- not exhaustive
          [ [pRec () (pExt () "x" (pLit () (IBool False)) (pExt () "y" (pLit () (IBool False)) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool True)) (pExt () "y" (pAny ()) (pNil ())))]
          ]

        runTestExhaustive
          "| { x = _, y = false } \
          \| { x = true, y = _ }"
          -- "
          False -- not exhaustive
          [ [pRec () (pExt () "x" (pAny ()) (pExt () "y" (pLit () (IBool False)) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool True)) (pExt () "y" (pAny ()) (pNil ())))]
          ]

        runTestExhaustive
          "| {}"
          True -- exhaustive
          [ [pRec () (pNil ())]
          ]

        runTestExhaustive
          "| { x = false, y = { z = false, a = false } } \
          \| { x = false, y = { z = false, a = true } } \
          \| { x = false, y = { z = true , a = false } } \
          \| { x = false, y = { z = true , a = true } } \
          \| { x = true, y = { z = false, a = false } } \
          \| { x = true, y = { z = false, a = true } } \
          \| { x = true, y = { z = true , a = false } } \
          \| { x = true, y = { z = true , a = true } }"
          -- "
          True -- exhaustive
          [ [pRec () (pExt () "x" (pLit () (IBool False)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool False)) (pExt () "a" (pLit () (IBool False)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool False)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool False)) (pExt () "a" (pLit () (IBool True)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool False)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool True)) (pExt () "a" (pLit () (IBool False)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool False)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool True)) (pExt () "a" (pLit () (IBool True)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool True)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool False)) (pExt () "a" (pLit () (IBool False)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool True)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool False)) (pExt () "a" (pLit () (IBool True)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool True)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool True)) (pExt () "a" (pLit () (IBool False)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool True)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool True)) (pExt () "a" (pLit () (IBool True)) (pNil ())))) (pNil ())))]
          ]

        runTestExhaustive
          "| { x = false, y = { z = false, a = false } } \
          \| { x = false, y = { z = false, a = true } } \
          \| { x = false, y = { z = true , a = false } } \
          \| { x = false, y = { z = true , a = true } } \
          \| { x = true, y = { z = false, a = false } } \
          \| { x = true, y = { z = false, a = true } } \
          \| { x = true, y = { z = true , a = false } } \
          \| { x = false, y = { z = true , a = true } }"
          -- "
          False -- not exhaustive
          [ [pRec () (pExt () "x" (pLit () (IBool False)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool False)) (pExt () "a" (pLit () (IBool False)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool False)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool False)) (pExt () "a" (pLit () (IBool True)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool False)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool True)) (pExt () "a" (pLit () (IBool False)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool False)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool True)) (pExt () "a" (pLit () (IBool True)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool True)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool False)) (pExt () "a" (pLit () (IBool False)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool True)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool False)) (pExt () "a" (pLit () (IBool True)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool True)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool True)) (pExt () "a" (pLit () (IBool False)) (pNil ())))) (pNil ())))]
          , [pRec () (pExt () "x" (pLit () (IBool False)) (pExt () "y" (pRec () (pExt () "z" (pLit () (IBool True)) (pExt () "a" (pLit () (IBool True)) (pNil ())))) (pNil ())))]
          ]

        runTestExhaustive
          "| { x = x | r }"
          True -- exhaustive
          [ [pRec () (pExt () "x" (pVar () "x") (pVar () "r"))]
          ]

        runTestExhaustive
          "| { x = 'a' | r }"
          False -- not exhaustive
          [ [pRec () (pExt () "x" (pLit () (IChar 'a')) (pVar () "r"))]
          ]

        runTestExhaustive
          "| { x = 'a' or 'b' | r }"
          False -- not exhaustive
          [ [pRec () (pExt () "x" (pOr () (pLit () (IChar 'a')) (pLit () (IChar 'b'))) (pVar () "r"))]
          ]

      describe "Combined patterns" $ do
        runTestExhaustive
          "| (1 or 2) as x"
          False -- not exhaustive
          [ [pAs () "x" (pOr () (pLit () (IInt 1)) (pLit () (IInt 2)))]
          ]

        runTestExhaustive
          "| (1 or 2) as x \
          \| _ as foo"
          -- "
          True -- exhaustive
          [ [pAs () "x" (pOr () (pLit () (IInt 1)) (pLit () (IInt 2)))]
          , [pAs () "foo" (pAny ())]
          ]

        runTestExhaustive
          "| ({}, {})"
          True -- exhaustive
          [ [pTup () [pRec () (pNil ()), pRec () (pNil ())]]
          ]

        runTestExhaustive
          "| { a = ({}, {}) }"
          True -- exhaustive
          [ [pRec () (pExt () "a" (pTup () [pRec () (pNil ()), pRec () (pNil ())]) (pNil ()))]
          ]

        runTestExhaustive
          "| { a = ({}, true) }"
          False -- not exhaustive
          [ [pRec () (pExt () "a" (pTup () [pRec () (pNil ()), pLit () (IBool True)]) (pNil ()))]
          ]

        runTestExhaustive
          "| { a = ({}, true) }\
          \| { a = ({}, false) }"
          -- "
          True -- exhaustive
          [ [pRec () (pExt () "a" (pTup () [pRec () (pNil ()), pLit () (IBool True)]) (pNil ()))]
          , [pRec () (pExt () "a" (pTup () [pRec () (pNil ()), pLit () (IBool False)]) (pNil ()))]
          ]

        runTestExhaustive
          "| { a = ({}, true) } \
          \| { a = _ }"
          -- "
          True -- exhaustive
          [ [pRec () (pExt () "a" (pTup () [pRec () (pNil ()), pLit () (IBool True)]) (pNil ()))]
          , [pRec () (pExt () "a" (pAny ()) (pNil ()))]
          ]

        runTestExhaustive
          "| { a = x :: xs } \
          \| { a = [] }"
          -- "
          True -- exhaustive
          [ [pRec () (pExt () "a" (pCon () "(::)" [pVar () "x", pVar () "xs"]) (pNil ()))]
          , [pRec () (pExt () "a" (pCon () "[]" []) (pNil ()))]
          ]

        runTestExhaustive
          "| { a = x :: xs } \
          \| { a = x :: _ }"
          -- "
          False -- not exhaustive
          [ [pRec () (pExt () "a" (pCon () "(::)" [pVar () "x", pVar () "xs"]) (pNil ()))]
          , [pRec () (pExt () "a" (pCon () "(::)" [pVar () "x", pAny ()]) (pNil ()))]
          ]

        runTestExhaustive
          "| ({ x = 'a' or 'b' | r }, { x = 'a' or 'b' | r })"
          False -- not exhaustive
          [
            [ pTup
                ()
                [ pRec
                    ()
                    ( pExt
                        ()
                        "x"
                        (pOr () (pLit () (IChar 'a')) (pLit () (IChar 'b')))
                        (pVar () "r")
                    )
                , pRec
                    ()
                    ( pExt
                        ()
                        "x"
                        (pOr () (pLit () (IChar 'a')) (pLit () (IChar 'b')))
                        (pVar () "r")
                    )
                ]
            ]
          ]

        runTestExhaustive
          "| ({ x = 'a' or 'b' | r }, { x = 'a' or 'b' | r }) \
          \| (_, _)"
          -- "
          True -- exhaustive
          [
            [ pTup
                ()
                [ pRec
                    ()
                    ( pExt
                        ()
                        "x"
                        (pOr () (pLit () (IChar 'a')) (pLit () (IChar 'b')))
                        (pVar () "r")
                    )
                , pRec
                    ()
                    ( pExt
                        ()
                        "x"
                        (pOr () (pLit () (IChar 'a')) (pLit () (IChar 'b')))
                        (pVar () "r")
                    )
                ]
            ]
          , [pTup () [pAny (), pAny ()]]
          ]

        runTestExhaustive
          "| [()]"
          False -- not exhaustive
          [ [pList () [pLit () IUnit]]
          ]

        runTestExhaustive
          "| [()] \
          \| []"
          -- "
          False -- not exhaustive
          [ [pList () [pLit () IUnit]]
          , [pList () []]
          ]

        runTestExhaustive
          "| [()] \
          \| [] \
          \| _ :: _ :: _"
          -- "
          True -- exhaustive
          [ [pList () [pLit () IUnit]]
          , [pList () []]
          , [pCon () "(::)" [pAny (), pCon () "(::)" [pAny (), pAny ()]]]
          ]

      describe "stage1" $ do
        describe "Expr" $ do
          let expr1 ::
                Expr
                  (Type Int)
                  ()
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr1 =
                tup
                  (tup () [tInt, tBool])
                  [ eLit tInt (IInt 1)
                  , eLit tBool (IBool True)
                  ]
              expr2 ::
                Expr
                  (Type Int)
                  ()
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr2 =
                eApp
                  (tApp kTyp (tApp kFun1 (tCon kFun2 "(,)") tInt) tBool)
                  ( eCon
                      ( tInt
                          ~> tBool
                          ~> tApp kTyp (tApp kFun1 (tCon kFun2 "(,)") tInt) tBool
                      )
                      "(,)"
                  )
                  [ eLit tInt (IInt 1)
                  , eLit tBool (IBool True)
                  ]
           in it
                "(1, true)              -->  ((,) 1) true"
                (desugar expr1 == expr2)

          let expr1 ::
                Expr
                  (Type Int)
                  ()
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr1 =
                eList
                  (tList tInt)
                  [ eLit tInt (IInt 1)
                  , eLit tInt (IInt 2)
                  , eLit tInt (IInt 3)
                  ]
              expr2 ::
                Expr
                  (Type Int)
                  ()
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr2 =
                eApp
                  (tList tInt)
                  (eCon (tInt ~> tList tInt ~> tList tInt) "(::)")
                  [ eLit tInt (IInt 1)
                  , eApp
                      (tList tInt)
                      (eCon (tInt ~> tList tInt ~> tList tInt) "(::)")
                      [ eLit tInt (IInt 2)
                      , eApp
                          (tList tInt)
                          (eCon (tInt ~> tList tInt ~> tList tInt) "(::)")
                          [ eLit tInt (IInt 3)
                          , eCon
                              (tList tInt)
                              "[]"
                          ]
                      ]
                  ]
           in it
                "[1, 2, 3]              -->  (::) 1 ((::) 2 ((::) 3 []))"
                (desugar expr1 == expr2)

          let expr1 ::
                Expr
                  (Type Int)
                  ()
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr1 =
                eRec
                  (tRec (tExt "a" tInt (tExt "b" tBool tNil)))
                  ( eExt
                      (tExt "a" tInt (tExt "b" tBool tNil))
                      "a"
                      (eLit tInt (IInt 1))
                      ( eExt
                          (tExt "b" tBool tNil)
                          "b"
                          (eLit tBool (IBool True))
                          (eNil rNil)
                      )
                  )
              expr2 ::
                Expr
                  (Type Int)
                  ()
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr2 =
                eApp
                  (tRec (tExt "a" tInt (tExt "b" tBool tNil)))
                  (eCon (tExt "a" tInt (tExt "b" tBool tNil) ~> tRec (tExt "a" tInt (tExt "b" tBool tNil))) "#Record")
                  [ eApp
                      (tExt "a" tInt (tExt "b" tBool tNil))
                      (eCon (tInt ~> tExt "b" tBool tNil ~> tExt "a" tInt (tExt "b" tBool tNil)) "{a}")
                      [ eLit tInt (IInt 1)
                      , eApp
                          (tExt "b" tBool tNil)
                          (eCon (tBool ~> tNil ~> tExt "b" tBool tNil) "{b}")
                          [ eLit tBool (IBool True)
                          , eCon tNil "{}"
                          ]
                      ]
                  ]
           in it
                "{ a = 1, b = true }    -->  #Record ({a} 1 ({b} true {}))"
                (desugar expr1 == expr2)

          let expr1 ::
                Expr
                  (Type Int)
                  ()
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr1 =
                eRec
                  (tRec (tExt "b" tBool (tExt "a" tInt tNil)))
                  ( eExt
                      (tExt "b" tBool (tExt "a" tInt tNil))
                      "b"
                      (eLit tBool (IBool True))
                      ( eExt
                          (tExt "a" tInt tNil)
                          "a"
                          (eLit tInt (IInt 1))
                          (eNil rNil)
                      )
                  )
              expr2 ::
                Expr
                  (Type Int)
                  ()
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr2 =
                eApp
                  (tRec (tExt "a" tInt (tExt "b" tBool tNil)))
                  (eCon (tExt "a" tInt (tExt "b" tBool tNil) ~> tRec (tExt "a" tInt (tExt "b" tBool tNil))) "#Record")
                  [ eApp
                      (tExt "a" tInt (tExt "b" tBool tNil))
                      (eCon (tInt ~> tExt "b" tBool tNil ~> tExt "a" tInt (tExt "b" tBool tNil)) "{a}")
                      [ eLit tInt (IInt 1)
                      , eApp
                          (tExt "b" tBool tNil)
                          (eCon (tBool ~> tNil ~> tExt "b" tBool tNil) "{b}")
                          [ eLit tBool (IBool True)
                          , eCon tNil "{}"
                          ]
                      ]
                  ]
           in it
                "{ b = true, a = 1 }    -->  #Record ({a} 1 ({b} true {}))"
                (desugar expr1 == expr2)

          let expr1 ::
                Expr
                  (Type Int)
                  ()
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr1 = eRec (tRec tNil) (eNil rNil)
              expr2 ::
                Expr
                  (Type Int)
                  ()
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr2 =
                eApp
                  (tRec tNil)
                  (eCon (tNil ~> tRec tNil) "#Record")
                  [ eCon tNil "{}"
                  ]
           in it
                "{}                     -->  #Record {}"
                (desugar expr1 == expr2)

        describe "Nested patterns" $ do
          let expr1 ::
                Expr
                  (Type Int)
                  ()
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr1 =
                ePat
                  tInt
                  (eVar (tup () [tInt, tBool]) "p")
                  [ Clause tInt [tup (tup () [tInt, tBool]) [pLit tInt (IInt 1), pLit tBool (IBool True)]] [Choice [] (eLit tInt (IInt 1))]
                  , Clause tInt [tup (tup () [tInt, tBool]) [pAny tInt, pAny tBool]] [Choice [] (eLit tInt (IInt 2))]
                  ]

              expr2 ::
                Expr
                  (Type Int)
                  ()
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr2 =
                ePat
                  tInt
                  (eVar (tup () [tInt, tBool]) "p")
                  [ Clause
                      tInt
                      [ pCon
                          (tApp kTyp (tApp kFun1 (tCon kFun2 "(,)") tInt) tBool)
                          "(,)"
                          [ pLit tInt (IInt 1)
                          , pLit tBool (IBool True)
                          ]
                      ]
                      [Choice [] (eLit tInt (IInt 1))]
                  , Clause
                      tInt
                      [ pCon
                          (tApp kTyp (tApp kFun1 (tCon kFun2 "(,)") tInt) tBool)
                          "(,)"
                          [ pAny tInt
                          , pAny tBool
                          ]
                      ]
                      [Choice [] (eLit tInt (IInt 2))]
                  ]
           in it
                "match p { (1, true) => 1 | (_, _) => 2 }  -->  match p { ((,) 1) true => 1 | ((,) _) _ => 2 }"
                (desugar expr1 == expr2)

          let expr1 ::
                Expr
                  (Type Int)
                  [Pattern (Type Int)]
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr1 =
                eLam
                  tInt
                  [ pList
                      (tList tInt)
                      [ pVar tInt "x"
                      , pLit tInt (IInt 2)
                      , pLit tInt (IInt 3)
                      ]
                  ]
                  (eVar tInt "x")

              expr2 ::
                Expr
                  (Type Int)
                  [Pattern (Type Int)]
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr2 =
                eLam
                  tInt
                  [ pCon
                      (tList tInt)
                      "(::)"
                      [ pVar tInt "x"
                      , pCon
                          (tList tInt)
                          "(::)"
                          [ pLit tInt (IInt 2)
                          , pCon
                              (tList tInt)
                              "(::)"
                              [ pLit tInt (IInt 3)
                              , pCon
                                  (tList tInt)
                                  "[]"
                                  []
                              ]
                          ]
                      ]
                  ]
                  (eVar tInt "x")
           in it
                "lam([ x, 2, 3 ]) => x                     -->  lam((::) x ((::) 2 ((::) 3 []))) => x"
                (desugar expr1 == expr2)

          let expr1 ::
                Expr
                  (Type Int)
                  [Pattern (Type Int)]
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr1 =
                eLet
                  tBool
                  (BPat (tup () [tInt, tInt]) (pTup (tup () [tInt, tInt]) [pVar tInt "a", pVar tInt "b"]))
                  (eVar (tup () [tInt, tInt]) "e1")
                  (eVar tBool "e2")

              expr2 ::
                Expr
                  (Type Int)
                  [Pattern (Type Int)]
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr2 =
                eLet
                  tBool
                  ( BPat
                      (tup () [tInt, tInt])
                      ( pCon
                          (tApp kTyp (tApp kFun1 (tCon kFun2 "(,)") tInt) tInt)
                          "(,)"
                          [ pVar tInt "a"
                          , pVar tInt "b"
                          ]
                      )
                  )
                  (eVar (tup () [tInt, tInt]) "e1")
                  (eVar tBool "e2")
           in it
                "let (a, b) = e1 in e2                     -->  let (((,) a) b) = e1 in e2"
                (desugar expr1 == expr2)

          let expr1 ::
                Expr
                  (Type Int)
                  [Pattern (Type Int)]
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr1 =
                eList
                  (tList tBool)
                  [ eLet
                      tBool
                      (BPat (tup () [tInt, tInt]) (pTup (tup () [tInt, tInt]) [pVar tInt "a", pVar tInt "b"]))
                      (eVar (tup () [tInt, tInt]) "e1")
                      (eVar tBool "e2")
                  ]

              expr2 ::
                Expr
                  (Type Int)
                  [Pattern (Type Int)]
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Clause (Type Int) [Pattern (Type Int)])
                  (Binding (Type Int))
              expr2 =
                eApp
                  (tList tBool)
                  (eCon (tBool ~> tList tBool ~> tList tBool) "(::)")
                  [ eLet
                      tBool
                      ( BPat
                          (tup () [tInt, tInt])
                          ( pCon
                              (tApp kTyp (tApp kFun1 (tCon kFun2 "(,)") tInt) tInt)
                              "(,)"
                              [ pVar tInt "a"
                              , pVar tInt "b"
                              ]
                          )
                      )
                      (eVar (tup () [tInt, tInt]) "e1")
                      (eVar tBool "e2")
                  , eCon (tList tBool) "[]"
                  ]
           in it
                "[ let (a, b) = e1 in e2 ]                 -->  [ let (((,) a) b) = e1 in e2 ]"
                (desugar expr1 == expr2)

        describe "Pattern" $ do
          let pattern1 :: Pattern (Type Int)
              pattern1 =
                pTup
                  (tup () [tInt, tInt])
                  [ pLit tInt (IInt 1)
                  , pLit tInt (IInt 2)
                  ]
              pattern2 :: Pattern (Type Int)
              pattern2 =
                pCon
                  (tApp kTyp (tApp kFun1 (tCon kFun2 "(,)") tInt) tInt)
                  "(,)"
                  [ pLit tInt (IInt 1)
                  , pLit tInt (IInt 2)
                  ]
           in it
                "| (1, 2)               -->  | ((,) 1) 2"
                (desugar pattern1 == pattern2)

          let pattern1 :: Pattern (Type Int)
              pattern1 =
                pList
                  (tList tInt)
                  [ pLit tInt (IInt 1)
                  , pLit tInt (IInt 2)
                  , pLit tInt (IInt 3)
                  ]
              pattern2 :: Pattern (Type Int)
              pattern2 =
                pCon
                  (tList tInt)
                  "(::)"
                  [ pLit tInt (IInt 1)
                  , pCon
                      (tList tInt)
                      "(::)"
                      [ pLit tInt (IInt 2)
                      , pCon
                          (tList tInt)
                          "(::)"
                          [ pLit tInt (IInt 3)
                          , pCon
                              (tList tInt)
                              "[]"
                              []
                          ]
                      ]
                  ]
           in it
                "| [1, 2, 3]            -->  | ((::) 1 ((::) 2 ((::) 3 [])))"
                (desugar pattern1 == pattern2)

          let pattern1 :: Pattern (Type Int)
              pattern1 =
                pRec
                  (tRec (tExt "a" tInt (tExt "b" tBool tNil)))
                  ( pExt
                      (tExt "a" tInt (tExt "b" tBool tNil))
                      "a"
                      (pLit tInt (IInt 1))
                      ( pExt
                          (tExt "b" tBool tNil)
                          "b"
                          (pLit tBool (IBool True))
                          (pNil rNil)
                      )
                  )
              pattern2 :: Pattern (Type Int)
              pattern2 =
                pCon
                  (tRec (tExt "a" tInt (tExt "b" tBool tNil)))
                  "#Record"
                  [ pCon
                      (tExt "a" tInt (tExt "b" tBool tNil))
                      "{a}"
                      [ pLit tInt (IInt 1)
                      , pCon
                          (tExt "b" tBool tNil)
                          "{b}"
                          [ pLit tBool (IBool True)
                          , pCon tNil "{}" []
                          ]
                      ]
                  ]
           in it
                "| { a = 1, b = true }  -->  | #Record ({a} 1 ({b} true {}))"
                (desugar pattern1 == pattern2)

          let pattern1 :: Pattern (Type Int)
              pattern1 =
                pRec
                  (tRec (tExt "b" tBool (tExt "a" tInt tNil)))
                  ( pExt
                      (tExt "b" tBool (tExt "a" tInt tNil))
                      "b"
                      (pLit tBool (IBool True))
                      ( pExt
                          (tExt "a" tInt tNil)
                          "a"
                          (pLit tInt (IInt 1))
                          (pNil rNil)
                      )
                  )
              pattern2 :: Pattern (Type Int)
              pattern2 =
                pCon
                  (tRec (tExt "a" tInt (tExt "b" tBool tNil)))
                  "#Record"
                  [ pCon
                      (tExt "a" tInt (tExt "b" tBool tNil))
                      "{a}"
                      [ pLit tInt (IInt 1)
                      , pCon
                          (tExt "b" tBool tNil)
                          "{b}"
                          [ pLit tBool (IBool True)
                          , pCon tNil "{}" []
                          ]
                      ]
                  ]
           in it
                "| { b = true, a = 1 }  -->  | #Record ({a} 1 ({b} true {}))"
                (desugar pattern1 == pattern2)

          let pattern1 :: Pattern (Type Int)
              pattern1 = pRec (tRec tNil) (pNil rNil)
              pattern2 :: Pattern (Type Int)
              pattern2 = pCon (tRec tNil) "#Record" [pCon tNil "{}" []]
           in it
                "| {}                   -->  | #Record {}"
                (desugar pattern1 == pattern2)

    ---------------------------------------------------------------------------
    describe "clauseGroups" $ do
      it "" $ do
        True
    ---------------------------------------------------------------------------
    describe "labeledClause" $ do
      let clause ::
            Clause
              ()
              [Pattern ()]
              ( Expr
                  ()
                  [Pattern ()]
                  (Clause () [Pattern ()])
                  (Clause () [Pattern ()])
                  (Binding ())
              )
          clause =
            Clause () [pCon () "(::)" [pVar () "x", pVar () "xs"]] []
       in it
            "| x :: xs"
            (LCon clause == labeledClause clause)

      let clause ::
            Clause
              ()
              [Pattern ()]
              ( Expr
                  ()
                  [Pattern ()]
                  (Clause () [Pattern ()])
                  (Clause () [Pattern ()])
                  (Binding ())
              )
          clause =
            Clause () [pVar () "x", pVar () "xs"] []
       in it
            "| x, xs"
            (LVar clause == labeledClause clause)
    ---------------------------------------------------------------------------
    describe "compilePatterns" $ do
      it "match xs { y :: ys => 1 | [] => 2 }" $
        let input :: State Int (Expr () Name (CaseClause ()) Void1 (Binding ()))
            input =
              compilePatterns
                (eVar () "xs")
                [ Clause () [pCon () "(::)" [pVar () "y", pVar () "ys"]] [Choice [] (eLit () (IInt 1))]
                , Clause () [pCon () "[]" []] [Choice [] (eLit () (IInt 2))]
                ]
            expr :: Expr () Name (CaseClause ()) Void1 (Binding ())
            expr =
              ePat
                ()
                (eVar () "xs")
                [ Case () "(::)" ["$p1", "$p2"] (eLit () (IInt 1))
                , Case () "[]" [] (eLit () (IInt 2))
                ]
         in (expr == evalState input 1)

      it "match xs { y :: ys => true | [] => false }" $
        let input :: State Int (Expr (Type Int) Name (CaseClause (Type Int)) Void1 (Binding (Type Int)))
            input =
              compilePatterns
                (eVar (tList tInt) "xs")
                [ Clause tBool [pCon (tList tInt) "(::)" [pVar tInt "y", pVar (tList tInt) "ys"]] [Choice [] (eLit tBool (IBool True))]
                , Clause tBool [pCon (tList tInt) "[]" []] [Choice [] (eLit tBool (IBool False))]
                ]
            expr :: Expr (Type Int) Name (CaseClause (Type Int)) Void1 (Binding (Type Int))
            expr =
              ePat
                tBool
                (eVar (tList tInt) "xs")
                [ Case (tInt ~> tList tInt ~> tList tInt) "(::)" ["$p1", "$p2"] (eLit tBool (IBool True))
                , Case (tList tInt) "[]" [] (eLit tBool (IBool False))
                ]
         in (expr == evalState input 1)

      it "match 123 { x when x == 456 => true, when true => false }" $
        let input :: State Int (Expr () Name (CaseClause ()) Void1 (Binding ()))
            input =
              compilePatterns
                (eLit () (IInt 123))
                [ Clause
                    ()
                    [pVar () "x"]
                    [ Choice [eOp2 () (OEq ()) (eVar () "x") (eLit () (IInt 456))] (eLit () (IBool True))
                    , Choice [eLit () (IBool True)] (eLit () (IBool False))
                    ]
                ]
            expr :: Expr () Name (CaseClause ()) Void1 (Binding ())
            expr =
              eIf
                ()
                (eOp2 () (OEq ()) (eLit () (IInt 123)) (eLit () (IInt 456)))
                (eLit () (IBool True))
                ( eIf
                    ()
                    (eLit () (IBool True))
                    (eLit () (IBool False))
                    (eVar () "<FAIL>")
                )
         in (expr == evalState input 1)
    ---------------------------------------------------------------------------
    describe "translateFun" $ do
      it "let f | y :: ys = true | [] = false" $
        let clauses :: [Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 (Binding ()))]
            clauses =
              [ Clause () [pCon () "(::)" [pVar () "y", pVar () "ys"]] [Choice [] (eLit () (IBool True))]
              , Clause () [pCon () "[]" []] [Choice [] (eLit () (IBool False))]
              ]
            expr :: Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 (Binding ())
            expr =
              eLam
                ()
                [pVar () "$v1"]
                ( ePat
                    ()
                    (eVar () "$v1")
                    [ Clause () [pCon () "(::)" [pVar () "y", pVar () "ys"]] [Choice [] (eLit () (IBool True))]
                    , Clause () [pCon () "[]" []] [Choice [] (eLit () (IBool False))]
                    ]
                )
         in (expr == translateFun clauses)

      it "let f | y :: ys = true | [] = false" $
        let clauses :: [Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 (Binding (Type ())))]
            clauses =
              [ Clause tBool [pCon (tList tInt) "(::)" [pVar tInt "y", pVar (tList tInt) "ys"]] [Choice [] (eLit tBool (IBool True))]
              , Clause tBool [pCon (tList tInt) "[]" []] [Choice [] (eLit tBool (IBool False))]
              ]
            expr :: Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 (Binding (Type ()))
            expr =
              eLam
                (tList tInt ~> tBool)
                [pVar (tList tInt) "$v1"]
                ( ePat
                    tBool
                    (eVar (tList tInt) "$v1")
                    [ Clause tBool [pCon (tList tInt) "(::)" [pVar tInt "y", pVar (tList tInt) "ys"]] [Choice [] (eLit tBool (IBool True))]
                    , Clause tBool [pCon (tList tInt) "[]" []] [Choice [] (eLit tBool (IBool False))]
                    ]
                )
         in (expr == translateFun clauses)

      it "let f | (1, a) = 1 | (_, _) = 2" $
        let clauses :: [Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 (Binding ()))]
            clauses =
              [ Clause
                  ()
                  [pLit () (IInt 1), pVar () "a"]
                  [Choice [] (eLit () (IInt 1))]
              , Clause
                  ()
                  [pAny (), pAny ()]
                  [Choice [] (eLit () (IInt 2))]
              ]
            expr :: Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 (Binding ())
            expr =
              eLam
                ()
                [pVar () "$v1", pVar () "$v2"]
                ( ePat
                    ()
                    (eTup () [eVar () "$v1", eVar () "$v2"])
                    [ Clause
                        ()
                        [pTup () [pLit () (IInt 1), pVar () "a"]]
                        [Choice [] (eLit () (IInt 1))]
                    , Clause
                        ()
                        [pTup () [pAny (), pAny ()]]
                        [Choice [] (eLit () (IInt 2))]
                    ]
                )
         in (expr == translateFun clauses)

      it "let f | (1, a) = 1 | (_, _) = 2" $
        let clauses :: [Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 (Binding (Type ())))]
            clauses =
              [ Clause
                  tInt
                  [pLit tInt (IInt 1), pVar tInt "a"]
                  [Choice [] (eLit tInt (IInt 1))]
              , Clause
                  tInt
                  [pAny tInt, pAny tInt]
                  [Choice [] (eLit tInt (IInt 2))]
              ]
            expr :: Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 (Binding (Type ()))
            expr =
              eLam
                (tInt ~> tInt ~> tInt)
                [pVar tInt "$v1", pVar tInt "$v2"]
                ( ePat
                    tInt
                    (eTup (tup () [tInt, tInt]) [eVar tInt "$v1", eVar tInt "$v2"])
                    [ Clause
                        tInt
                        [pTup (tup () [tInt, tInt]) [pLit tInt (IInt 1), pVar tInt "a"]]
                        [Choice [] (eLit tInt (IInt 1))]
                    , Clause
                        tInt
                        [pTup (tup () [tInt, tInt]) [pAny tInt, pAny tInt]]
                        [Choice [] (eLit tInt (IInt 2))]
                    ]
                )
         in (expr == translateFun clauses)

      it "let f | (1, true, a) = () | (_, _, _) = ()" $
        let clauses :: [Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 (Binding (Type ())))]
            clauses =
              [ Clause
                  tUnit
                  [pLit tFloat (IFloat 1), pLit tBool (IBool True), pVar tInt "a"]
                  [Choice [] (eLit tUnit IUnit)]
              , Clause
                  tUnit
                  [pAny tFloat, pAny tBool, pAny tInt]
                  [Choice [] (eLit tUnit IUnit)]
              ]
            expr :: Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 (Binding (Type ()))
            expr =
              eLam
                (tFloat ~> tBool ~> tInt ~> tUnit)
                [pVar tFloat "$v1", pVar tBool "$v2", pVar tInt "$v3"]
                ( ePat
                    tUnit
                    (eTup (tup () [tFloat, tBool, tInt]) [eVar tFloat "$v1", eVar tBool "$v2", eVar tInt "$v3"])
                    [ Clause
                        tUnit
                        [pTup (tup () [tFloat, tBool, tInt]) [pLit tFloat (IFloat 1), pLit tBool (IBool True), pVar tInt "a"]]
                        [Choice [] (eLit tUnit IUnit)]
                    , Clause
                        tUnit
                        [pTup (tup () [tFloat, tBool, tInt]) [pAny tFloat, pAny tBool, pAny tInt]]
                        [Choice [] (eLit tUnit IUnit)]
                    ]
                )
         in (expr == translateFun clauses)

    ---------------------------------------------------------------------------
    describe "translateLam" $ do
      describe "Untyped" $ do
        it
          "lam[] => e               -->  e"
          $ let expr :: Expr () Name (Clause () [Pattern ()]) Void1 Void
                expr = translateLam () [] (eVar () "e")
             in (eVar () "e" == expr)
        it
          "lam(x) => e              -->  lam(x) => e"
          $ let expr :: Expr () Name (Clause () [Pattern ()]) Void1 Void
                expr = translateLam () [pVar () "x"] (eVar () "e")
                result :: Expr () Name (Clause () [Pattern ()]) Void1 Void
                result = eLam () "x" (eVar () "e")
             in (result == expr)
        it
          "lam(x, y) => e           -->  lam(x) => lam(y) => e"
          $ let expr :: Expr () Name (Clause () [Pattern ()]) Void1 Void
                expr = translateLam () [pVar () "x", pVar () "y"] (eVar () "e")
                result :: Expr () Name (Clause () [Pattern ()]) Void1 Void
                result =
                  eLam
                    ()
                    "x"
                    ( eLam
                        ()
                        "y"
                        (eVar () "e")
                    )
             in (result == expr)
        it
          "lam(x, z :: zs, y) => e  -->  lam(x) => lam($p) => match $p { z :: zs => lam(y) => e }"
          $ let expr :: Expr () Name (Clause () [Pattern ()]) Void1 Void
                expr =
                  translateLam
                    ()
                    [ pVar () "x"
                    , pCon
                        ()
                        "(::)"
                        [ pVar () "z"
                        , pVar () "zs"
                        ]
                    , pVar () "y"
                    ]
                    (eVar () "e")
                result :: Expr () Name (Clause () [Pattern ()]) Void1 Void
                result =
                  eLam
                    ()
                    "x"
                    ( eLam
                        ()
                        "$p"
                        ( ePat
                            ()
                            (eVar () "$p")
                            [ Clause
                                ()
                                [pCon () "(::)" [pVar () "z", pVar () "zs"]]
                                [ Choice
                                    []
                                    (eLam () "y" (eVar () "e"))
                                ]
                            ]
                        )
                    )
             in (result == expr)

      describe "Typed" $ do
        it
          "lam[] => e               -->  e"
          $ let expr :: Expr (Type ()) Name (Clause (Type ()) [Pattern (Type ())]) Void1 Void
                expr = translateLam tInt [] (eVar tInt "e")
             in (eVar tInt "e" == expr)

        it
          "lam(x) => e              -->  lam(x) => e"
          $ let expr :: Expr (Type ()) Name (Clause (Type ()) [Pattern (Type ())]) Void1 Void
                expr =
                  translateLam
                    (tInt ~> tInt)
                    [pVar tInt "x"]
                    (eVar tInt "e")
                result :: Expr (Type ()) Name (Clause (Type ()) [Pattern (Type ())]) Void1 Void
                result =
                  eLam
                    (tInt ~> tInt)
                    "x"
                    (eVar tInt "e")
             in (result == expr)

        it
          "lam(x, y) => e           -->  lam(x) => lam(y) => e"
          $ let expr :: Expr (Type ()) Name (Clause (Type ()) [Pattern (Type ())]) Void1 Void
                expr = translateLam (tInt ~> tInt ~> tBool) [pVar tInt "x", pVar tInt "y"] (eVar tBool "e")
                result :: Expr (Type ()) Name (Clause (Type ()) [Pattern (Type ())]) Void1 Void
                result =
                  eLam
                    (tInt ~> tInt ~> tBool)
                    "x"
                    ( eLam
                        (tInt ~> tBool)
                        "y"
                        (eVar tBool "e")
                    )
             in (result == expr)

        it
          "lam(x, z :: zs, y) => e  -->  lam(x) => lam($p) => match $p { z :: zs => lam(y) => e }"
          $ let expr :: Expr (Type ()) Name (Clause (Type ()) [Pattern (Type ())]) Void1 Void
                expr =
                  translateLam
                    (tInt ~> tList tInt ~> tInt ~> tBool)
                    [ pVar tInt "x"
                    , pCon
                        (tList tInt)
                        "(::)"
                        [ pVar tInt "z"
                        , pVar (tList tInt) "zs"
                        ]
                    , pVar tInt "y"
                    ]
                    (eVar tBool "e")
                result :: Expr (Type ()) Name (Clause (Type ()) [Pattern (Type ())]) Void1 Void
                result =
                  eLam
                    (tInt ~> tList tInt ~> tInt ~> tBool)
                    "x"
                    ( eLam
                        (tList tInt ~> tInt ~> tBool)
                        "$p"
                        ( ePat
                            (tInt ~> tBool)
                            (eVar (tList tInt) "$p")
                            [ Clause
                                (tInt ~> tBool)
                                [pCon (tList tInt) "(::)" [pVar tInt "z", pVar (tList tInt) "zs"]]
                                [ Choice
                                    []
                                    (eLam (tInt ~> tBool) "y" (eVar tBool "e"))
                                ]
                            ]
                        )
                    )
             in (result == expr)

    describe "translateLet" $ do
      describe "Untyped" $ do
        it
          "let v = e1 in e2         -->  fix v = e1 in e2"
          $ let expr :: Expr () Name (Clause () [Pattern ()]) Void1 Void
                expr =
                  translateLet
                    ()
                    ( BPat
                        ()
                        (pVar () "v")
                    )
                    (eVar () "e1")
                    (eVar () "e2")
                result :: Expr () Name (Clause () [Pattern ()]) Void1 Void
                result =
                  eFix () "v" (eVar () "e1") (eVar () "e2")
             in (result == expr)

        it
          "let S(v) = e1 in e2      -->  match e1 { S(v) => e2 }"
          $ let expr :: Expr () Name (Clause () [Pattern ()]) Void1 Void
                expr =
                  translateLet
                    ()
                    ( BPat
                        ()
                        (pCon () "S" [pVar () "v"])
                    )
                    (eVar () "e1")
                    (eVar () "e2")
                result :: Expr () Name (Clause () [Pattern ()]) Void1 Void
                result =
                  ePat
                    ()
                    (eVar () "e1")
                    [ Clause () [pCon () "S" [pVar () "v"]] [Choice [] (eVar () "e2")]
                    ]
             in (result == expr)

        it
          "let f(x, y) = e1 in e2   -->  fix f = lam(x) => lam(y) => e1 in e2"
          $ let expr :: Expr () Name (Clause () [Pattern ()]) Void1 Void
                expr =
                  translateLet
                    ()
                    ( BFun
                        ()
                        "f"
                        [ pVar () "x"
                        , pVar () "y"
                        ]
                    )
                    (eVar () "e1")
                    (eVar () "e2")
                result :: Expr () Name (Clause () [Pattern ()]) Void1 Void
                result =
                  eFix
                    ()
                    "f"
                    ( eLam
                        ()
                        "x"
                        ( eLam
                            ()
                            "y"
                            (eVar () "e1")
                        )
                    )
                    (eVar () "e2")
             in (result == expr)

      describe "Typed" $ do
        it
          "let v = e1 in e2         -->  fix v = e1 in e2"
          $ let expr :: Expr (Type ()) Name (Clause (Type ()) [Pattern (Type ())]) Void1 Void
                expr =
                  translateLet
                    tBool
                    ( BPat
                        tInt
                        (pVar tInt "v")
                    )
                    (eVar tInt "e1")
                    (eVar tBool "e2")
                result :: Expr (Type ()) Name (Clause (Type ()) [Pattern (Type ())]) Void1 Void
                result = eFix tBool "v" (eVar tInt "e1") (eVar tBool "e2")
             in (result == expr)

        it
          "let S(v) = e1 in e2      -->  match e1 { S(v) => e2 }"
          $ let expr :: Expr (Type ()) Name (Clause (Type ()) [Pattern (Type ())]) Void1 Void
                expr =
                  translateLet
                    tBool
                    ( BPat
                        (tApp kTyp (tCon kFun1 "S") tInt)
                        (pCon (tApp kTyp (tCon kFun1 "S") tInt) "S" [pVar tInt "v"])
                    )
                    (eVar (tApp kTyp (tCon kFun1 "S") tInt) "e1")
                    (eVar tBool "e2")
                result :: Expr (Type ()) Name (Clause (Type ()) [Pattern (Type ())]) Void1 Void
                result =
                  ePat
                    tBool
                    (eVar (tApp kTyp (tCon kFun1 "S") tInt) "e1")
                    [ Clause tBool [pCon (tApp kTyp (tCon kFun1 "S") tInt) "S" [pVar tInt "v"]] [Choice [] (eVar tBool "e2")]
                    ]
             in (result == expr)

        it
          "let f(x, y) = e1 in e2   -->  fix f = lam(x) => lam(y) => e1 in e2"
          $ let expr :: Expr (Type ()) Name (Clause (Type ()) [Pattern (Type ())]) Void1 Void
                expr =
                  translateLet
                    tUnit
                    ( BFun
                        (tInt ~> tInt ~> tBool)
                        "f"
                        [ pVar tInt "x"
                        , pVar tInt "y"
                        ]
                    )
                    (eVar tBool "e1")
                    (eVar tUnit "e2")
                result :: Expr (Type ()) Name (Clause (Type ()) [Pattern (Type ())]) Void1 Void
                result =
                  eFix
                    tUnit
                    "f"
                    ( eLam
                        (tInt ~> tInt ~> tBool)
                        "x"
                        ( eLam
                            (tInt ~> tBool)
                            "y"
                            (eVar tBool "e1")
                        )
                    )
                    (eVar tUnit "e2")
             in (result == expr)

    describe "dropOrPatterns" $ do
      it "| [x, _] or [x, _, _] => true       -->  | [x, _] => true | [x, _, _] => true" $
        let clause :: Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)
            clause =
              Clause
                ()
                [ pOr
                    ()
                    (pList () [pVar () "x", pAny ()])
                    (pList () [pVar () "x", pAny (), pAny ()])
                ]
                [Choice [] (eLit () (IBool True))]
            clauses :: [Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)]
            clauses =
              [ Clause () [pList () [pVar () "x", pAny ()]] [Choice [] (eLit () (IBool True))]
              , Clause () [pList () [pVar () "x", pAny (), pAny ()]] [Choice [] (eLit () (IBool True))]
              ]
         in (dropOrPatterns clause == clauses)

      it "| ([x, _] or [x, _, _], 1) => true  -->  | ([x, _], 1) => true | ([x, _, _], 1) => true" $
        let clause :: Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)
            clause =
              Clause
                ()
                [ pTup
                    ()
                    [ pOr
                        ()
                        (pList () [pVar () "x", pAny ()])
                        (pList () [pVar () "x", pAny (), pAny ()])
                    , pLit () (IInt 1)
                    ]
                ]
                [Choice [] (eLit () (IBool True))]
            clauses :: [Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)]
            clauses =
              [ Clause () [pTup () [pList () [pVar () "x", pAny ()], pLit () (IInt 1)]] [Choice [] (eLit () (IBool True))]
              , Clause () [pTup () [pList () [pVar () "x", pAny (), pAny ()], pLit () (IInt 1)]] [Choice [] (eLit () (IBool True))]
              ]
         in (dropOrPatterns clause == clauses)

    describe "dropAnyPatterns" $ do
      it "| [x, _] or [x, _, _] => true       -->  | [x, $_1] or [x, $_2, $_3] => true" $
        let clause :: Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)
            clause =
              Clause
                ()
                [ pOr
                    ()
                    (pList () [pVar () "x", pAny ()])
                    (pList () [pVar () "x", pAny (), pAny ()])
                ]
                [Choice [] (eLit () (IBool True))]
            result :: Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)
            result =
              Clause
                ()
                [ pOr
                    ()
                    (pList () [pVar () "x", pVar () "$_1"])
                    (pList () [pVar () "x", pVar () "$_2", pVar () "$_3"])
                ]
                [Choice [] (eLit () (IBool True))]
         in (dropAnyPatterns clause == result)

    describe "dropLitPatterns" $ do
      it "| [5, _] => e                                         -->  | [$s1, _] when $s1 == 5 => e" $
        let clause :: Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)
            clause =
              Clause
                ()
                [ pList
                    ()
                    [ pLit () (IInt 5)
                    , pAny ()
                    ]
                ]
                [Choice [] (eVar () "e")]
            result :: Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)
            result =
              Clause
                ()
                [ pList
                    ()
                    [ pVar () "$s1"
                    , pAny ()
                    ]
                ]
                [Choice [eOp2 () (OEq ()) (eVar () "$s1") (eLit () (IInt 5))] (eVar () "e")]
         in (dropLitPatterns clause == result)

      it "| [5, _] => e                                         -->  | [$s1, _] when $s1 == 5 => e" $
        let clause :: Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)
            clause =
              Clause
                tBool
                [ pList
                    (tList tInt)
                    [ pLit tInt (IInt 5)
                    , pAny tInt
                    ]
                ]
                [Choice [] (eVar tBool "e")]
            result :: Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)
            result =
              Clause
                tBool
                [ pList
                    (tList tInt)
                    [ pVar tInt "$s1"
                    , pAny tInt
                    ]
                ]
                [Choice [eOp2 tBool (OEq (tInt ~> tInt ~> tBool)) (eVar tInt "$s1") (eLit tInt (IInt 5))] (eVar tBool "e")]
         in (dropLitPatterns clause == result)

      it "| [5, 1] => e                                         -->  | [$s1, $s2] when ($s1 == 5 && $s2 == 1) => e" $
        let clause :: Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)
            clause =
              Clause
                tBool
                [ pList
                    (tList tInt)
                    [ pLit tInt (IInt 5)
                    , pLit tInt (IInt 1)
                    ]
                ]
                [Choice [] (eVar tBool "e")]
            result :: Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)
            result =
              Clause
                tBool
                [ pList
                    (tList tInt)
                    [ pVar tInt "$s1"
                    , pVar tInt "$s2"
                    ]
                ]
                [ Choice
                    [ eOp2 tBool (OEq (tInt ~> tInt ~> tBool)) (eVar tInt "$s1") (eLit tInt (IInt 5))
                    , eOp2 tBool (OEq (tInt ~> tInt ~> tBool)) (eVar tInt "$s2") (eLit tInt (IInt 1))
                    ]
                    (eVar tBool "e")
                ]
         in (dropLitPatterns clause == result)

      it "| [5, _] when a => e1, otherwise => e2                -->  | [$s1, _] when $s1 == 5 && a => e1, when $s1 == 5 => e2" $
        let clause :: Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)
            clause =
              Clause
                ()
                [ pList () [pLit () (IInt 5), pAny ()]
                ]
                [ Choice [eVar () "a"] (eVar () "e1")
                , Choice [] (eVar () "e2")
                ]
            result :: Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)
            result =
              Clause
                ()
                [ pList () [pVar () "$s1", pAny ()]
                ]
                [ Choice [eVar () "a", eOp2 () (OEq ()) (eVar () "$s1") (eLit () (IInt 5))] (eVar () "e1")
                , Choice [eOp2 () (OEq ()) (eVar () "$s1") (eLit () (IInt 5))] (eVar () "e2")
                ]
         in (dropLitPatterns clause == result)

      it "| [5, _] when a => e1, otherwise => e2                -->  | [$s1, _] when $s1 == 5 && a => e1, when $s1 == 5 => e2" $
        let clause :: Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)
            clause =
              Clause
                tBool
                [ pList (tList tInt) [pLit tInt (IInt 5), pAny tInt]
                ]
                [ Choice [eVar tBool "a"] (eVar tBool "e1")
                , Choice [] (eVar tBool "e2")
                ]
            result :: Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)
            result =
              Clause
                tBool
                [ pList (tList tInt) [pVar tInt "$s1", pAny tInt]
                ]
                [ Choice [eVar tBool "a", eOp2 tBool (OEq (tInt ~> tInt ~> tBool)) (eVar tInt "$s1") (eLit tInt (IInt 5))] (eVar tBool "e1")
                , Choice [eOp2 tBool (OEq (tInt ~> tInt ~> tBool)) (eVar tInt "$s1") (eLit tInt (IInt 5))] (eVar tBool "e2")
                ]
         in (dropLitPatterns clause == result)

      it "| [5, _] when a => e1, when b => e2, otherwise => e3  -->  | [$s1, _] when $s1 == 5 && a => e1, when $s1 == 5 && b => e2, when $s1 == 5 => e3" $
        let clause :: Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)
            clause =
              Clause
                ()
                [ pList () [pLit () (IInt 5), pAny ()]
                ]
                [ Choice [eVar () "a"] (eVar () "e1")
                , Choice [eVar () "b"] (eVar () "e2")
                , Choice [] (eVar () "e3")
                ]
            result :: Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)
            result =
              Clause
                ()
                [ pList () [pVar () "$s1", pAny ()]
                ]
                [ Choice [eVar () "a", eOp2 () (OEq ()) (eVar () "$s1") (eLit () (IInt 5))] (eVar () "e1")
                , Choice [eVar () "b", eOp2 () (OEq ()) (eVar () "$s1") (eLit () (IInt 5))] (eVar () "e2")
                , Choice [eOp2 () (OEq ()) (eVar () "$s1") (eLit () (IInt 5))] (eVar () "e3")
                ]
         in (dropLitPatterns clause == result)

      it "| [5, _] when a => e1, when b => e2, otherwise => e3  -->  | [$s1, _] when $s1 == 5 && a => e1, when $s1 == 5 && b => e2, when $s1 == 5 => e3" $
        let clause :: Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)
            clause =
              Clause
                tBool
                [ pList (tList tInt) [pLit tInt (IInt 5), pAny tInt]
                ]
                [ Choice [eVar tBool "a"] (eVar tBool "e1")
                , Choice [eVar tBool "b"] (eVar tBool "e2")
                , Choice [] (eVar tBool "e3")
                ]
            result :: Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)
            result =
              Clause
                tBool
                [ pList (tList tInt) [pVar tInt "$s1", pAny tInt]
                ]
                [ Choice [eVar tBool "a", eOp2 tBool (OEq (tInt ~> tInt ~> tBool)) (eVar tInt "$s1") (eLit tInt (IInt 5))] (eVar tBool "e1")
                , Choice [eVar tBool "b", eOp2 tBool (OEq (tInt ~> tInt ~> tBool)) (eVar tInt "$s1") (eLit tInt (IInt 5))] (eVar tBool "e2")
                , Choice [eOp2 tBool (OEq (tInt ~> tInt ~> tBool)) (eVar tInt "$s1") (eLit tInt (IInt 5))] (eVar tBool "e3")
                ]
         in (dropLitPatterns clause == result)

    describe "dropAsPatterns" $ do
      it "| [x, _, _] => e1 | _ => e2" $
        --    | [x, _, _] => e1
        --    | _         => e2
        --
        let clauses :: [Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)]
            clauses =
              [ Clause () [pList () [pVar () "x", pAny (), pAny ()]] [Choice [] (eVar () "e1")]
              , Clause () [pAny ()] [Choice [] (eVar () "e2")]
              ]
         in (dropAsPatterns clauses == clauses)

      it "| [x, _, _] as xs => e1 | _ => e2" $
        --    | [x, _, _] as xs => e1
        --    | _               => e2
        --
        --    | xs =>
        --        match xs {
        --          | [x, _, _] => e1
        --          | _         => e2
        --        }
        --    | _ => e2
        --
        let clauses :: [Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)]
            clauses =
              [ Clause () [pAs () "xs" (pList () [pVar () "x", pAny (), pAny ()])] [Choice [] (eVar () "e1")]
              , Clause () [pAny ()] [Choice [] (eVar () "e2")]
              ]
            result :: [Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)]
            result =
              [ Clause
                  ()
                  [pVar () "xs"]
                  [ Choice
                      []
                      ( ePat
                          ()
                          (eVar () "xs")
                          [ Clause
                              ()
                              [pList () [pVar () "x", pAny (), pAny ()]]
                              [Choice [] (eVar () "e1")]
                          , Clause () [pAny ()] [Choice [] (eVar () "e2")]
                          ]
                      )
                  ]
              , Clause () [pAny ()] [Choice [] (eVar () "e2")]
              ]
         in (dropAsPatterns clauses == result)

      it "| [x, _, _] as xs => e1 | _ => e2" $
        --    | [x, _, _] as xs => e1
        --    | _               => e2
        --
        --    | xs =>
        --        match xs {
        --          | [x, _, _] => e1
        --          | _         => e2
        --        }
        --    | _ => e2
        --
        let clauses :: [Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)]
            clauses =
              [ Clause tBool [pAs (tList tInt) "xs" (pList (tList tInt) [pVar tInt "x", pAny tInt, pAny tInt])] [Choice [] (eVar tBool "e1")]
              , Clause tBool [pAny (tList tInt)] [Choice [] (eVar tBool "e2")]
              ]
            result :: [Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)]
            result =
              [ Clause
                  tBool
                  [pVar (tList tInt) "xs"]
                  [ Choice
                      []
                      ( ePat
                          tBool
                          (eVar (tList tInt) "xs")
                          [ Clause
                              tBool
                              [pList (tList tInt) [pVar tInt "x", pAny tInt, pAny tInt]]
                              [Choice [] (eVar tBool "e1")]
                          , Clause tBool [pAny (tList tInt)] [Choice [] (eVar tBool "e2")]
                          ]
                      )
                  ]
              , Clause tBool [pAny (tList tInt)] [Choice [] (eVar tBool "e2")]
              ]
         in (dropAsPatterns clauses == result)

      it "| ([x, _, _] as xs, []) => e1 | _ => e2" $
        --    | ([x, _, _] as xs, []) => e1
        --    | _                     => e2
        --
        --    | (xs, []) =>
        --        match xs {
        --          | [x, _, _] => e1
        --          | _         => e2
        --        }
        --    | _ => e2
        --
        let clauses :: [Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)]
            clauses =
              [ Clause () [pTup () [pAs () "xs" (pList () [pVar () "x", pAny (), pAny ()]), pList () []]] [Choice [] (eVar () "e1")]
              , Clause () [pAny ()] [Choice [] (eVar () "e2")]
              ]
            result :: [Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)]
            result =
              [ Clause
                  ()
                  [pTup () [pVar () "xs", pList () []]]
                  [ Choice
                      []
                      ( ePat
                          ()
                          (eVar () "xs")
                          [ Clause
                              ()
                              [pList () [pVar () "x", pAny (), pAny ()]]
                              [Choice [] (eVar () "e1")]
                          , Clause () [pAny ()] [Choice [] (eVar () "e2")]
                          ]
                      )
                  ]
              , Clause () [pAny ()] [Choice [] (eVar () "e2")]
              ]
         in (dropAsPatterns clauses == result)

      it "| ([x, _, _] as xs, []) => e1 | _ => e2" $
        --    | ([x, _, _] as xs, []) => e1
        --    | _                     => e2
        --
        --    | (xs, []) =>
        --        match xs {
        --          | [x, _, _] => e1
        --          | _         => e2
        --        }
        --    | _ => e2
        --
        let clauses :: [Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)]
            clauses =
              [ Clause tBool [pTup (tup () [tList tInt, tList tInt]) [pAs (tList tInt) "xs" (pList (tList tInt) [pVar tInt "x", pAny tInt, pAny tInt]), pList (tList tInt) []]] [Choice [] (eVar tBool "e1")]
              , Clause tBool [pAny (tup () [tList tInt, tList tInt])] [Choice [] (eVar tBool "e2")]
              ]
            result :: [Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)]
            result =
              [ Clause
                  tBool
                  [pTup (tup () [tList tInt, tList tInt]) [pVar (tList tInt) "xs", pList (tList tInt) []]]
                  [ Choice
                      []
                      ( ePat
                          tBool
                          (eVar (tList tInt) "xs")
                          [ Clause
                              tBool
                              [pList (tList tInt) [pVar tInt "x", pAny tInt, pAny tInt]]
                              [Choice [] (eVar tBool "e1")]
                          , Clause tBool [pAny (tup () [tList tInt, tList tInt])] [Choice [] (eVar tBool "e2")]
                          ]
                      )
                  ]
              , Clause tBool [pAny (tup () [tList tInt, tList tInt])] [Choice [] (eVar tBool "e2")]
              ]
         in (dropAsPatterns clauses == result)

      it "| [(1 :: _) as xs, (2 :: _) as ys, (3 :: _) as zs] => e1 | _ => e2" $
        --    | [(1 :: _) as xs, (2 :: _) as ys, (3 :: _) as zs] => e1
        --    | _                                                => e2
        --
        --    | [xs, ys, zs] =>
        --        match (xs, ys, zs) {
        --          | (1 :: _, 2 :: _, 3 :: _) => e1
        --          | _                        => e2
        --        }
        --    | _ => e2
        --
        let clauses :: [Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)]
            clauses =
              [ Clause
                  ()
                  [ pList
                      ()
                      [ pAs () "xs" (pCon () "(::)" [pLit () (IInt 1), pAny ()])
                      , pAs () "ys" (pCon () "(::)" [pLit () (IInt 2), pAny ()])
                      , pAs () "zs" (pCon () "(::)" [pLit () (IInt 3), pAny ()])
                      ]
                  ]
                  [Choice [] (eVar () "e1")]
              , Clause () [pAny ()] [Choice [] (eVar () "e2")]
              ]
            result :: [Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)]
            result =
              [ Clause
                  ()
                  [pList () [pVar () "xs", pVar () "ys", pVar () "zs"]]
                  [ Choice
                      []
                      ( ePat
                          ()
                          (eTup () [eVar () "xs", eVar () "ys", eVar () "zs"])
                          [ Clause
                              ()
                              [ pTup
                                  ()
                                  [ pCon () "(::)" [pLit () (IInt 1), pAny ()]
                                  , pCon () "(::)" [pLit () (IInt 2), pAny ()]
                                  , pCon () "(::)" [pLit () (IInt 3), pAny ()]
                                  ]
                              ]
                              [ Choice [] (eVar () "e1")
                              ]
                          , Clause () [pAny ()] [Choice [] (eVar () "e2")]
                          ]
                      )
                  ]
              , Clause () [pAny ()] [Choice [] (eVar () "e2")]
              ]
         in (dropAsPatterns clauses == result)

      it "| [(1 :: _) as xs, (2 :: _) as ys, (3 :: _) as zs] => e1 | _ => e2" $
        --    | [(1 :: _) as xs, (2 :: _) as ys, (3 :: _) as zs] => e1
        --    | _                                                => e2
        --
        --    | [xs, ys, zs] =>
        --        match (xs, ys, zs) {
        --          | (1 :: _, 2 :: _, 3 :: _) => e1
        --          | _                        => e2
        --        }
        --    | _ => e2
        --
        let clauses :: [Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)]
            clauses =
              [ Clause
                  tBool
                  [ pList
                      (tList (tList tInt))
                      [ pAs (tList tInt) "xs" (pCon (tList tInt) "(::)" [pLit tInt (IInt 1), pAny (tList tInt)])
                      , pAs (tList tInt) "ys" (pCon (tList tInt) "(::)" [pLit tInt (IInt 2), pAny (tList tInt)])
                      , pAs (tList tInt) "zs" (pCon (tList tInt) "(::)" [pLit tInt (IInt 3), pAny (tList tInt)])
                      ]
                  ]
                  [Choice [] (eVar tBool "e1")]
              , Clause tBool [pAny (tList (tList tInt))] [Choice [] (eVar tBool "e2")]
              ]
            result :: [Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)]
            result =
              [ Clause
                  tBool
                  [pList (tList (tList tInt)) [pVar (tList tInt) "xs", pVar (tList tInt) "ys", pVar (tList tInt) "zs"]]
                  [ Choice
                      []
                      ( ePat
                          tBool
                          (eTup (tup () [tList tInt, tList tInt, tList tInt]) [eVar (tList tInt) "xs", eVar (tList tInt) "ys", eVar (tList tInt) "zs"])
                          [ Clause
                              tBool
                              [ pTup
                                  (tup () [tList tInt, tList tInt, tList tInt])
                                  [ pCon (tList tInt) "(::)" [pLit tInt (IInt 1), pAny (tList tInt)]
                                  , pCon (tList tInt) "(::)" [pLit tInt (IInt 2), pAny (tList tInt)]
                                  , pCon (tList tInt) "(::)" [pLit tInt (IInt 3), pAny (tList tInt)]
                                  ]
                              ]
                              [ Choice [] (eVar tBool "e1")
                              ]
                          , Clause tBool [pAny (tList (tList tInt))] [Choice [] (eVar tBool "e2")]
                          ]
                      )
                  ]
              , Clause tBool [pAny (tList (tList tInt))] [Choice [] (eVar tBool "e2")]
              ]
         in (dropAsPatterns clauses == result)

      it "([x, _, _] as xs, []) when a => e1, when b => e2 | _ => e3" $
        --    | ([x, _, _] as xs, [])
        --        when a => e1
        --      , when b => e2
        --    | _ => e3
        --
        --    | (xs, []) =>
        --        match xs {
        --          | [x, _, _]
        --              when a => e1
        --            , when b => e2
        --          | _ => e3
        --        }
        --    | _ => e2
        --
        let clauses :: [Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)]
            clauses =
              [ Clause
                  ()
                  [pTup () [pAs () "xs" (pList () [pVar () "x", pAny (), pAny ()]), pList () []]]
                  [ Choice [eVar () "a"] (eVar () "e1")
                  , Choice [eVar () "b"] (eVar () "e2")
                  ]
              , Clause () [pAny ()] [Choice [] (eVar () "e3")]
              ]
            result :: [Clause () [Pattern ()] (Expr () [Pattern ()] (Clause () [Pattern ()]) Void1 Void)]
            result =
              [ Clause
                  ()
                  [pTup () [pVar () "xs", pList () []]]
                  [ Choice
                      []
                      ( ePat
                          ()
                          (eVar () "xs")
                          [ Clause
                              ()
                              [pList () [pVar () "x", pAny (), pAny ()]]
                              [ Choice [eVar () "a"] (eVar () "e1")
                              , Choice [eVar () "b"] (eVar () "e2")
                              ]
                          , Clause () [pAny ()] [Choice [] (eVar () "e3")]
                          ]
                      )
                  ]
              , Clause () [pAny ()] [Choice [] (eVar () "e3")]
              ]
         in (dropAsPatterns clauses == result)

      it "([x, _, _] as xs, []) when a => e1, when b => e2 | _ => e3" $
        --    | ([x, _, _] as xs, [])
        --        when a => e1
        --      , when b => e2
        --    | _ => e3
        --
        --    | (xs, []) =>
        --        match xs {
        --          | [x, _, _]
        --              when a => e1
        --            , when b => e2
        --          | _ => e3
        --        }
        --    | _ => e2
        --
        let clauses :: [Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)]
            clauses =
              [ Clause
                  tBool
                  [pTup (tup () [tList tInt, tList tInt]) [pAs (tList tInt) "xs" (pList (tList tInt) [pVar tInt "x", pAny tInt, pAny tInt]), pList (tList tInt) []]]
                  [ Choice [eVar tBool "a"] (eVar tBool "e1")
                  , Choice [eVar tBool "b"] (eVar tBool "e2")
                  ]
              , Clause tBool [pAny (tup () [tList tInt, tList tInt])] [Choice [] (eVar tBool "e3")]
              ]
            result :: [Clause (Type ()) [Pattern (Type ())] (Expr (Type ()) [Pattern (Type ())] (Clause (Type ()) [Pattern (Type ())]) Void1 Void)]
            result =
              [ Clause
                  tBool
                  [pTup (tup () [tList tInt, tList tInt]) [pVar (tList tInt) "xs", pList (tList tInt) []]]
                  [ Choice
                      []
                      ( ePat
                          tBool
                          (eVar (tList tInt) "xs")
                          [ Clause
                              tBool
                              [pList (tList tInt) [pVar tInt "x", pAny tInt, pAny tInt]]
                              [ Choice [eVar tBool "a"] (eVar tBool "e1")
                              , Choice [eVar tBool "b"] (eVar tBool "e2")
                              ]
                          , Clause tBool [pAny (tup () [tList tInt, tList tInt])] [Choice [] (eVar tBool "e3")]
                          ]
                      )
                  ]
              , Clause tBool [pAny (tup () [tList tInt, tList tInt])] [Choice [] (eVar tBool "e3")]
              ]
         in (dropAsPatterns clauses == result)

    describe "extractAsPatterns" $ do
      -- TODO
      pure ()

runTestExhaustive ::
  (Row t, Tuple t ()) => String -> Bool -> PatternMatrix t -> SpecWith ()
runTestExhaustive msg b px =
  it (prefix <> " " <> msg) (b == runReader (exhaustive px) testConstructorEnv)
  where
    prefix = if b then "✔" else "✗"

{- ORMOLU_DISABLE -}

testConstructorEnv :: ConstructorEnv
testConstructorEnv =
  constructorEnv
    [ ("[]"      , (["[]", "(::)"], 0))
    , ("(::)"    , (["[]", "(::)"], 2))
    ]

{- ORMOLU_ENABLE -}
