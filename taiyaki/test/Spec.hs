{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Taiyaki.Data
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

      let expr :: Expr (Type ())
          expr = con (tList tInt) "[]" []
       in it
            "[]"
            (eCon (tList tInt) "[]" == expr)

      let expr :: Expr (Type ())
          expr = con (tList tInt) "(::)" [eVar tInt "x", con (tList tInt) "[]" []]
       in it
            "x :: []"
            (eApp (tList tInt) (eCon (tInt ~> tList tInt ~> tList tInt) "(::)") [eVar tInt "x", eCon (tList tInt) "[]"] == expr)
    ---------------------------------------------------------------------------
    describe "(~>)" $ do
      let ty1 :: Type Name
          ty1 = tVar kTyp "a" ~> tVar kTyp "b" ~> tVar kTyp "c"
          ty2 :: Type Name
          ty2 = tArr (tVar kTyp "a") (tArr (tVar kTyp "b") (tVar kTyp "c"))
      it
        "a ~> b ~> c"
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
            (tApp kTyp (tApp kFun1 (tApp kFun2 (tCon (kFun 3) (tupleCon 3)) tInt) tInt) tBool == ty)

      let expr :: Expr ()
          expr = tup () [eLit () (IInt 1), eLit () (IInt 2)]
       in it
            "(1, 2)"
            (eTup () [eLit () (IInt 1), eLit () (IInt 2)] == expr)
    ---------------------------------------------------------------------------
    describe "Row" $ do
      let expr :: Expr ()
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
      let expr :: Expr ()
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
          [ [pTup () [pRec () (pExt () "x" (pOr () (pLit () (IChar 'a')) (pLit () (IChar 'b'))) (pVar () "r")), pRec () (pExt () "x" (pOr () (pLit () (IChar 'a')) (pLit () (IChar 'b'))) (pVar () "r"))]]
          ]

        runTestExhaustive
          "| ({ x = 'a' or 'b' | r }, { x = 'a' or 'b' | r }) \
          \| (_, _)"
          -- "
          True -- exhaustive
          [ [pTup () [pRec () (pExt () "x" (pOr () (pLit () (IChar 'a')) (pLit () (IChar 'b'))) (pVar () "r")), pRec () (pExt () "x" (pOr () (pLit () (IChar 'a')) (pLit () (IChar 'b'))) (pVar () "r"))]]
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