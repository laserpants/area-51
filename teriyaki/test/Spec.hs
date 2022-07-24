{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Teriyaki.Data
import Teriyaki.Lang
import Teriyaki.Tree
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    testExhaustive

testExhaustive :: SpecWith ()
testExhaustive =
  describe "Exhaustive pattern match checking" $ do
    runTestExhaustive
      "No patterns"
      True -- exhaustive
      [ [] :: [Pattern ()]
      ]

    describe "Literal patterns" $ do
      runTestExhaustive
        "| True | False"
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
        "| True | _"
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
        "| 5 | 4"
        False -- not exhaustive
        [ [pLit () (IInt 5)]
        , [pLit () (IInt 4)]
        ]

      runTestExhaustive
        "| 5 | x"
        True -- exhaustive
        [ [pLit () (IInt 5)]
        , [pVar () "x"]
        ]

      runTestExhaustive
        "| 5, 5 | x, y"
        True -- exhaustive
        [ [pLit () (IInt 5), pLit () (IInt 5)]
        , [pVar () "x", pVar () "y"]
        ]

      runTestExhaustive
        "| 5, 5 | x, 0"
        False -- not exhaustive
        [ [pLit () (IInt 5), pLit () (IInt 5)]
        , [pVar () "x", pLit () (IInt 0)]
        ]

      runTestExhaustive
        "| \"x\" | \"y\""
        False -- not exhaustive
        [ [pLit () (IString "x")]
        , [pLit () (IString "y")]
        ]

      runTestExhaustive
        "| \"x\" | \"y\" | _"
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
        "| [_, _, _] | []"
        False -- not exhaustive
        [ [pList () [pAny (), pAny (), pAny ()]]
        , [pList () []]
        ]

      runTestExhaustive
        "| [_, _, _] | _"
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
        "| [x, 2] | _"
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
        "| x :: ys | []"
        True -- exhaustive
        [ [pCon () "(::)" [pVar () "x", pVar () "ys"]]
        , [pList () []]
        ]

      runTestExhaustive
        "| [] | [x] | [x, y] | x :: y :: ys"
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

    describe "Constructed value patterns" $ do
      runTestExhaustive
        "| _ :: _ :: _ :: []"
        False -- not exhaustive
        [ [pCon () "(::)" [pAny (), pCon () "(::)" [pAny (), pCon () "(::)" [pAny (), pCon () "[]" []]]]]
        ]

      runTestExhaustive
        "| [] | x :: y :: ys | z :: zs"
        True -- exhaustive
        [ [pCon () "[]" []]
        , [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
        , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
        ]

      runTestExhaustive
        "| x :: y :: ys | [] | z :: zs"
        True -- exhaustive
        [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
        , [pCon () "[]" []]
        , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
        ]

      runTestExhaustive
        "| x :: y :: ys | z :: zs"
        False -- not exhaustive
        [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
        , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
        ]

      runTestExhaustive
        "| x :: y :: ys | z :: zs | _ :: _"
        False -- not exhaustive
        [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
        , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
        , [pCon () "(::)" [pAny (), pAny ()]]
        ]

      runTestExhaustive
        "| x :: y :: ys | z :: zs | _ :: _ | []"
        True -- exhaustive
        [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
        , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
        , [pCon () "(::)" [pAny (), pAny ()]]
        , [pCon () "[]" []]
        ]

      runTestExhaustive
        "| x :: y :: ys | [] | z :: []"
        True -- exhaustive
        [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
        , [pCon () "[]" []]
        , [pCon () "(::)" [pVar () "z", pCon () "[]" []]]
        ]

      runTestExhaustive
        "| x :: y :: ys | []"
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
        "| [] | _"
        True -- exhaustive
        [ [pCon () "[]" []]
        , [pAny ()]
        ]

      runTestExhaustive
        "| x :: ys | []"
        True -- exhaustive
        [ [pCon () "(::)" [pVar () "x", pVar () "ys"]]
        , [pCon () "[]" []]
        ]

      runTestExhaustive
        "| x :: ys | x"
        True -- exhaustive
        [ [pCon () "(::)" [pVar () "x", pVar () "ys"]]
        , [pVar () "x"]
        ]

      runTestExhaustive
        "| x :: ys, 2 | [], _"
        False -- not exhaustive
        [ [pCon () "(::)" [pVar () "x", pVar () "ys", pLit () (IInt 2)]]
        , [pCon () "[]" [], pAny ()]
        ]

      runTestExhaustive
        "| x :: xs, true | x :: xs, false | [], _"
        True -- exhaustive
        [ [pCon () "(::)" [pVar () "x", pVar () "xs"], pLit () (IBool True)]
        , [pCon () "(::)" [pVar () "x", pVar () "xs"], pLit () (IBool False)]
        , [pCon () "[]" [], pAny ()]
        ]

      runTestExhaustive
        "| x :: xs, true | 3 :: xs, false | [], _"
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
        "| (1, 2) | (_, _)"
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
        "| (5, 5) | (x, y)"
        True -- exhaustive
        [ [pTup () [pLit () (IInt 5), pLit () (IInt 5)]]
        , [pTup () [pVar () "x", pVar () "y"]]
        ]

      runTestExhaustive
        "| (5, 5) | (x, 0)"
        False -- not exhaustive
        [ [pTup () [pLit () (IInt 5), pLit () (IInt 5)]]
        , [pTup () [pVar () "x", pLit () (IInt 0)]]
        ]

      runTestExhaustive
        "| (x :: ys, 2) | ([], _)"
        False -- not exhaustive
        [ [pTup () [pCon () "(::)" [pVar () "x", pVar () "ys", pLit () (IInt 2)]]]
        , [pTup () [pCon () "[]" [], pAny ()]]
        ]

      runTestExhaustive
        "| (x :: xs, true) | (x :: xs, false) | ([], _)"
        True -- exhaustive
        [ [pTup () [pCon () "(::)" [pVar () "x", pVar () "xs"], pLit () (IBool True)]]
        , [pTup () [pCon () "(::)" [pVar () "x", pVar () "xs"], pLit () (IBool False)]]
        , [pTup () [pCon () "[]" [], pAny ()]]
        ]

      runTestExhaustive
        "| (x :: xs, true) | (3 :: xs, false) | ([], _)"
        False -- not exhaustive
        [ [pTup () [pCon () "(::)" [pVar () "x", pVar () "xs"], pLit () (IBool True)]]
        , [pTup () [pCon () "(::)" [pLit () (IInt 3), pVar () "xs"], pLit () (IBool False)]]
        , [pTup () [pCon () "[]" [], pAny ()]]
        ]

      runTestExhaustive
        "| (x :: xs, true) | (3 :: xs, false) | ([], _) | _"
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

runTestExhaustive :: (Row t) => String -> Bool -> PatternMatrix t -> SpecWith ()
runTestExhaustive msg b px =
  it (prefix <> " " <> msg) $ b == runReader (exhaustive px) testConstructorEnv
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
