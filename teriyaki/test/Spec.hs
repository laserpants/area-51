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
    runExchaustivePatternMatchTest
      "No patterns"
      True -- exhaustive
      [ []
      ]

    describe "Literal patterns" $ do
      runExchaustivePatternMatchTest
        "| True | False"
        True -- exhaustive
        [ [pLit () (IBool True)]
        , [pLit () (IBool False)]
        ]

      runExchaustivePatternMatchTest
        "| True"
        False -- not exhaustive
        [ [pLit () (IBool True)]
        ]

      runExchaustivePatternMatchTest
        "| True | _"
        True -- exhaustive
        [ [pLit () (IBool True)]
        , [pAny ()]
        ]

      runExchaustivePatternMatchTest
        "| ()"
        True -- exhaustive
        [ [pLit () IUnit]
        ]

      runExchaustivePatternMatchTest
        "| (), ()"
        True -- exhaustive
        [ [pLit () IUnit, pLit () IUnit]
        ]

      runExchaustivePatternMatchTest
        "| (), _"
        True -- exhaustive
        [ [pLit () IUnit, pAny ()]
        ]

      runExchaustivePatternMatchTest
        "| (), 3"
        False -- not exhaustive
        [ [pLit () IUnit, pLit () (IInt 3)]
        ]

      runExchaustivePatternMatchTest
        "| 5"
        False -- not exhaustive
        [ [pLit () (IInt 5)]
        ]

      runExchaustivePatternMatchTest
        "| 5 | 4"
        False -- not exhaustive
        [ [pLit () (IInt 5)]
        , [pLit () (IInt 4)]
        ]

      runExchaustivePatternMatchTest
        "| 5 | x"
        True -- exhaustive
        [ [pLit () (IInt 5)]
        , [pVar () "x"]
        ]

      runExchaustivePatternMatchTest
        "| 5, 5 | x, y"
        True -- exhaustive
        [ [pLit () (IInt 5), pLit () (IInt 5)]
        , [pVar () "x", pVar () "y"]
        ]

      runExchaustivePatternMatchTest
        "| 5, 5 | x, 0"
        False -- not exhaustive
        [ [pLit () (IInt 5), pLit () (IInt 5)]
        , [pVar () "x", pLit () (IInt 0)]
        ]

      runExchaustivePatternMatchTest
        "| \"x\" | \"y\""
        False -- not exhaustive
        [ [pLit () (IString "x")]
        , [pLit () (IString "y")]
        ]

      runExchaustivePatternMatchTest
        "| \"x\" | \"y\" | _"
        True -- exhaustive
        [ [pLit () (IString "x")]
        , [pLit () (IString "y")]
        , [pAny ()]
        ]

    describe "List literals" $ do
      runExchaustivePatternMatchTest
        "| [_, _, _]"
        False -- not exhaustive
        [ [pList () [pAny (), pAny (), pAny ()]]
        ]

      runExchaustivePatternMatchTest
        "| [_, _, _] | []"
        False -- not exhaustive
        [ [pList () [pAny (), pAny (), pAny ()]]
        , [pList () []]
        ]

      runExchaustivePatternMatchTest
        "| [_, _, _] | _"
        True -- exhaustive
        [ [pList () [pAny (), pAny (), pAny ()]]
        , [pAny ()]
        ]

    describe "Constructed value patterns" $ do
      runExchaustivePatternMatchTest
        "| _ :: _ :: _ :: []"
        False -- not exhaustive
        [ [pCon () "(::)" [pAny (), pCon () "(::)" [pAny (), pCon () "(::)" [pAny (), pCon () "[]" []]]]]
        ]

      runExchaustivePatternMatchTest
        "| [] | x :: y :: ys | z :: zs"
        True -- exhaustive
        [ [pCon () "[]" []]
        , [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
        , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
        ]

      runExchaustivePatternMatchTest
        "| x :: y :: ys | [] | z :: zs"
        True -- exhaustive
        [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
        , [pCon () "[]" []]
        , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
        ]

      runExchaustivePatternMatchTest
        "| x :: y :: ys | z :: zs"
        False -- not exhaustive
        [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
        , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
        ]

      runExchaustivePatternMatchTest
        "| x :: y :: ys | z :: zs | _ :: _"
        False -- not exhaustive
        [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
        , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
        , [pCon () "(::)" [pAny (), pAny ()]]
        ]

      runExchaustivePatternMatchTest
        "| x :: y :: ys | z :: zs | _ :: _ | []"
        True -- exhaustive
        [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
        , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
        , [pCon () "(::)" [pAny (), pAny ()]]
        , [pCon () "[]" []]
        ]

      runExchaustivePatternMatchTest
        "| x :: y :: ys | [] | z :: []"
        True -- exhaustive
        [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
        , [pCon () "[]" []]
        , [pCon () "(::)" [pVar () "z", pCon () "[]" []]]
        ]

      runExchaustivePatternMatchTest
        "| x :: y :: ys | []"
        False -- not exhaustive
        [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
        , [pCon () "[]" []]
        ]

      runExchaustivePatternMatchTest
        "| []"
        False -- not exhaustive
        [ [pCon () "[]" []]
        ]

      runExchaustivePatternMatchTest
        "| _"
        True -- exhaustive
        [ [pAny ()]
        ]

      runExchaustivePatternMatchTest
        "| [] | _"
        True -- exhaustive
        [ [pCon () "[]" []]
        , [pAny ()]
        ]

      runExchaustivePatternMatchTest
        "| x :: ys | []"
        True -- exhaustive
        [ [pCon () "(::)" [pVar () "x", pVar () "ys"]]
        , [pCon () "[]" []]
        ]

      runExchaustivePatternMatchTest
        "| x :: ys | x"
        True -- exhaustive
        [ [pCon () "(::)" [pVar () "x", pVar () "ys"]]
        , [pVar () "x"]
        ]

    describe "Tuple patterns" $ do
      runExchaustivePatternMatchTest
        "| (1, 2)"
        False -- not exhaustive
        [ [pTup () [pLit () (IInt 1), pLit () (IInt 2)]]
        ]

      runExchaustivePatternMatchTest
        "| (_, _)"
        True -- exhaustive
        [ [pTup () [pAny (), pAny ()]]
        ]

      runExchaustivePatternMatchTest
        "| (1, 2) | (_, _)"
        True -- exhaustive
        [ [pTup () [pLit () (IInt 1), pLit () (IInt 2)]]
        , [pTup () [pAny (), pAny ()]]
        ]

runExchaustivePatternMatchTest :: String -> Bool -> PatternMatrix t -> SpecWith ()
runExchaustivePatternMatchTest msg b px =
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
