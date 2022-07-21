module Main where

import Control.Monad.Reader
import Teriyaki.Data
import Teriyaki.Lang
import Teriyaki.Tree

testConstructorEnv :: ConstructorEnv
testConstructorEnv =
  constructorEnv
    [ --      ("Some"     , ( ["Some", "None"], 1 ))
      --    , ("None"     , ( ["Some", "None"], 0 ))
      --    , ("Zero"     , ( ["Zero", "Succ"], 0 ))
      --    , ("Succ"     , ( ["Zero", "Succ"], 1 ))
      --    , ("Leaf"     , ( ["Leaf", "Node"], 0 ))
      --    , ("Node"     , ( ["Leaf", "Node"], 3 ))
      --    , ("Leaf'"    , ( ["Leaf'", "Node'"], 0 ))
      --    , ("Node'"    , ( ["Leaf'", "Node'"], 5 ))
      ("[]", (["[]", "(::)"], 0))
    , ("(::)", (["[]", "(::)"], 2))
    --    , ("(,)"      , ( ["(,)"], 2 ))
    --    , ("Foo"      , ( ["Foo"], 2 ))
    --    , ("#"        , ( ["#"], 1 ))
    --    , ("{}"       , ( ["{}"], 0 ))
    --    , ("Cons'"    , ( ["Nil'", "Cons'"], 3 ))
    --    , ("Nil'"     , ( ["Nil'", "Cons'"], 0 ))
    ]

runtest :: [[Pattern t]] -> Bool
runtest t = runReader (exhaustive t) testConstructorEnv

-- exhaustive
test1 :: [[Pattern ()]]
test1 =
  [ []
  ]

-- exhaustive
test2 :: [[Pattern ()]]
test2 =
  [ [pLit () (IBool True)]
  , [pLit () (IBool False)]
  ]

-- not exhaustive
test3 :: [[Pattern ()]]
test3 =
  [ [pLit () (IBool True)]
  ]

-- exhaustive
test4 :: [[Pattern ()]]
test4 =
  -- x :: (y :: ys)
  [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
  , -- []
    [pCon () "[]" []]
  , -- z :: zs
    [pCon () "(::)" [pVar () "z", pVar () "zs"]]
  ]

-- not exhaustive
test5 :: [[Pattern ()]]
test5 =
  -- x :: (y :: ys)
  [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
  , -- z :: zs
    [pCon () "(::)" [pVar () "z", pVar () "zs"]]
  ]

-- not exhaustive
test6 :: [[Pattern ()]]
test6 =
  -- x :: (y :: ys)
  [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
  , -- z :: zs
    [pCon () "(::)" [pVar () "z", pVar () "zs"]]
  , -- _ :: _
    [pCon () "(::)" [pAny (), pAny ()]]
  ]

-- exhaustive
test7 :: [[Pattern ()]]
test7 =
  -- x :: (y :: ys)
  [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
  , -- z :: zs
    [pCon () "(::)" [pVar () "z", pVar () "zs"]]
  , -- _ :: _
    [pCon () "(::)" [pAny (), pAny ()]]
  , -- []
    [pCon () "[]" []]
  ]

-- exhaustive
test8 :: [[Pattern ()]]
test8 =
  -- x :: y :: ys
  -- []
  -- z :: []
  [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
  , [pCon () "[]" []]
  , [pCon () "(::)" [pVar () "z", pCon () "[]" []]]
  ]

-- not exhaustive
test9 :: [[Pattern ()]]
test9 =
  -- x :: (y :: ys)
  -- []
  [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
  , [pCon () "[]" []]
  ]

-- not exhaustive
test10 :: [[Pattern ()]]
test10 =
  -- []
  [ [pCon () "[]" []]
  ]

-- exhaustive
test11 :: [[Pattern ()]]
test11 =
  -- []
  [ [pAny ()]
  ]

-- exhaustive
test12 :: [[Pattern ()]]
test12 =
  -- x :: ys
  -- []
  [ [pCon () "(::)" [pVar () "x", pVar () "ys"]]
  , [pCon () "[]" []]
  ]

-- exhaustive
test13 :: [[Pattern ()]]
test13 =
  -- x :: ys
  -- x
  [ [pCon () "(::)" [pVar () "x", pVar () "ys"]]
  , [pVar () "x"]
  ]

-- exhaustive
test14 :: [[Pattern ()]]
test14 =
  -- 5
  -- x
  [ [pLit () (IInt 5)]
  , [pVar () "x"]
  ]

-- not exhaustive
test15 :: [[Pattern ()]]
test15 =
  -- 5
  -- x
  [ [pLit () (IInt 5)]
  , [pLit () (IInt 4)]
  ]

-- exhaustive
test16 :: [[Pattern ()]]
test16 =
  -- 5, 5
  -- x, y
  [ [pLit () (IInt 5), pLit () (IInt 5)]
  , [pVar () "x", pVar () "y"]
  ]

-- not exhaustive
test17 :: [[Pattern ()]]
test17 =
  -- 5, 5
  -- x, 0
  [ [pLit () (IInt 5), pLit () (IInt 5)]
  , [pVar () "x", pLit () (IInt 0)]
  ]

main :: IO ()
main = print ("X" :: String) -- print kTyp
