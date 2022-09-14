module Main where

import Control.Monad.Reader
import Taiyaki.Data
import Taiyaki.Lang
import Taiyaki.Tree
import Prelude hiding (print, showString)

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

-- runtest :: [[Pattern t]] -> Bool
-- runtest t = runReader (exhaustive t) testConstructorEnv
--
---- exhaustive
-- test1 :: [[Pattern ()]]
-- test1 =
--  [ []
--  ]
--
---- exhaustive
-- test2 :: [[Pattern ()]]
-- test2 =
--  -- True
--  -- False
--  [ [pLit () (IBool True)]
--  , [pLit () (IBool False)]
--  ]
--
---- not exhaustive
-- test3 :: [[Pattern ()]]
-- test3 =
--  -- True
--  [ [pLit () (IBool True)]
--  ]
--
---- exhaustive
-- test4 :: [[Pattern ()]]
-- test4 =
--  -- x :: y :: ys
--  -- []
--  -- z :: zs
--  [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
--  , [pCon () "[]" []]
--  , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
--  ]
--
---- not exhaustive
-- test5 :: [[Pattern ()]]
-- test5 =
--  -- x :: y :: ys
--  -- z :: zs
--  [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
--  , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
--  ]
--
---- not exhaustive
-- test6 :: [[Pattern ()]]
-- test6 =
--  -- x :: y :: ys
--  -- z :: zs
--  -- _ :: _
--  --
--  [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
--  , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
--  , [pCon () "(::)" [pAny (), pAny ()]]
--  ]
--
---- exhaustive
-- test7 :: [[Pattern ()]]
-- test7 =
--  -- x :: y :: ys
--  -- z :: zs
--  -- _ :: _
--  -- []
--  --
--  [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
--  , [pCon () "(::)" [pVar () "z", pVar () "zs"]]
--  , [pCon () "(::)" [pAny (), pAny ()]]
--  , [pCon () "[]" []]
--  ]
--
---- exhaustive
-- test8 :: [[Pattern ()]]
-- test8 =
--  -- x :: y :: ys
--  -- []
--  -- z :: []
--  [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
--  , [pCon () "[]" []]
--  , [pCon () "(::)" [pVar () "z", pCon () "[]" []]]
--  ]
--
---- not exhaustive
-- test9 :: [[Pattern ()]]
-- test9 =
--  -- x :: (y :: ys)
--  -- []
--  [ [pCon () "(::)" [pVar () "x", pCon () "(::)" [pVar () "y", pVar () "ys"]]]
--  , [pCon () "[]" []]
--  ]
--
---- not exhaustive
-- test10 :: [[Pattern ()]]
-- test10 =
--  -- []
--  [ [pCon () "[]" []]
--  ]
--
---- exhaustive
-- test11 :: [[Pattern ()]]
-- test11 =
--  -- []
--  [ [pAny ()]
--  ]
--
---- exhaustive
-- test12 :: [[Pattern ()]]
-- test12 =
--  -- x :: ys
--  -- []
--  [ [pCon () "(::)" [pVar () "x", pVar () "ys"]]
--  , [pCon () "[]" []]
--  ]
--
---- exhaustive
-- test13 :: [[Pattern ()]]
-- test13 =
--  -- x :: ys
--  -- x
--  [ [pCon () "(::)" [pVar () "x", pVar () "ys"]]
--  , [pVar () "x"]
--  ]
--
---- exhaustive
-- test14 :: [[Pattern ()]]
-- test14 =
--  -- 5
--  -- x
--  [ [pLit () (IInt 5)]
--  , [pVar () "x"]
--  ]
--
---- not exhaustive
-- test15 :: [[Pattern ()]]
-- test15 =
--  -- 5
--  -- x
--  [ [pLit () (IInt 5)]
--  , [pLit () (IInt 4)]
--  ]
--
---- exhaustive
-- test16 :: [[Pattern ()]]
-- test16 =
--  -- 5, 5
--  -- x, y
--  [ [pLit () (IInt 5), pLit () (IInt 5)]
--  , [pVar () "x", pVar () "y"]
--  ]
--
---- not exhaustive
-- test17 :: [[Pattern ()]]
-- test17 =
--  -- 5, 5
--  -- x, 0
--  [ [pLit () (IInt 5), pLit () (IInt 5)]
--  , [pVar () "x", pLit () (IInt 0)]
--  ]
--
---- exhaustive
-- test18 :: [[Pattern ()]]
-- test18 =
--  [ [pLit () (IBool True)]
--  , [pLit () (IBool False)]
--  ]
--
---- exhaustive
-- test19 :: [[Pattern ()]]
-- test19 =
--  [ [pLit () (IBool True)]
--  , [pAny ()]
--  ]
--
---- not exhaustive
-- test20 :: [[Pattern ()]]
-- test20 =
--  [ [pLit () (IBool True)]
--  ]
--
---- exhaustive
-- test21 :: [[Pattern ()]]
-- test21 =
--  [ [pLit () IUnit]
--  ]
--
---- exhaustive
-- test22 :: [[Pattern ()]]
-- test22 =
--  [ [pLit () IUnit, pLit () IUnit]
--  ]
--
---- exhaustive
-- test23 :: [[Pattern ()]]
-- test23 =
--  [ [pLit () IUnit, pAny ()]
--  ]
--
---- not exhaustive
-- test24 :: [[Pattern ()]]
-- test24 =
--  [ [pLit () IUnit, pLit () (IInt 3)]
--  ]
--
---- not exhaustive
-- test25 :: [[Pattern ()]]
-- test25 =
--  [ [pLit () (IString "x")]
--  , [pLit () (IString "y")]
--  ]
--
---- exhaustive
-- test26 :: [[Pattern ()]]
-- test26 =
--  [ [pLit () (IString "x")]
--  , [pLit () (IString "y")]
--  , [pAny ()]
--  ]
--
----------------- Tuple patterns
--
---- not exhaustive
-- test27 :: [[Pattern ()]]
-- test27 =
--  -- (1, 2)
--  [ [pTup () [pLit () (IInt 1), pLit () (IInt 2)]]
--  ]
--
---- exhaustive
-- test28 :: [[Pattern ()]]
-- test28 =
--  -- (_, _)
--  [ [pTup () [pAny (), pAny ()]]
--  ]
--
---- exhaustive
-- test29 :: [[Pattern ()]]
-- test29 =
--  -- (1, 2)
--  -- (_, _)
--  [ [pTup () [pLit () (IInt 1), pLit () (IInt 2)]]
--  , [pTup () [pAny (), pAny ()]]
--  ]

---- { name = n, id = a }
-- test1x :: Pattern (Type Int)
-- test1x =
--  pExt
--    (tExt "name" tString (tExt "id" tInt tNil))
--    "name"
--    (pVar tString "n")
--    ( pExt
--        (tExt "id" tInt tNil)
--        "id"
--        (pVar tInt "a")
--        (pNil tNil)
--    )
--
-- test2x :: Pattern (Type Int)
-- test2x = pCon (tExt "id" tInt (tExt "name" tString tNil)) "{id}" [pVar tInt "a", pCon (tExt "name" tString tNil) "{name}" [pVar tString "n", pCon tNil "{}" []]]
--
-- test3x = rExt "baz" (eLit tInt (IInt 5)) rNil
--
-- test4x = rExt "foo" (eLit tInt (IInt 4)) test3x
--
-- test5x = pRec tInt test1x
--
-- test6x = con (tList tInt) "(::)" [eVar tInt "a", con (tList tInt) "[]" []] :: Expr (Type Int)

main :: IO ()
main = pure () -- print ("X" :: String) -- print kTyp

-- data Show_ a = Show_ { show :: a -> String }

-- show_ :: (a -> String) -> a -> String
-- show_ f = f
--
-- f1 :: (a -> String) -> (b -> String) -> (a, b) -> String
-- f1 f g (p, q) = "(" <> f p <> "," <> g q <> ")"
--
-- f2 :: Int -> String
-- f2 = show
--
-- f3 :: String -> String
-- f3 = id
--
-- abc :: (String, String)
-- abc =
--  let g h x = show_ (f1 h h) (x, x)
--   in
--     (g f2 5, g f3 "foo")
--

printPair0 :: (Print0 a, Print0 b) => (a, b) -> String
printPair0 (x, y) = "(" <> print0 x <> "," <> print0 y <> ")"

class Print0 a where
  print0 :: a -> String

instance Print0 Int where
  print0 = show

instance (Print0 a, Print0 b) => Print0 (a, b) where
  print0 = printPair0

--

newtype Print a = Print
  {print :: a -> String}

-- instance Print Int where ...
printInt :: Print Int
printInt = Print{print = show}

-- instance Print String where ...
printString :: Print String
printString = Print{print = id}

-- instance (Print a, Print b) => Print (a, b) where ...
printPair :: (Print a, Print b) -> Print (a, b)
printPair (d1, d2) =
  Print
    { print = \(a, b) -> "(" <> print d1 a <> "," <> print d2 b <> ")"
    }

-- abc :: (String, String)
-- abc =
--  let
--      g x = print (x, x)     -- print : Print (a, a) -> (a, a) -> string
--   in
--     (g 5, g "foo")          -- g : Print int -> int -> string , g : Print string -> string -> string

abc :: (String, String)
abc =
  let g :: Print a -> a -> String
      g h x = print (printPair (h, h)) (x, x)
   in (g printInt 5, g printString "foo")
