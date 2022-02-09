{-# LANGUAGE OverloadedStrings #-}

module Pong.Test.Data where

import qualified Data.Map.Strict as Map
import Data.Void
import Pong.Data
import Pong.Lang
import qualified Pong.Util.Env as Env

i32 :: Type
i32 = tInt32

input1 :: Expr () () () () a3
input1 =
  let_
    ((), "f")
    (var ((), "foo"))
    (lam [((), "x")] (app (var ((), "f")) [var ((), "x")]))

input1Typed :: Expr Type () () () Void
input1Typed =
  let_
    (i32 ~> i32, "f")
    (var (i32 ~> i32, "foo"))
    (lam [(i32, "x")] (app (var (i32 ~> i32, "f")) [var (i32, "x")]))

input2 :: Expr () a0 a1 a2 a3
input2 = op2 OAddInt32 (var ((), "x")) (var ((), "y"))

input2Typed :: Expr Type () () () Void
input2Typed = op2 OAddInt32 (var (i32, "x")) (var (i32, "y"))

input3 :: Expr () () a1 a2 a3
input3 =
  case_
    (var ((), "xs"))
    [([((), "Cons"), ((), "x"), ((), "ys")], var ((), "x"))]

input4 :: Expr () a0 a1 a2 a3
input4 =
  case_
    (var ((), "xs"))
    [([((), "Cons"), ((), "x"), ((), "ys")], var ((), "y"))]

input5 :: Expr () a0 a1 a2 a3
input5 =
  case_
    (var ((), "xs"))
    [ ([((), "Nil")], var ((), "y"))
    , ([((), "Cons"), ((), "x"), ((), "ys")], var ((), "x"))
    ]

input6 :: Expr Type () () () a3
input6 =
  let_
    (i32, "sum")
    (lam
       [(i32, "m")]
       (let_
          (i32, "p")
          (lit (LInt32 3))
          (lam
             [(i32, "n")]
             (op2
                OAddInt32
                (op2 OAddInt32 (var (i32, "m")) (var (i32, "n")))
                (var (i32, "p"))))))
    (var (i32 ~> i32 ~> i32, "sum"))

input6NoLetBindings :: Expr Type a0 () () a3
input6NoLetBindings =
  lam
    [(i32, "m")]
    (lam
       [(i32, "n")]
       (op2
          OAddInt32
          (op2 OAddInt32 (var (i32, "m")) (var (i32, "n")))
          (lit (LInt32 3))))

input7 :: Expr Type () () () a3
input7 =
  let_
    (i32, "x")
    (app (var (i32 ~> i32, "foo")) [var (i32, "x")])
    (op2 OAddInt32 (var (i32, "x")) (lit (LInt32 3)))

input7NoLetBindings :: Expr Type a0 () () a3
input7NoLetBindings =
  app
    (lam [(i32, "x")] (op2 OAddInt32 (var (i32, "x")) (lit (LInt32 3))))
    [app (var (i32 ~> i32, "foo")) [var (i32, "x")]]

input8 :: Expr Type a0 () () a3
input8 =
  lam
    [(i32, "m")]
    (lam
       [(i32, "n")]
       (op2
          OAddInt32
          (op2 OAddInt32 (var (i32, "m")) (var (i32, "n")))
          (lit (LInt32 3))))

input8Converted :: Expr Type a0 () () a3
input8Converted =
  lam
    [(i32, "m"), (i32, "n")]
    (op2
       OAddInt32
       (op2 OAddInt32 (var (i32, "m")) (var (i32, "n")))
       (lit (LInt32 3)))

input9 :: Expr Type a0 () () a3
input9 = lam [(i32, "p")] (lam [(i32, "x")] (var (i32, "p")))

input9ClosuresConverted :: Expr Type a0 () () a3
input9ClosuresConverted =
  lam
    [(i32, "p")]
    (app (lam [(i32, "p"), (i32, "x")] (var (i32, "p"))) [var (i32, "p")])

input10 :: Program
input10 =
  Program
    { count = 0
    , definitions =
        Map.fromList
          [ ("foo", Function (Signature [(i32, "x")] (i32, lit (LInt32 123))))
          , ("baz", Function (Signature [(i32, "x")] (i32, lit (LInt32 123))))
          ]
    }

--input11 :: Program
--input11 =
--  Program
--    { count = 0
--    , definitions =
--        Map.fromList
--          [ ( "plus"
--            , Function
--                (Signature
--                   [(i32, "x"), (i32, "y")]
--                   (i32, bOp2 OAddInt32 (bVar "x") (bVar "y"))))
--          , ( "fun"
--            , Function
--                (Signature [(i32, "x")] (i32 ~> i32, bCall "plus" [bVar "x"])))
--          ]
--    }
input12 :: Expr () () () () a3
input12 = lam [((), "x")] (app (var ((), "plus")) [var ((), "x")])

input13 :: TypeEnv
input13 = Env.fromList [("plus", i32 ~> i32 ~> i32)]

--input14 :: Body
--input14 = bCase (bVar "xs") [(["Cons", "x", "ys"], bVar "x")]
input15 :: Expr Type a0 () () a3
input15 =
  lam
    [(i32, "x")]
    (lam [(i32, "y")] (op2 OAddInt32 (var (i32, "x")) (var (i32, "y"))))

input15ClosuresConverted :: Expr Type a0 () () a3
input15ClosuresConverted =
  lam
    [(i32, "x")]
    (app
       (lam
          [(i32, "x"), (i32, "y")]
          (op2 OAddInt32 (var (i32, "x")) (var (i32, "y"))))
       [var (i32, "x")])

-- (\xs : List -> foo(xs))(Cons(5, Nil))
input16 :: Expr Type () a1 () a3
input16 =
  let_
    (tData "List", "xs")
    (app
       (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
       [lit (LInt32 5), var (tData "List", "Nil")])
    (app (var (tData "List" ~> tInt32, "foo")) [var (tData "List", "xs")])

-- (\xs : List -> foo(xs))(Cons(5, Nil))
input16NoLetBindings :: Expr Type a0 () () a3
input16NoLetBindings =
  app
    (lam
       [(tData "List", "xs")]
       (app (var (tData "List" ~> tInt32, "foo")) [var (tData "List", "xs")]))
    [ app
        (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
        [lit (LInt32 5), var (tData "List", "Nil")]
    ]

--input16 :: [(Name, Definition (Expr ()))]
--input16 =
--  [ ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
--  , ("foo", Function (Signature [] (tInt32, fooAst)))
--  , ("main", Function (Signature [] (tInt32, mainAst)))
--  ]
--  where
--    fooAst =
--      let_
--        ((), "xs")
--        (app () (var ((), "Cons")) [lit (LInt32 5), app () (var ((), "Nil")) []])
--        (let_
--           ((), "ys")
--           (app () (var ((), "Cons")) [lit (LInt32 5), var ((), "xs")])
--           (case_
--              (var ((), "ys"))
--              [ ([((), "Nil")], lit (LInt32 1))
--              , ( [((), "Cons"), ((), "_"), ((), "zs")]
--                , case_
--                    (var ((), "zs"))
--                    [ ([((), "Nil")], lit (LInt32 2))
--                    , ([((), "Cons"), ((), "_"), ((), "_")], lit (LInt32 3))
--                    ])
--              ]))
--    mainAst = app () (var ((), "foo")) []
--
--input16Compiled :: [(Name, Definition Body)]
--input16Compiled =
--  [ ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
--  , ( "def_0"
--    , Function
--        (Signature
--           [ (tInt32 `tArr` tData "List" `tArr` tData "List", "Cons")
--           , (tData "List", "xs")
--           ]
--           (tInt32, bCall "def_1" [bCall "Cons" [bLit (LInt32 5), bVar "xs"]])))
--  , ( "def_1"
--    , Function
--        (Signature
--           [(tData "List", "ys")]
--           ( tInt32
--           , bCase
--               (bVar "ys")
--               [ (["Nil"], bLit (LInt32 1))
--               , ( ["Cons", "_", "zs"]
--                 , bCase
--                     (bVar "zs")
--                     [ (["Nil"], bLit (LInt32 2))
--                     , (["Cons", "_", "_"], bLit (LInt32 3))
--                     ])
--               ])))
--  , ( "foo"
--    , Function
--        (Signature
--           []
--           ( tInt32
--           , bCall
--               "def_0"
--               [bVar "Cons", bCall "Cons" [bLit (LInt32 5), bCall "Nil" []]])))
--  , ("main", Function (Signature [] (tInt32, bCall "foo" [])))
--  ]
program1 :: [(Name, Definition (SourceExpr ()))]
program1 =
  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
  , ("print_int32", External (Signature [tInt32] tInt32))
  , ( "List"
    , Data
        "List"
        [Constructor "Nil" [], Constructor "Cons" [tVar 0, tData "List"]])
  , ( "foo"
    , Function
        (Signature
           [(tUnit, "_")]
           ( tInt32
           , let_
               ((), "foo")
               (app
                  (var ((), "Cons"))
                  [lit (LInt32 5), app (var ((), "Nil")) []])
               (case_
                  (var ((), "foo"))
                  [ ([((), "Cons"), ((), "x"), ((), "xs")], var ((), "x"))
                  , ([((), "Nil")], lit (LInt32 9))
                  ]))))
  , ( "main"
    , Function
        (Signature
           []
           ( tInt32
           , app (var ((), "print_int32")) [app (var ((), "foo")) [lit LUnit]])))
  ]
