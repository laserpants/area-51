{-# LANGUAGE OverloadedStrings #-}

module Pong.Test.Data where

import qualified Data.Map.Strict as Map
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
    (lam [((), "x")] (app () (var ((), "f")) [var ((), "x")]))

--input1Typed :: Ast
--input1Typed =
--  let_
--    (i32 .-> i32, "f")
--    (var (i32 .-> i32, "foo"))
--    (lam [(i32, "x")] (app i32 (var (i32 .-> i32, "f")) [var (i32, "x")]))

input2 :: Expr () a0 a1 a2 a3
input2 = op2 OAddInt32 (var ((), "x")) (var ((), "y"))

--input2Typed :: Ast
--input2Typed = op2 OAddInt32 (var (i32, "x")) (var (i32, "y"))

input3 :: Expr () () a1 a2 a3
input3 = case_ (var ((), "xs")) [([((), "Cons"), ((), "x"), ((), "ys")], var ((), "x"))]

input4 :: Expr () a0 a1 a2 a3
input4 = case_ (var ((), "xs")) [([((), "Cons"), ((), "x"), ((), "ys")], var ((), "y"))]

input5 :: Expr () a0 a1 a2 a3
input5 =
  case_ (var ((), "xs")) [([((), "Nil")], var ((), "y")), ([((), "Cons"), ((), "x"), ((), "ys")], var ((), "x"))]

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
    (var (i32 .-> i32 .-> i32, "sum"))

input6Converted :: Expr Type a0 () () a3
input6Converted =
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
    (app i32 (var (i32 .-> i32, "foo")) [var (i32, "x")])
    (op2 OAddInt32 (var (i32, "x")) (lit (LInt32 3)))

input7Converted :: Expr Type a0 () () a3
input7Converted =
  app
    i32
    (lam [(i32, "x")] (op2 OAddInt32 (var (i32, "x")) (lit (LInt32 3))))
    [app i32 (var (i32 .-> i32, "foo")) [var (i32, "x")]]

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
    (op2 OAddInt32 (op2 OAddInt32 (var (i32, "m")) (var (i32, "n"))) (lit (LInt32 3)))

input9 :: Expr Type a0 () () a3
input9 = lam [(i32, "p")] (lam [(i32, "x")] (var (i32, "p")))

input9Converted :: Expr Type a0 () () a3
input9Converted =
  lam
    [(i32, "p")]
    (app
       (i32 .-> i32)
       (lam [(i32, "p"), (i32, "x")] (var (i32, "p")))
       [var (i32, "p")])

--input10 :: Program
--input10 =
--  Program
--    { count = 0
--    , definitions =
--        Map.fromList
--          [ ("foo", Function (Signature [(i32, "x")] (i32, bLit (LInt32 123))))
--          , ("baz", Function (Signature [(i32, "x")] (i32, bLit (LInt32 123))))
--          ]
--    }
--
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
--                (Signature [(i32, "x")] (i32 .-> i32, bCall "plus" [bVar "x"])))
--          ]
--    }
--
--input12 :: Expr ()
--input12 = lam [((), "x")] (app () (var ((), "plus")) [var ((), "x")])
--
--input13 :: TypeEnv
--input13 = Env.fromList [("plus", i32 .-> i32 .-> i32)]
--
--input14 :: Body
--input14 = bCase (bVar "xs") [(["Cons", "x", "ys"], bVar "x")]

input15 :: Expr Type a0 () () a3
input15 =
  lam
    [(i32, "x")]
    (lam [(i32, "y")] (op2 OAddInt32 (var (i32, "x")) (var (i32, "y"))))

input15Converted :: Expr Type a0 () () a3
input15Converted =
  lam
    [(i32, "x")]
    (app
       (i32 .-> i32)
       (lam [(i32, "x"), (i32, "y")] (op2 OAddInt32 (var (i32, "x")) (var (i32, "y"))))
       [var (i32, "x")])

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
