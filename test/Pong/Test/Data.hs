{-# LANGUAGE OverloadedStrings #-}

module Pong.Test.Data where

import qualified Data.Map.Strict as Map
import Pong.Data
import Pong.Lang
import qualified Pong.Util.Env as Env

i32 :: Type
i32 = tInt32

input1 :: Ast ()
input1 =
  let_
    ((), "f")
    (var () "foo")
    (lam [((), "x")] (app () (var () "f") [var () "x"]))

input1Typed :: Expr
input1Typed =
  let_
    (i32 .-> i32, "f")
    (var (i32 .-> i32) "foo")
    (lam
       [(i32, "x")]
       (app i32 (var (i32 .-> i32) "f") [var i32 "x"]))

input2 :: Ast ()
input2 = op2 OAddInt32 (var () "x") (var () "y")

input2Typed :: Expr
input2Typed = op2 OAddInt32 (var i32 "x") (var i32 "y")

input3 :: Ast ()
input3 = case_ (var () "xs") [(["Cons", "x", "ys"], var () "x")]

input4 :: Ast ()
input4 = case_ (var () "xs") [(["Cons", "x", "ys"], var () "y")]

input5 :: Ast ()
input5 =
  case_
    (var () "xs")
    [(["Nil"], var () "y"), (["Cons", "x", "ys"], var () "x")]

{-

let sum = 
  lam(m) => 
    let p = Int32(3) 
      in lam(n) => m + n + p 
  in sum

lam(m) => lam(n) => m + n + Int32(3) 

-}
input6 :: Expr
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
                (op2 OAddInt32 (var i32 "m") (var i32 "n"))
                (var i32 "p")))))
    (var (i32 .-> i32 .-> i32) "sum")

input6Converted :: Expr
input6Converted =
  lam
    [(i32, "m")]
    (lam
       [(i32, "n")]
       (op2
          OAddInt32
          (op2 OAddInt32 (var i32 "m") (var i32 "n"))
          (lit (LInt32 3))))

{-

let x = 
  foo(x)
  in
    x + 3

((x) => x + 3)(foo(x))

-}
input7 :: Expr
input7 =
  let_
    (i32, "x")
    (app i32 (var (i32 .-> i32) "foo") [var i32 "x"])
    (op2 OAddInt32 (var i32 "x") (lit (LInt32 3)))

input7Converted :: Expr
input7Converted =
  app
    i32
    (lam
       [(i32, "x")]
       (op2 OAddInt32 (var i32 "x") (lit (LInt32 3))))
    [app i32 (var (i32 .-> i32) "foo") [var i32 "x"]]

input8 :: Expr
input8 =
  lam
    [(i32, "m")]
    (lam
       [(i32, "n")]
       (op2
          OAddInt32
          (op2 OAddInt32 (var i32 "m") (var i32 "n"))
          (lit (LInt32 3))))

input8Converted :: Expr
input8Converted =
  lam
    [(i32, "m"), (i32, "n")]
    (op2
       OAddInt32
       (op2 OAddInt32 (var i32 "m") (var i32 "n"))
       (lit (LInt32 3)))

input9 :: Expr
input9 = lam [(i32, "p")] (lam [(i32, "x")] (var i32 "p"))

input9Converted :: Expr
input9Converted =
  lam
    [(i32, "p")]
    (app
       (i32 .-> i32)
       (lam [(i32, "p"), (i32, "x")] (var i32 "p"))
       [var i32 "p"])

input10 :: Program
input10 =
  Program
    { count = 0
    , definitions =
        Map.fromList
          [ ( "foo"
            , Function (Signature [(i32, "x")] (i32, bLit (LInt32 123))))
          , ( "baz"
            , Function (Signature [(i32, "x")] (i32, bLit (LInt32 123))))
          ]
    }

input11 :: Program
input11 =
  Program
    { count = 0
    , definitions =
        Map.fromList
          [ ( "plus"
            , Function
                (Signature
                   [(i32, "x"), (i32, "y")]
                   (i32, bOp2 OAddInt32 (bVar "x") (bVar "y"))))
          , ( "fun"
            , Function
                (Signature
                   [(i32, "x")]
                   (i32 .-> i32, bCall "plus" [bVar "x"])))
          ]
    }

input12 :: Ast ()
input12 = lam [((), "x")] (app () (var () "plus") [var () "x"])

input13 :: TypeEnv
input13 = Env.fromList [("plus", i32 .-> i32 .-> i32)]

input14 :: Body
input14 = bCase (bVar "xs") [(["Cons", "x", "ys"], bVar "x")]

input15 :: Expr
input15 =
  lam
    [(i32, "x")]
    (lam
       [(i32, "y")]
       (op2 OAddInt32 (var i32 "x") (var i32 "y")))

input15Converted :: Expr
input15Converted =
  lam
    [(i32, "x")]
    (app
       (i32 .-> i32)
       (lam
          [(i32, "x"), (i32, "y")]
          (op2 OAddInt32 (var i32 "x") (var i32 "y")))
       [var i32 "x"])
