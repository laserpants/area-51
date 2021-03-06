{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.TheFatalAuction where

-- import Data.List.NonEmpty (fromList)
-- import qualified Data.Map.Strict as Map
-- import Pong.Data
-- import Pong.Lang
import Pong.Util

-- program44 :: Text
-- program44 =
--  "def foo(x : a, y : a) : a=\
--  \  x\
--  \\r\n\
--  \def main(a : unit) : int =\
--  \  foo(4, 5)\
--  \"
--
---- "
--
-- program46 :: Text
-- program46 =
--  "def foo(f : a -> a, y : b) : b=\
--  \  y\
--  \\r\n\
--  \def main(a : unit) : int =\
--  \  foo(lam(x) => x, 5)\
--  \"
--
---- "
--
-- program48 :: Text
-- program48 =
--  "def foo(f : a -> b, y : a) : b=\
--  \  f(y)\
--  \\r\n\
--  \def main(a : unit) : int =\
--  \  foo(lam(x) => x, 5)\
--  \"
--
---- "

program50 :: Text
program50 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \func foo(x : float, y : int, z : bool) : int=\
  \  y\
  \\r\n\
  \func prog(a : unit) : int =\
  \  let f = foo(4.0f, 5) in f(true)\
  \\r\n\
  \func main(a : unit) : int =\
  \  print_int(prog(()))\
  \"

-- "

program51 :: Text
program51 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \func foo(x : float, y : int, z : bool) : int=\
  \  y\
  \\r\n\
  \func prog(a : unit) : int =\
  \  let f = foo(4.0f) in f(7, true)\
  \\r\n\
  \func main(a : unit) : int =\
  \  print_int(prog(()))\
  \"

-- "

program52 :: Text
program52 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \func foo(x : float, y : int, z : bool) : int=\
  \  y\
  \\r\n\
  \func prog(a : unit) : int =\
  \  let f = foo(4.0f) in let g = f(9) in g(true)\
  \\r\n\
  \func main(a : unit) : int =\
  \  print_int(prog(()))\
  \"

-- "

program522 :: Text
program522 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \func foo(x : float, y : int, z : bool) : int=\
  \  y\
  \\r\n\
  \func prog(a : unit) : int =\
  \  let f = foo(4.0f) in let g = f(9) in g(true)\
  \\r\n\
  \func main(a : unit) : int =\
  \  print_int(prog())\
  \"

-- "

--
---- def foo(x : a, y : a) : a=
----   x
----
---- def main(a : unit) : int =
----   foo(4, 5)
--
-- program45 :: Program MonoType Ast
-- program45 =
--  Program
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  (tInt ~> tInt ~> tInt, "$var_foo_1")
--                  (eVar (tInt ~> tInt ~> tInt, "$lam1"))
--                  ( eCall
--                      (tInt ~> tInt ~> tInt, "$var_foo_1")
--                      [eLit (PInt 4), eLit (PInt 5)]
--                  )
--              )
--          )
--        ,
--          ( (Scheme (tVar "a" ~> tVar "a" ~> tVar "a"), "foo")
--          , Function
--              (fromList [(tVar 0, "x"), (tVar 0, "y")])
--              ( tVar 0
--              , eVar (tVar 0, "x")
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt ~> tInt), "$lam1")
--          , Function
--              (fromList [(tInt, "x"), (tInt, "y")])
--              ( tInt
--              , eVar (tInt, "x")
--              )
--          )
--        ]
--    )
--
----  def foo(f : a -> a, y : b) : b=
----    y
----
----  def main(a : unit) : int =
----    foo(lam(x) => x, 5)
--
-- program47 :: Program MonoType Ast
-- program47 =
--  Program
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  ((tVar 0 ~> tVar 0) ~> tInt ~> tInt, "$var_foo_1")
--                  (eVar ((tVar 0 ~> tVar 0) ~> tInt ~> tInt, "$lam1"))
--                  ( eCall
--                      ((tVar 0 ~> tVar 0) ~> tInt ~> tInt, "$var_foo_1")
--                      [eVar (tVar 0 ~> tVar 0, "$lam2"), eLit (PInt 5)]
--                  )
--              )
--          )
--        ,
--          ( (Scheme ((tVar "a" ~> tVar "a") ~> tVar "b" ~> tVar "b"), "foo")
--          , Function
--              (fromList [(tVar 1 ~> tVar 1, "f"), (tVar 2, "y")])
--              ( tVar 2
--              , eVar (tVar 2, "y")
--              )
--          )
--        ,
--          ( (Scheme ((tVar "a0" ~> tVar "a0") ~> tInt ~> tInt), "$lam1")
--          , Function
--              (fromList [(tVar 0 ~> tVar 0, "f"), (tInt, "y")])
--              ( tInt
--              , eVar (tInt, "y")
--              )
--          )
--        ,
--          ( (Scheme (tVar "a0" ~> tVar "a0"), "$lam2")
--          , Function
--              (fromList [(tVar 0, "x")])
--              ( tVar 0
--              , eVar (tVar 0, "x")
--              )
--          )
--        ]
--    )
--
---- def foo(f : a -> b, y : a) : b=
----   f(y)
--
---- def main(a : unit) : int =
----   foo(lam(x) => x, 5)
--
-- program49 :: Program MonoType Ast
-- program49 =
--  Program
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  ((tInt ~> tInt) ~> tInt ~> tInt, "$var_foo_1")
--                  (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "$lam1"))
--                  ( eCall
--                      ((tInt ~> tInt) ~> tInt ~> tInt, "$var_foo_1")
--                      [eVar (tInt ~> tInt, "$lam2"), eLit (PInt 5)]
--                  )
--              )
--          )
--        ,
--          ( (Scheme ((tVar "a" ~> tVar "b") ~> tVar "a" ~> tVar "b"), "foo")
--          , Function
--              (fromList [(tVar 0 ~> tVar 1, "f"), (tVar 0, "y")])
--              ( tVar 1
--              , eCall (tVar 0 ~> tVar 1, "f") [eVar (tVar 0, "y")]
--              )
--          )
--        ,
--          ( (Scheme ((tInt ~> tInt) ~> tInt ~> tInt), "$lam1")
--          , Function
--              (fromList [(tInt ~> tInt, "f"), (tInt, "y")])
--              ( tInt
--              , eCall (tInt ~> tInt, "f") [eVar (tInt, "y")]
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt), "$lam2")
--          , Function
--              (fromList [(tInt, "x")])
--              ( tInt
--              , eVar (tInt, "x")
--              )
--          )
--        ]
--    )
