{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.JackOfClubs where

import Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
import Pong.Lang
import Pong.Util

programqq4 :: Text
programqq4 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  if true == false\
  \    then\
  \      1\
  \    else\
  \      100\
  \"

-- "

programrr4 :: Text
programrr4 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  if 3.4 == 3.4\
  \    then\
  \      1\
  \    else\
  \      100\
  \"

-- "

programoo4 :: Text
programoo4 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  prog()\
  \\r\n\
  \func prog(a : unit) : int =\
  \  let\
  \    f =\
  \      lam(n) =>\
  \        if n == 0\
  \          then\
  \            1\
  \          else\
  \            n * f(n - 1)\
  \    in\
  \      f(15)\
  \"

-- "

programo4 :: Text
programo4 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \func main(a : unit) : int =\
  \  print_int(prog())\
  \\r\n\
  \func prog(a : unit) : int =\
  \  let\
  \    f =\
  \      lam(n) =>\
  \        if n == 0\
  \          then\
  \            1\
  \          else\
  \            n * f(n - 1)\
  \    in\
  \      f(15)\
  \"

-- "

programi4 :: Text
programi4 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \    f =\
  \      lam(n) =>\
  \        2 * f(1)\
  \    in\
  \      f(5)\
  \"

-- "

programh4 :: Text
programh4 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \    f =\
  \      1 + f\
  \    in\
  \      5 -- f(5)\
  \"

-- "

programz4 :: Text
programz4 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \    h =\
  \      lam(z) =>\
  \        z\
  \    in\
  \      let\
  \        f =\
  \          lam(n) =>\
  \            h(n - 1)\
  \        in\
  \          f(5)\
  \"

-- "

--
-- func main(a : unit) : int =
--   let
--     f =
--       $lam3($lam1)
--     in
--       f(5)
--
--  func $lam1(z : int) : int =
--    z
--
--  func $lam3(h-1 : int -> int, n : int) : int =
--    h-1(n - 1)
--
--

programg4 :: Text
programg4 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \    f =\
  \      lam(n) =>\
  \        if n == 0\
  \          then\
  \            1\
  \          else\
  \            n * f(n - 1)\
  \    in\
  \      f(5)\
  \"

-- "

programf4 :: Text
programf4 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \    f =\
  \      lam(g) =>\
  \        lam(n) =>\
  \          if n == 0\
  \            then\
  \              1\
  \            else\
  \              n * g(f, n - 1)\
  \      in\
  \        f(f, 5)\
  \"

-- "

programb4 :: Text
programb4 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \    f =\
  \      lam(g) =>\
  \        lam(n) =>\
  \          if n == 0\
  \            then\
  \              1\
  \            else\
  \              n * g(g, n - 1)\
  \      in\
  \        f(f, 5)\
  \"

-- "

programc4 :: ModuleDefs () SourceExpr
programc4 =
  Map.fromList
    [
      (
        ( Scheme (tUnit ~> tInt)
        , "main"
        )
      , Function
          (fromList [((), "a")])
          ( ()
          , eLet
              ((), "f")
              ( eLam
                  ()
                  [((), "g")]
                  ( eLam
                      ()
                      [((), "n")]
                      ( eIf
                          ( eOp2
                              ((), OEq)
                              (eVar ((), "n"))
                              (eLit (PInt 0))
                          )
                          (eLit (PInt 1))
                          ( eOp2
                              ((), OMul)
                              (eVar ((), "n"))
                              ( eApp
                                  ()
                                  (eVar ((), "g"))
                                  [ eVar ((), "g")
                                  , eOp2
                                      ((), OSub)
                                      (eVar ((), "n"))
                                      (eLit (PInt 1))
                                  ]
                              )
                          )
                      )
                  )
              )
              ( eApp
                  ()
                  (eVar ((), "f"))
                  [ eVar ((), "f")
                  , eLit (PInt 5)
                  ]
              )
          )
      )
    ]

-- x123 =
--    let
--      f =
--        \g n ->
--          if n == 0 then 1 else n * (g g (n - 1)
--      in
--        f f 5

programc6 :: ModuleDefs MonoType TypedExpr
programc6 =
  Map.fromList
    [
      (
        ( Scheme (tUnit ~> tInt)
        , "main"
        )
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLet
              ((((tVar 3 ~> tInt ~> tInt) ~> tInt ~> tInt) ~> tInt ~> tInt) ~> tInt ~> tInt, "f")
              ( eLam
                  ()
                  [((tVar 3 ~> tInt ~> tInt) ~> tInt ~> tInt, "g")]
                  ( eLam
                      ()
                      [(tInt, "n")]
                      ( eIf
                          ( eOp2
                              oEqInt
                              (eVar (tInt, "n"))
                              (eLit (PInt 0))
                          )
                          (eLit (PInt 1))
                          ( eOp2
                              oMulInt
                              (eVar (tInt, "n"))
                              ( eApp
                                  tInt
                                  (eVar ((tVar 3 ~> tInt ~> tInt) ~> tInt ~> tInt, "g"))
                                  [ eVar ((tVar 3 ~> tInt ~> tInt) ~> tInt ~> tInt, "g")
                                  , eOp2
                                      oSubInt
                                      (eVar (tInt, "n"))
                                      (eLit (PInt 1))
                                  ]
                              )
                          )
                      )
                  )
              )
              ( eApp
                  tInt
                  (eVar (((tVar 22 ~> tInt ~> tInt) ~> tInt ~> tInt) ~> tInt ~> tInt, "f"))
                  [ eVar ((tVar 22 ~> tInt ~> tInt) ~> tInt ~> tInt, "f")
                  , eLit (PInt 5)
                  ]
              )
          )
      )
    ]

programe4 :: Text
programe4 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \    f =\
  \      lam(g) =>\
  \        lam(n) =>\
  \          1\
  \      in\
  \        f(f, 5)\
  \"

-- "

--  func main(a : unit) : int =
--    let
--      f =
--        lam(g) =>
--          lam(n) =>
--            1
--        in
--          f(f, 5)

programd4 :: ModuleDefs () SourceExpr
programd4 =
  Map.fromList
    [
      (
        ( Scheme (tUnit ~> tInt)
        , "main"
        )
      , Function
          (fromList [((), "a")])
          ( ()
          , eLet
              ((), "f")
              ( eLam
                  ()
                  [((), "g")]
                  ( eLam
                      ()
                      [((), "n")]
                      (eLit (PInt 1))
                  )
              )
              ( eApp
                  ()
                  (eVar ((), "f"))
                  [ eVar ((), "f")
                  , eLit (PInt 5)
                  ]
              )
          )
      )
    ]

programd5 :: ModuleDefs MonoType TypedExpr
programd5 =
  Map.fromList
    [
      (
        ( Scheme (tUnit ~> tInt)
        , "main"
        )
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLet
              (tVar 0 ~> tVar 1 ~> tInt, "f")
              ( eLam
                  ()
                  [(tVar 0, "g")]
                  ( eLam
                      ()
                      [(tVar 1, "n")]
                      (eLit (PInt 1))
                  )
              )
              ( eApp
                  tInt
                  (eVar ((tVar 2 ~> tVar 3 ~> tInt) ~> tInt ~> tInt, "f"))
                  [ eVar (tVar 2 ~> tVar 3 ~> tInt, "f")
                  , eLit (PInt 5)
                  ]
              )
          )
      )
    ]

programa4 :: Text
programa4 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \    foo =\
  \      lam(x) =>\
  \        lam(y) =>\
  \          lam(z) =>\
  \            x + y + z\
  \    in\
  \      let\
  \        f =\
  \          foo(1)\
  \        in\
  \          let\
  \            g =\
  \              f(2)\
  \            in\
  \              g(3)\
  \"

-- "

program94 :: Text
program94 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \     foo =\
  \       lam(f) =>\
  \         f(8)\
  \    in\
  \      let\
  \        id =\
  \          lam(x) =>\
  \            x\
  \        in\
  \          let\
  \            add =\
  \              lam(x) =>\
  \                lam(y) =>\
  \                  x + y\
  \            in\
  \              let\
  \                add2 =\
  \                  add(2)\
  \                in\
  \                  foo(add2) + add(4, 5)\
  \"

-- "

program84 :: Text
program84 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \    id =\
  \      lam(x) =>\
  \        x\
  \    in\
  \      let\
  \        add =\
  \          lam(x) =>\
  \            lam(y) =>\
  \              x + y\
  \        in\
  \          let\
  \            add2 =\
  \              add(2)\
  \            in\
  \              foo(add2) + add(4, 5)\
  \\r\n\
  \func foo(f : int -> int) : int = \
  \  f(8)\
  \"

-- "

program74 :: Text
program74 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \    add =\
  \      lam(x) =>\
  \        lam(y) =>\
  \          x + y\
  \    in\
  \      let\
  \        add2 =\
  \          add(2)\
  \        in\
  \          add2(3)\
  \"

-- "

program4 :: Text
program4 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \    id =\
  \      lam(x) =>\
  \        x\
  \    in\
  \      let\
  \        add =\
  \          lam(x) =>\
  \            lam(y) =>\
  \              x + y\
  \        in\
  \          let\
  \            add2 =\
  \              add(2)\
  \            in\
  \              (id(add2))(id(3)) + add(4, 5)\
  \"

-- "

program5 :: ModuleDefs () SourceExpr
program5 =
  Map.fromList
    [
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [((), "a")])
          ( ()
          , eLet
              ((), "id")
              (eLam () [((), "x")] (eVar ((), "x")))
              ( eLet
                  ((), "add")
                  ( eLam
                      ()
                      [((), "x")]
                      ( eLam
                          ()
                          [((), "y")]
                          ( eOp2
                              ((), OAdd)
                              (eVar ((), "x"))
                              (eVar ((), "y"))
                          )
                      )
                  )
                  ( eLet
                      ((), "add2")
                      (eApp () (eVar ((), "add")) [eLit (PInt 2)])
                      ( eOp2
                          ((), OAdd)
                          ( eApp
                              ()
                              (eApp () (eVar ((), "id")) [eVar ((), "add2")])
                              [ eApp () (eVar ((), "id")) [eLit (PInt 3)]
                              ]
                          )
                          (eApp () (eVar ((), "add")) [eLit (PInt 4), eLit (PInt 5)])
                      )
                  )
              )
          )
      )
    ]

program6 :: ModuleDefs MonoType TypedExpr
program6 =
  Map.fromList
    [
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLet
              (tVar 0 ~> tVar 0, "id")
              (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
              ( eLet
                  (tVar 1 ~> tVar 1 ~> tVar 1, "add")
                  ( eLam
                      ()
                      [(tVar 1, "x")]
                      ( eLam
                          ()
                          [(tVar 1, "y")]
                          ( eOp2
                              (tVar 1 ~> tVar 1 ~> tVar 1, OAdd)
                              (eVar (tVar 1, "x"))
                              (eVar (tVar 1, "y"))
                          )
                      )
                  )
                  ( eLet
                      (tInt ~> tInt, "add2")
                      (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 2)])
                      ( eOp2
                          oAddInt
                          ( eApp
                              tInt
                              (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id")) [eVar (tInt ~> tInt, "add2")])
                              [ eApp tInt (eVar (tInt ~> tInt, "id")) [eLit (PInt 3)]
                              ]
                          )
                          (eApp tInt (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 4), eLit (PInt 5)])
                      )
                  )
              )
          )
      )
    ]

-- program7 :: Module MonoType TypedExpr
-- program7 =
--  Module
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  (tVar 0 ~> tVar 0, "id")
--                  (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
--                  ( eLet
--                      (tVar 1 ~> tVar 1 ~> tVar 1, "add")
--                      ( eLam
--                          ()
--                          [(tVar 1, "x"), (tVar 1, "y")]
--                          ( eOp2
--                              (tVar 1 ~> tVar 1 ~> tVar 1, OAdd)
--                              (eVar (tVar 1, "x"))
--                              (eVar (tVar 1, "y"))
--                          )
--                      )
--                      ( eLet
--                          (tInt ~> tInt, "add2")
--                          (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 2)])
--                          ( eOp2
--                              oAddInt
--                              ( eApp
--                                  tInt
--                                  (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id")) [eVar (tInt ~> tInt, "add2")])
--                                  [ eApp tInt (eVar (tInt ~> tInt, "id")) [eLit (PInt 3)]
--                                  ]
--                              )
--                              (eApp tInt (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 4), eLit (PInt 5)])
--                          )
--                      )
--                  )
--              )
--          )
--        ]
--    )
--
-- program8 :: Module MonoType TypedExpr
-- program8 =
--  Module
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")
--                  (eLam () [(tInt ~> tInt, "x")] (eVar (tInt ~> tInt, "x")))
--                  ( eLet
--                      (tInt ~> tInt, "id-4")
--                      (eLam () [(tInt, "x")] (eVar (tInt, "x")))
--                      ( eLet
--                          (tVar 0 ~> tVar 0, "id")
--                          (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
--                          ( eLet
--                              (tInt ~> tInt ~> tInt, "add-1")
--                              ( eLam
--                                  ()
--                                  [(tInt, "x")]
--                                  ( eLam
--                                      ()
--                                      [(tInt, "y")]
--                                      ( eOp2
--                                          oAddInt
--                                          (eVar (tInt, "x"))
--                                          (eVar (tInt, "y"))
--                                      )
--                                  )
--                              )
--                              ( eLet
--                                  (tInt ~> tInt ~> tInt, "add-2")
--                                  ( eLam
--                                      ()
--                                      [(tInt, "x")]
--                                      ( eLam
--                                          ()
--                                          [(tInt, "y")]
--                                          ( eOp2
--                                              oAddInt
--                                              (eVar (tInt, "x"))
--                                              (eVar (tInt, "y"))
--                                          )
--                                      )
--                                  )
--                                  ( eLet
--                                      (tVar 1 ~> tVar 1 ~> tVar 1, "add")
--                                      ( eLam
--                                          ()
--                                          [(tVar 1, "x")]
--                                          ( eLam
--                                              ()
--                                              [(tVar 1, "y")]
--                                              ( eOp2
--                                                  (tVar 1 ~> tVar 1 ~> tVar 1, OAdd)
--                                                  (eVar (tVar 1, "x"))
--                                                  (eVar (tVar 1, "y"))
--                                              )
--                                          )
--                                      )
--                                      ( eLet
--                                          (tInt ~> tInt, "add2")
--                                          (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "add-1")) [eLit (PInt 2)])
--                                          ( eOp2
--                                              oAddInt
--                                              ( eApp
--                                                  tInt
--                                                  (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")) [eVar (tInt ~> tInt, "add2")])
--                                                  [ eApp tInt (eVar (tInt ~> tInt, "id-4")) [eLit (PInt 3)]
--                                                  ]
--                                              )
--                                              (eApp tInt (eVar (tInt ~> tInt ~> tInt, "add-2")) [eLit (PInt 4), eLit (PInt 5)])
--                                          )
--                                      )
--                                  )
--                              )
--                          )
--                      )
--                  )
--              )
--          )
--        ]
--    )
--
-- program9 :: Module MonoType TypedExpr
-- program9 =
--  Module
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")
--                  (eLam () [(tInt ~> tInt, "x")] (eVar (tInt ~> tInt, "x")))
--                  ( eLet
--                      (tInt ~> tInt, "id-4")
--                      (eLam () [(tInt, "x")] (eVar (tInt, "x")))
--                      ( eLet
--                          (tVar 0 ~> tVar 0, "id")
--                          (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
--                          ( eLet
--                              (tInt ~> tInt ~> tInt, "add-1")
--                              ( eLam
--                                  ()
--                                  [(tInt, "x"), (tInt, "y")]
--                                  ( eOp2
--                                      oAddInt
--                                      (eVar (tInt, "x"))
--                                      (eVar (tInt, "y"))
--                                  )
--                              )
--                              ( eLet
--                                  (tInt ~> tInt ~> tInt, "add-2")
--                                  ( eLam
--                                      ()
--                                      [(tInt, "x"), (tInt, "y")]
--                                      ( eOp2
--                                          oAddInt
--                                          (eVar (tInt, "x"))
--                                          (eVar (tInt, "y"))
--                                      )
--                                  )
--                                  ( eLet
--                                      (tVar 1 ~> tVar 1 ~> tVar 1, "add")
--                                      ( eLam
--                                          ()
--                                          [(tVar 1, "x"), (tVar 1, "y")]
--                                          ( eOp2
--                                              (tVar 1 ~> tVar 1 ~> tVar 1, OAdd)
--                                              (eVar (tVar 1, "x"))
--                                              (eVar (tVar 1, "y"))
--                                          )
--                                      )
--                                      ( eLet
--                                          (tInt ~> tInt, "add2")
--                                          (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "add-1")) [eLit (PInt 2)])
--                                          ( eOp2
--                                              oAddInt
--                                              ( eApp
--                                                  tInt
--                                                  (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")) [eVar (tInt ~> tInt, "add2")])
--                                                  [ eApp tInt (eVar (tInt ~> tInt, "id-4")) [eLit (PInt 3)]
--                                                  ]
--                                              )
--                                              (eApp tInt (eVar (tInt ~> tInt ~> tInt, "add-2")) [eLit (PInt 4), eLit (PInt 5)])
--                                          )
--                                      )
--                                  )
--                              )
--                          )
--                      )
--                  )
--              )
--          )
--        ]
--    )
--
----  def main(a : unit) : int =
----    let
----      id =
----        lam(x) =>
----          x
----      in
----        let
----          add =
----            lam(x) =>
----              lam(y) =>
----                x + y
----          in
----            let
----              add2 =
----                add(2)
----              in
----                (id(add2))(id(3)) + add(4, 5)
--
----
---- let
----   id-3 =
----     lam(x) => x
----   in
----     let
----       id-4 =
----         lam(x) => x
----       in
----         let
----           id =
----             lam(x) => x
----           in
----             let
----               add-1 =
----                 lam(x) =>
----                   lam(y) =>
----                     x + y
----               in
----                 let
----                   add-2 =
----                     lam(x) =>
----                       lam(y) =>
----                         x + y
----                   in
----                     let
----                       add =
----                         lam(x) =>
----                           lam(y) =>
----                             x + y
----                       in
----                         let
----                           add2 =
----                             add-1(2)
----                           in
----                             (id-3(add2))(id-4(3))
----                             +
----                             add-2(4, 5)
----
----
--
---------------------------------------------------------------------------------
--
---- def main(a : unit) : int =
----   let
----     add2 =
----       add-1(2)
----     in
----       id-3(add2, id-4(3))
----       +
----       add-2(4, 5)
----
---- def $lam2(x : int) : int =
----   x
----
---- def $lam4(x : int, y : int) : int =
----   x + y
----
---- def $lam5(x : int, $v0 : int) : int =
----   $lam4(x, $v0)
----
---- def $lam6(x : int, y : int) : int =
----   x + y
----
---- def $lam7(x : int, $v0 : int) : int =
----   $lam6(x, $v0)
----
---- def $lam1(x : int -> int, $v0 : int) : int =
----   x($v0)
----
---- def $lam8(x : '1, y : '1) : '1 =
----   x + y
----
---- def $lam9(x : '1, $v0 : '1) : '1 =
----   $lam8(x, $v0)
----
---- def $lam3(x : '0) : '0 =
----   x
----
--
---- ((Scheme (Fix (TArr (Fix (TVar "a0")) (Fix (TArr (Fix (TVar "a0")) (Fix (TVar "a0")))))),"$lam8"),
----   Function ((Fix (TVar 1),"x") :| [(Fix (TVar 1),"y")]) (Fix (TVar 1),Fix (EOp2 (Fix (TArr (Fix (TVar 1)) (Fix (TArr (Fix (TVar 1)) (Fix (TVar 1))))),OAdd) (Fix (EVar (Fix (TVar 1),"x"))) (Fix (EVar (Fix (TVar 1),"y")))))),((Scheme (Fix (TArr (Fix (TVar "a0")) (Fix (TArr (Fix (TVar "a0")) (Fix (TVar "a0")))))),"$lam9"),Function ((Fix (TVar 1),"x") :| [(Fix (TVar 1),"$v0")]) (Fix (TVar 1),Fix (ECall () (Fix (TArr (Fix (TVar 1)) (Fix (TArr (Fix (TVar 1)) (Fix (TVar 1))))),"$lam8") [Fix (EVar (Fix (TVar 1),"x")),Fix (EVar (Fix (TVar 1),"$v0"))]))),((Scheme (Fix (TArr (Fix (TVar "a0")) (Fix (TVar "a0")))),"$lam3"),Function ((Fix (TVar 0),"x") :| []) (Fix (TVar 0),Fix (EVar (Fix (TVar 0),"x"))))])
--
----  def main(a : unit) : int =
----    let
----      id =
----        lam(x) =>
----          x
----      in
----        let
----          add =
----            lam(x) =>
----              lam(y) =>
----                x + y
----          in
----            let
----              add2 =
----                add(2)
----              in
----                (id(add2))(id(3)) + add(4, 5)\
--
-- program10 :: Module MonoType Ast
-- program10 =
--  Module
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")
--                  (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "$lam1"))
--                  ( eLet
--                      (tInt ~> tInt, "id-4")
--                      (eVar (tInt ~> tInt, "$lam2"))
--                      ( eLet
--                          (tInt ~> tInt ~> tInt, "add-1")
--                          (eVar (tInt ~> tInt ~> tInt, "$lam4"))
--                          ( eLet
--                              (tInt ~> tInt ~> tInt, "add-2")
--                              (eVar (tInt ~> tInt ~> tInt, "$lam6"))
--                              ( eLet
--                                  (tInt ~> tInt, "add2")
--                                  (eCall (tInt ~> tInt ~> tInt, "add-1") [eLit (PInt 2)])
--                                  ( eOp2
--                                      oAddInt
--                                      ( eCall
--                                          ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")
--                                          [ eVar (tInt ~> tInt, "add2")
--                                          , eCall (tInt ~> tInt, "id-4") [eLit (PInt 3)]
--                                          ]
--                                      )
--                                      (eCall (tInt ~> tInt ~> tInt, "add-2") [eLit (PInt 4), eLit (PInt 5)])
--                                  )
--                              )
--                          )
--                      )
--                  )
--              )
--          )
--        ,
--          ( (Scheme ((tInt ~> tInt) ~> tInt ~> tInt), "$lam1")
--          , Function
--              (fromList [(tInt ~> tInt, "x"), (tInt, "$v0")])
--              ( tInt
--              , eCall (tInt ~> tInt, "x") [eVar (tInt, "$v0")]
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
--        ,
--          ( (Scheme (tInt ~> tInt ~> tInt), "$lam3")
--          , Function
--              (fromList [(tInt, "x"), (tInt, "y")])
--              ( tInt
--              , eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt ~> tInt), "$lam4")
--          , Function
--              (fromList [(tInt, "x"), (tInt, "$v0")])
--              ( tInt
--              , eCall (tInt ~> tInt ~> tInt, "$lam3") [eVar (tInt, "x"), eVar (tInt, "$v0")]
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt ~> tInt), "$lam5")
--          , Function
--              (fromList [(tInt, "x"), (tInt, "y")])
--              ( tInt
--              , eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt ~> tInt), "$lam6")
--          , Function
--              (fromList [(tInt, "x"), (tInt, "$v0")])
--              ( tInt
--              , eCall (tInt ~> tInt ~> tInt, "$lam5") [eVar (tInt, "x"), eVar (tInt, "$v0")]
--              )
--          )
--        ]
--    )
--
----
---- def main(a : unit) : int =
----   let
----     id-3 =                          : (int ~> int) ~> int ~> int
----       $lam1
----     in
----       let
----         id-4 =
----           $lam2
----         in
----           let
----             add-1 =
----               $lam3
----             in
----               let
----                 add-2 =
----                   $lam4
----                 in
----                   let
----                     add2 =               : int -> int
----                       add-1(2)
----                     in
----                       (id-3(add2))($var_id-4(3)) + $var_add-2(4, 5)
----
---- def $lam1(x : int -> int, $v0 : int) : int =
----   x($v0)
----
---- def $lam2(x : int) : int =
----   x
----
---- def $lam3(x : int, y : int) : int =
----   x + y
----
---- def $lam4(x : int, y : int) : int =
----   x + y
----
--
-- program11 :: Module MonoType Ast
-- program11 =
--  Module
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")
--                  (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "$lam1"))
--                  ( eLet
--                      (tInt ~> tInt, "id-4")
--                      (eVar (tInt ~> tInt, "$lam2"))
--                      ( eLet
--                          (tInt ~> tInt ~> tInt, "add-1")
--                          (eVar (tInt ~> tInt ~> tInt, "$lam3"))
--                          ( eLet
--                              (tInt ~> tInt ~> tInt, "add-2")
--                              (eVar (tInt ~> tInt ~> tInt, "$lam4"))
--                              ( eLet
--                                  (tInt ~> tInt, "add2")
--                                  (eCall (tInt ~> tInt ~> tInt, "add-1") [eLit (PInt 2)])
--                                  ( eOp2
--                                      oAddInt
--                                      ( eCall
--                                          ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")
--                                          [ eVar (tInt ~> tInt, "add2")
--                                          , eCall (tInt ~> tInt, "id-4") [eLit (PInt 3)]
--                                          ]
--                                      )
--                                      (eCall (tInt ~> tInt ~> tInt, "add-2") [eLit (PInt 4), eLit (PInt 5)])
--                                  )
--                              )
--                          )
--                      )
--                  )
--              )
--          )
--        ,
--          ( (Scheme ((tInt ~> tInt) ~> tInt ~> tInt), "$lam1")
--          , Function
--              (fromList [(tInt ~> tInt, "x"), (tInt, "$v0")])
--              ( tInt
--              , eCall (tInt ~> tInt, "x") [eVar (tInt, "$v0")]
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
--        ,
--          ( (Scheme (tInt ~> tInt ~> tInt), "$lam3")
--          , Function
--              (fromList [(tInt, "x"), (tInt, "y")])
--              ( tInt
--              , eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt ~> tInt), "$lam4")
--          , Function
--              (fromList [(tInt, "x"), (tInt, "y")])
--              ( tInt
--              , eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))
--              )
--          )
--        ]
--    )

module123 :: ModuleDefs MonoType Ast
module123 =
  Map.fromList
    [ -- func main(a : unit) : int =
      --   let
      --     add2 =
      --       $lam4(2)
      --     in
      --       $lam1(add2, $lam2(3))
      --       +
      --       $lam5(4, 5)

      (
        ( Scheme (tUnit ~> tInt)
        , "main"
        )
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLet
              (tInt ~> tInt, "add2")
              ( eCall
                  ()
                  (tInt ~> tInt ~> tInt, "$lam4")
                  [eLit (PInt 2)]
              )
              ( eOp2
                  oAddInt
                  ( eCall
                      ()
                      ( (tInt ~> tInt) ~> tInt ~> tInt
                      , "$lam1"
                      )
                      [ eVar (tInt ~> tInt, "add2")
                      , eCall
                          ()
                          (tInt ~> tInt, "$lam2")
                          [ eLit (PInt 3)
                          ]
                      ]
                  )
                  ( eCall
                      ()
                      (tInt ~> tInt ~> tInt, "$lam5")
                      [ eLit (PInt 4)
                      , eLit (PInt 5)
                      ]
                  )
              )
          )
      )
    , -- func $lam2(x : int) : int =
      --   x

      (
        ( Scheme (tInt ~> tInt)
        , "$lam2"
        )
      , Function
          (fromList [(tInt, "x")])
          ( tInt
          , eVar (tInt, "x")
          )
      )
    , -- func $lam4(x : int, y : int) : int =
      --   x + y

      (
        ( Scheme (tInt ~> tInt ~> tInt)
        , "$lam4"
        )
      , Function
          (fromList [(tInt, "x"), (tInt, "y")])
          ( tInt
          , eOp2
              oAddInt
              (eVar (tInt, "x"))
              (eVar (tInt, "y"))
          )
      )
    , -- func $lam5(x : int, y : int) : int =
      --   x + y

      (
        ( Scheme (tInt ~> tInt ~> tInt)
        , "$lam5"
        )
      , Function
          (fromList [(tInt, "x"), (tInt, "y")])
          ( tInt
          , eOp2
              oAddInt
              (eVar (tInt, "x"))
              (eVar (tInt, "y"))
          )
      )
    , -- func $lam1(x : int -> int, $v0 : int) : int =
      --   x($v0)

      (
        ( Scheme ((tInt ~> tInt) ~> tInt ~> tInt)
        , "$lam1"
        )
      , Function
          (fromList [(tInt ~> tInt, "x"), (tInt, "$v0")])
          ( tInt
          , eCall
              ()
              (tInt ~> tInt, "x")
              [eVar (tInt, "$v0")]
          )
      )
    ]

eCall_ :: Label t -> [Expr t a0 a1 ()] -> Expr t a0 a1 ()
eCall_ = eCall ()

-- func main(a : unit) : int =
--   let
--     f =
--       lam(g) =>
--         lam(n) =>
--           if n == 0
--             then
--               1
--             else
--               n * g(g, n - 1)
--       in
--         f(f, 5)

module456 :: ModuleDefs MonoType Ast
module456 =
  Map.fromList
    [
      (
        ( Scheme (tUnit ~> tInt)
        , "main"
        )
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eCall_
              (((tVar 22 ~> tInt ~> tInt) ~> tInt ~> tInt) ~> tInt ~> tInt, "$lam1")
              [ eVar ((tVar 22 ~> tInt ~> tInt) ~> tInt ~> tInt, "$lam2")
              , eLit (PInt 5)
              ]
          )
      )
    ,
      (
        ( Scheme (((tVar "a0" ~> tInt ~> tInt) ~> tInt ~> tInt) ~> tInt ~> tInt)
        , "$lam1"
        )
      , Function
          ( fromList
              [ ((tVar 3 ~> tInt ~> tInt) ~> tInt ~> tInt, "g")
              , (tInt, "n")
              ]
          )
          ( tInt
          , eIf
              ( eOp2
                  oEqInt
                  (eVar (tInt, "n"))
                  (eLit (PInt 0))
              )
              (eLit (PInt 1))
              ( eOp2
                  oMulInt
                  (eVar (tInt, "n"))
                  ( eCall_
                      (tVar 3 ~> (tInt ~> tInt) ~> tInt ~> tInt, "g")
                      [ eVar ((tVar 3 ~> tInt ~> tInt) ~> tInt ~> tInt, "g")
                      , eOp2
                          oSubInt
                          (eVar (tInt, "n"))
                          (eLit (PInt 1))
                      ]
                  )
              )
          )
      )
    ,
      (
        ( Scheme (((tVar "a0" ~> tInt ~> tInt) ~> tInt ~> tInt) ~> tInt ~> tInt)
        , "$lam2"
        )
      , Function
          ( fromList
              [ ((tVar 3 ~> tInt ~> tInt) ~> tInt ~> tInt, "g")
              , (tInt, "n")
              ]
          )
          ( tInt
          , eIf
              (eOp2 oEqInt (eVar (tInt, "n")) (eLit (PInt 0)))
              (eLit (PInt 1))
              ( eOp2
                  oMulInt
                  (eVar (tInt, "n"))
                  ( eCall_
                      ((tVar 3 ~> tInt ~> tInt) ~> tInt ~> tInt, "g")
                      [ eVar ((tVar 3 ~> tInt ~> tInt) ~> tInt ~> tInt, "g")
                      , eOp2
                          oSubInt
                          (eVar (tInt, "n"))
                          (eLit (PInt 1))
                      ]
                  )
              )
          )
      )
    ]
