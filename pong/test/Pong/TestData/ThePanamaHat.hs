{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.ThePanamaHat where

import Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
-- import Pong.Data
import Pong.Lang
import Pong.Util

expr101 :: Text
expr101 =
  "let\
  \  x =\
  \    5\
  \  in\
  \    y\
  \"

-- "

expr102 :: Text
expr102 =
  "let\
  \  x =\
  \    5\
  \  in\
  \    z\
  \"

-- "

expr113 :: Text
expr113 =
  "let\
  \  x =\
  \    5\
  \  in\
  \    let\
  \      a =\
  \        6\
  \      in\
  \        y\
  \"

-- "

expr114 :: Text
expr114 =
  "let\
  \  x =\
  \    5\
  \  in\
  \    let\
  \      a =\
  \        6\
  \      in\
  \        z\
  \"

-- "

expr103 :: Text
expr103 =
  "let\
  \  x =\
  \    5\
  \  in\
  \    let\
  \      y =\
  \        6\
  \      in\
  \        y\
  \"

-- "

expr104 :: Text
expr104 =
  "let\
  \  x =\
  \    y\
  \  in\
  \    match xs {\
  \      | A(y) => y\
  \      | B(x) => y\
  \    }\
  \"

-- "

expr105 :: Text
expr105 =
  "let\
  \  x =\
  \    z\
  \  in\
  \    match xs {\
  \      | A(y) => y\
  \      | B(x) => z\
  \    }\
  \"

-- "

expr106 :: Text
expr106 =
  "let\
  \  r =\
  \    { a = 5 }\
  \  in\
  \    field\
  \      { a = y | q } =\
  \        r\
  \      in\
  \        y\
  \"

-- "

expr1062 :: Text
expr1062 =
  "let\
  \  r =\
  \    { a = 5 }\
  \  in\
  \    match r\
  \      { a = y | q } =>\
  \        y\
  \"

-- "


-- program200 :: Text
-- program200 =
--  "def main(_ : unit) : int =\
--  \  let\
--  \    xs =\
--  \      Nil\
--  \    in\
--  \      match xs\
--  \        { Nil => 401\
--  \        | Cons(y, ys) => 402\
--  \        }\
--  \"
--
---- "
--
-- program201 :: Program () SourceExpr
-- program201 =
--  Program
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [((), "_")])
--              ( ()
--              , eLet
--                  ((), "xs")
--                  (eCon ((), "Nil"))
--                  ( ePat
--                      (eVar ((), "xs"))
--                      [ ([((), "Nil")], eLit (PInt 401))
--                      , ([((), "Cons"), ((), "y"), ((), "ys")], eLit (PInt 402))
--                      ]
--                  )
--              )
--          )
--        ]
--    )
--
-- program202 :: Program MonoType TypedExpr
-- program202 =
--  Program
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "_")])
--              ( tInt
--              , eLet
--                  (tCon "List" [tVar 0], "xs")
--                  (eCon (tCon "List" [tVar 0], "Nil"))
--                  ( ePat
--                      (eVar (tCon "List" [tVar 1], "xs"))
--                      [ ([(tCon "List" [tVar 1], "Nil")], eLit (PInt 401))
--                      ,
--                        (
--                          [ (tVar 1 ~> tCon "List" [tVar 1] ~> tCon "List" [tVar 1], "Cons")
--                          , (tVar 1, "y")
--                          , (tCon "List" [tVar 1], "ys")
--                          ]
--                        , eLit (PInt 402)
--                        )
--                      ]
--                  )
--              )
--          )
--        ]
--    )
--
-- program203 :: Text
-- program203 =
--  "def main(_ : unit) : int =\
--  \  let\
--  \    xs =\
--  \      Nil\
--  \    in\
--  \      match xs\
--  \        { Nil => 401\
--  \        | Cons(y, ys) => y\
--  \        }\
--  \"
--
---- "
--
-- program204 :: Program () SourceExpr
-- program204 =
--  Program
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [((), "_")])
--              ( ()
--              , eLet
--                  ((), "xs")
--                  (eCon ((), "Nil"))
--                  ( ePat
--                      (eVar ((), "xs"))
--                      [ ([((), "Nil")], eLit (PInt 401))
--                      , ([((), "Cons"), ((), "y"), ((), "ys")], eVar ((), "y"))
--                      ]
--                  )
--              )
--          )
--        ]
--    )

program205 :: ModuleDefs MonoType TypedExpr
program205 =
  Map.fromList
    [
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "_")])
          ( tInt
          , eLet
              (tCon "List" [tVar 0], "xs")
              (eCon (tCon "List" [tVar 0], "Nil"))
              ( ePat
                  (eVar (tCon "List" [tInt], "xs"))
                  [ ([(tCon "List" [tInt], "Nil")], eLit (PInt 401))
                  ,
                    (
                      [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
                      , (tInt, "y")
                      , (tCon "List" [tInt], "ys")
                      ]
                    , eVar (tInt, "y")
                    )
                  ]
              )
          )
      )
    ]

program206 :: ModuleDefs MonoType TypedExpr
program206 =
  Map.fromList
    [
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "_")])
          ( tInt
          , eLet
              (tCon "List" [tInt], "xs-1")
              (eCon (tCon "List" [tInt], "Nil"))
              ( ePat
                  (eVar (tCon "List" [tInt], "xs-1"))
                  [ ([(tCon "List" [tInt], "Nil")], eLit (PInt 401))
                  ,
                    (
                      [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
                      , (tInt, "y")
                      , (tCon "List" [tInt], "ys")
                      ]
                    , eVar (tInt, "y")
                    )
                  ]
              )
          )
      )
    ]

program207 :: ModuleDefs MonoType TypedExpr
program207 =
  Map.fromList
    [
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "_")])
          ( tInt
          , eLet
              (tCon "List" [tInt], "xs-1")
              (eCon (tCon "List" [tInt], "Nil"))
              ( eLet
                  (tCon "List" [tVar 0], "xs")
                  (eCon (tCon "List" [tVar 0], "Nil"))
                  ( ePat
                      (eVar (tCon "List" [tInt], "xs-1"))
                      [ ([(tCon "List" [tInt], "Nil")], eLit (PInt 401))
                      ,
                        (
                          [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
                          , (tInt, "y")
                          , (tCon "List" [tInt], "ys")
                          ]
                        , eVar (tInt, "y")
                        )
                      ]
                  )
              )
          )
      )
    ]

program217 :: ModuleDefs MonoType Ast
program217 =
  Map.fromList
    [
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "_")])
          ( tInt
          , ePat
              (eVar (tCon "List" [tInt], "Nil"))
              [ ([(tCon "List" [tInt], "Nil")], eLit (PInt 401))
              ,
                (
                  [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
                  , (tInt, "y")
                  , (tCon "List" [tInt], "ys")
                  ]
                , eVar (tInt, "y")
                )
              ]
          )
      )
    ]

-- program208 :: Text
-- program208 =
--  "def main(_ : unit) : int =\
--  \  let\
--  \    xs =\
--  \      Cons(5, Nil)\
--  \    in\
--  \      match xs\
--  \        { Nil => 401\
--  \        | Cons(y, ys) => y\
--  \        }\
--  \"
--
---- "
--
-- program209 :: Program () SourceExpr
-- program209 =
--  Program
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [((), "_")])
--              ( ()
--              , eLet
--                  ((), "xs")
--                  (eApp () (eCon ((), "Cons")) [eLit (PInt 5), eCon ((), "Nil")])
--                  ( ePat
--                      (eVar ((), "xs"))
--                      [ ([((), "Nil")], eLit (PInt 401))
--                      , ([((), "Cons"), ((), "y"), ((), "ys")], eVar ((), "y"))
--                      ]
--                  )
--              )
--          )
--        ]
--    )

program210 :: ModuleDefs MonoType TypedExpr
program210 =
  Map.fromList
    [
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "_")])
          ( tInt
          , eLet
              (tCon "List" [tInt], "xs")
              ( eApp
                  (tCon "List" [tInt])
                  (eCon (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons"))
                  [ eLit (PInt 5)
                  , eCon (tCon "List" [tInt], "Nil")
                  ]
              )
              ( ePat
                  (eVar (tCon "List" [tInt], "xs"))
                  [ ([(tCon "List" [tInt], "Nil")], eLit (PInt 401))
                  ,
                    (
                      [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
                      , (tInt, "y")
                      , (tCon "List" [tInt], "ys")
                      ]
                    , eVar (tInt, "y")
                    )
                  ]
              )
          )
      )
    ]

program271 :: ModuleDefs MonoType Ast
program271 =
  Map.fromList
    [
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "_")])
          ( tInt
          , eLet
              (tCon "List" [tInt], "xs")
              ( eCall
                  ()
                  (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
                  [ eLit (PInt 5)
                  , eVar (tCon "List" [tInt], "Nil")
                  ]
              )
              ( ePat
                  (eVar (tCon "List" [tInt], "xs"))
                  [ ([(tCon "List" [tInt], "Nil")], eLit (PInt 401))
                  ,
                    (
                      [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
                      , (tInt, "y")
                      , (tCon "List" [tInt], "ys")
                      ]
                    , eVar (tInt, "y")
                    )
                  ]
              )
          )
      )
    ]

-- program212 :: Text
-- program212 =
--  "def main(_ : unit) : int =\
--  \  let\
--  \    xs =\
--  \      Cons(5, Cons(4, Cons(3, Nil)))\
--  \    in\
--  \      match xs\
--  \        { Nil => 401\
--  \        | Cons(y, ys) =>\
--  \            match ys\
--  \              { Nil => 404\
--  \              | Cons(z, zs) => z\
--  \              }\
--  \        }\
--  \"
--
---- "
--
---- program300 :: Text
---- program300 =
----  "\
----  \def main(_ : unit) : int =\
----  \  let\
----  \    xs =\
----  \      Nil()\
----  \    in\
----  \      match xs\
----  \        { Nil => 401\
----  \        | Cons(y, ys) => 402\
----  \        }\
----  \"
----
------ "
--
---- program301 :: Program () SourceExpr
---- program301 =
----  Program
----    ( Map.fromList
----        [
----          ( (Scheme (tVar "a" ~> tCon "List" [tVar "a"] ~> tCon "List" [tVar "a"]), "Cons")
----          , Function
----              (fromList [(tVar 0, "x"), ((), "xs")])
----              ((), eVar ((), "foo"))
----          )
----        , ( (Scheme (tCon "List" [tVar "a"]), "Nil")
----          , Constant ((), eVar ((), "foo"))
----          )
----        , ( (Scheme (tUnit ~> tInt), "main")
----          , Function
----              (fromList [((), "a")])
----              ( ()
----              , eLet
----                  ((), "xs")
----                  (eCon ((), "Nil"))
----                  ( ePat
----                      (eVar ((), "xs"))
----                      [ ([((), "Nil")], eLit (PInt 401))
----                      , ([((), "Cons"), ((), "y"), ((), "ys")], eLit (PInt 402))
----                      ]
----                  )
----              )
----          )
----        ]
----    )
--
----          ( (Scheme (tVar "a" ~> tCon "List" [tVar "a"] ~> tCon "List" [tVar "a"]), "Cons")
----          , Function
----              (fromList [(tVar 0, "x"), (tCon "List" [tVar 0], "xs")])
----              ( tCon "List" [tVar 0]
----              , eVar (tCon "List" [tVar 0], "foo")
----              )
----          )
----        , ( (Scheme (tCon "List" [tVar "a"]), "Nil")
----          , Constant
----              ( tCon "List" [tVar 0]
----              , eVar (tCon "List" [tVar 0], "foo")
----              )
----          )

program302 :: ModuleDefs MonoType TypedExpr
program302 =
  Map.fromList
    [
      ( (Scheme (tCon "List" [tVar "a"]), "List")
      , Data
          "List"
          [ ("Nil", [])
          , ("Cons", [tVar "a", tCon "List" [tVar "a"]])
          ]
      )
    ,
      ( (Scheme (tInt ~> tInt), "print_int")
      , Extern [tInt] tInt
      )
    ,
      ( (Scheme (tVar "a" ~> tCon "List" [tVar "a"] ~> tCon "List" [tVar "a"]), "Cons")
      , Function
          (fromList [(tVar 0, "x"), (tCon "List" [tVar 0], "xs")])
          (tCon "List" [tVar 0], eVar (tCon "List" [tVar 0], "foo"))
      )
    ,
      ( (Scheme (tCon "List" [tVar "a"]), "Nil")
      , Constant (tCon "List" [tVar 0], eVar (tCon "List" [tVar 0], "foo"))
      )
    ,
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eApp tInt (eVar (tInt ~> tInt, "print_int")) [eApp tInt (eVar (tUnit ~> tInt, "main1")) [eLit PUnit]]
          )
      )
    ,
      ( (Scheme (tUnit ~> tInt), "main1")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLet
              (tCon "List" [tVar 0], "xs")
              (eCon (tCon "List" [tVar 0], "Nil"))
              ( ePat
                  (eVar (tCon "List" [tVar 1], "xs"))
                  [
                    (
                      [ (tCon "List" [tVar 1], "Nil")
                      ]
                    , eLit (PInt 401)
                    )
                  ,
                    (
                      [ (tVar 1 ~> tCon "List" [tVar 1] ~> tCon "List" [tVar 1], "Cons")
                      , (tVar 1, "y")
                      , (tCon "List" [tVar 1], "ys")
                      ]
                    , eLit (PInt 402)
                    )
                  ]
              )
          )
      )
    ]

----  print_int(main1())
--
---- program300 :: Text
---- program300 =
----   "\
----   \extern print_int : int -> int\
----   \\r\n\
----   \const nil : List a =\
----   \  foo\
----   \\r\n\
----   \def cons(x : a, xs : List a) : List a =\
----   \  foo\
----   \\r\n\
----   \def main(_ : unit) : int =\
----   \  print_int(main1( () ))\
----   \\r\n\
----   \def main1(_ : unit) : int =\
----   \  let\
----   \    xs =\
----   \      Cons(111, Nil())\
----   \    in\
----   \      match xs\
----   \        { Nil => 401\
----   \        | Cons(y, ys) => 402\
----   \        }\
----   \"
----
---- -- "

program303 :: ModuleDefs MonoType TypedExpr
program303 =
  Map.fromList
    [
      ( (Scheme (tInt ~> tInt), "print_int")
      , Extern [tInt] tInt
      )
    ,
      ( (Scheme (tVar "a" ~> tCon "List" [tVar "a"] ~> tCon "List" [tVar "a"]), "Cons")
      , Function
          (fromList [(tVar 0, "x"), (tCon "List" [tVar 0], "xs")])
          (tCon "List" [tVar 0], eVar (tCon "List" [tVar 0], "foo"))
      )
    ,
      ( (Scheme (tCon "List" [tVar "a"]), "Nil")
      , Constant (tCon "List" [tVar 0], eVar (tCon "List" [tVar 0], "foo"))
      )
    ,
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eApp
              tInt
              (eVar (tInt ~> tInt, "print_int"))
              [ eApp tInt (eVar (tUnit ~> tInt, "main1")) [eLit PUnit]
              ]
          )
      )
    ,
      ( (Scheme (tUnit ~> tInt), "main1")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLet
              (tCon "List" [tInt], "xs")
              ( eApp
                  (tCon "List" [tInt])
                  (eCon (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons"))
                  [ eLit (PInt 111)
                  , eCon (tCon "List" [tInt], "Nil")
                  ]
              )
              ( ePat
                  (eVar (tCon "List" [tInt], "xs"))
                  [
                    (
                      [ (tCon "List" [tInt], "Nil")
                      ]
                    , eLit (PInt 401)
                    )
                  ,
                    (
                      [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
                      , (tInt, "y")
                      , (tCon "List" [tInt], "ys")
                      ]
                    , eLit (PInt 402)
                    )
                  ]
              )
          )
      )
    ]

program304 :: ModuleDefs MonoType TypedExpr
program304 =
  Map.fromList
    [
      ( (Scheme (tInt ~> tInt), "print_int")
      , Extern [tInt] tInt
      )
    ,
      ( (Scheme (tVar "a" ~> tCon "List" [tVar "a"] ~> tCon "List" [tVar "a"]), "Cons")
      , Function
          (fromList [(tVar 0, "x"), (tCon "List" [tVar 0], "xs")])
          (tCon "List" [tVar 0], eVar (tCon "List" [tVar 0], "foo"))
      )
    ,
      ( (Scheme (tCon "List" [tVar "a"]), "Nil")
      , Constant (tCon "List" [tVar 0], eVar (tCon "List" [tVar 0], "foo"))
      )
    ,
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eApp
              tInt
              (eVar (tInt ~> tInt, "print_int"))
              [ eApp tInt (eVar (tUnit ~> tInt, "main1")) [eLit PUnit]
              ]
          )
      )
    ,
      ( (Scheme (tUnit ~> tInt), "main1")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLet
              (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons-1")
              ( eLam
                  ()
                  [(tInt, "x"), (tCon "List" [tInt], "xs")]
                  (eVar (tCon "List" [tInt], "foo"))
              )
              ( eLet
                  (tCon "List" [tInt], "xs")
                  ( eApp
                      (tCon "List" [tInt])
                      (eCon (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons-1"))
                      [ eLit (PInt 111)
                      , eCon (tCon "List" [tInt], "Nil")
                      ]
                  )
                  ( ePat
                      (eVar (tCon "List" [tInt], "xs"))
                      [
                        (
                          [ (tCon "List" [tInt], "Nil")
                          ]
                        , eLit (PInt 401)
                        )
                      ,
                        (
                          [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
                          , (tInt, "y")
                          , (tCon "List" [tInt], "ys")
                          ]
                        , eLit (PInt 402)
                        )
                      ]
                  )
              )
          )
      )
    ]

-- expr305 :: Ast
-- expr305 =
--  eLet
--    (tCon "List" [tInt], "xs")
--    (eVar (tCon "List" [tInt], "Nil"))
--    ( ePat
--        (eVar (tCon "List" [tInt], "xs"))
--        [
--          (
--            [ (tCon "List" [tInt], "Nil")
--            ]
--          , eLit (PInt 401)
--          )
--        ,
--          (
--            [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
--            , (tInt, "y")
--            , (tCon "List" [tInt], "ys")
--            ]
--          , eLit (PInt 402)
--          )
--        ]
--    )
--
-- expr306 :: Ast
-- expr306 =
--  eLet
--    (tCon "List" [tInt], "xs")
--    ( eCall
--        (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
--        [ eLit (PInt 5)
--        , eVar (tCon "List" [tInt], "Nil")
--        ]
--    )
--    ( ePat
--        (eVar (tCon "List" [tInt], "xs"))
--        [
--          (
--            [ (tCon "List" [tInt], "Nil")
--            ]
--          , eLit (PInt 401)
--          )
--        ,
--          (
--            [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
--            , (tInt, "y")
--            , (tCon "List" [tInt], "ys")
--            ]
--          , eLit (PInt 402)
--          )
--        ]
--    )
--
-- program305 :: Program MonoType TypedExpr
-- program305 =
--  Program
--    ( Map.fromList
--        [
--          ( (Scheme (tInt ~> tInt), "print_int")
--          , Extern [tInt] tInt
--          )
--        ,
--          ( (Scheme (tVar "a" ~> tCon "List" [tVar "a"] ~> tCon "List" [tVar "a"]), "Cons")
--          , Function
--              (fromList [(tVar 0, "x"), (tCon "List" [tVar 0], "xs")])
--              ( tCon "List" [tVar 0]
--              , eVar (tCon "List" [tVar 0], "{{data}}")
--              )
--          )
--        ,
--          ( (Scheme (tCon "List" [tVar "a"]), "Nil")
--          , Constant (tCon "List" [tVar 0], eVar (tCon "List" [tVar 0], "{{data}}"))
--          )
--        ,
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons-1")
--                  ( eLam
--                      ()
--                      [(tInt, "x"), (tCon "List" [tInt], "xs")]
--                      ( eApp
--                          (tCon "List" [tInt])
--                          (eVar (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons-1"))
--                          [eVar (tInt, "x"), eVar (tCon "List" [tInt], "xs")]
--                      )
--                  )
--                  ( eLet
--                      (tCon "List" [tInt], "xs")
--                      ( eApp
--                          (tCon "List" [tInt])
--                          (eCon (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons-1"))
--                          [ eLit (PInt 111)
--                          , eCon (tCon "List" [tInt], "Nil")
--                          ]
--                      )
--                      ( ePat
--                          (eVar (tCon "List" [tInt], "xs"))
--                          [
--                            (
--                              [ (tCon "List" [tInt], "Nil")
--                              ]
--                            , eLit (PInt 401)
--                            )
--                          ,
--                            (
--                              [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
--                              , (tInt, "y")
--                              , (tCon "List" [tInt], "ys")
--                              ]
--                            , eLit (PInt 402)
--                            )
--                          ]
--                      )
--                  )
--              )
--          )
--        ]
--    )

program306 :: ModuleDefs MonoType TypedExpr
program306 =
  Map.fromList
    [
      ( (Scheme (tCon "List" [tVar "a"]), "List")
      , Data
          "List"
          [ ("Nil", [])
          , ("Cons", [tVar "a", tCon "List" [tVar "a"]])
          ]
      )
    ,
      ( (Scheme (tInt ~> tInt), "print_int")
      , Extern [tInt] tInt
      )
    ,
      ( (Scheme (tVar "a" ~> tCon "List" [tVar "a"] ~> tCon "List" [tVar "a"]), "Cons")
      , Function
          (fromList [(tVar 0, "x"), (tCon "List" [tVar 0], "xs")])
          ( tCon "List" [tVar 0]
          , eVar (tCon "List" [tVar 0], "{{data}}")
          )
          -- , eApp
          --    (tCon "List" [tVar 0])
          --    (eVar (tVar 0 ~> tCon "List" [tVar 0] ~> tCon "List" [tVar 0], "Cons"))
          --    [eVar (tVar 0, "x"), eVar (tCon "List" [tVar 0], "xs")]
          -- )
      )
    ,
      ( (Scheme (tCon "List" [tVar "a"]), "Nil")
      , Constant (tCon "List" [tVar 0], eVar (tCon "List" [tVar 0], "{{data}}"))
      )
    ,
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eApp tInt (eVar (tInt ~> tInt, "print_int")) [eApp tInt (eVar (tUnit ~> tInt, "main1")) [eLit PUnit]]
          )
      )
    ,
      ( (Scheme (tUnit ~> tInt), "main1")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLet
              (tCon "List" [tInt], "xs")
              ( eApp
                  (tCon "List" [tInt])
                  (eCon (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons"))
                  [ eLit (PInt 111)
                  , eCon (tCon "List" [tInt], "Nil")
                  ]
              )
              ( ePat
                  (eVar (tCon "List" [tInt], "xs"))
                  [
                    (
                      [ (tCon "List" [tInt], "Nil")
                      ]
                    , eLit (PInt 401)
                    )
                  ,
                    (
                      [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
                      , (tInt, "y")
                      , (tCon "List" [tInt], "ys")
                      ]
                    , eLit (PInt 402)
                    )
                  ]
              )
          )
      )
    ]

program400 :: Text
program400 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \type List a\
  \  = Nil\
  \  | Cons(a, List a)\
  \\r\n\
  \func main(_ : unit) : int =\
  \  print_int(runner())\
  \\r\n\
  \func runner(_ : unit) : int =\
  \  let\
  \    xs =\
  \      Nil\
  \    in\
  \      match xs\
  \        { Nil => 401\
  \        | Cons(y, ys) => 402\
  \        }\
  \"

-- "

program500 :: Text
program500 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \type List a\
  \  = Nil\
  \  | Cons(a, List a)\
  \\r\n\
  \func main(_ : unit) : int =\
  \  print_int(runner())\
  \\r\n\
  \func runner(_ : unit) : int =\
  \  let\
  \    xs =\
  \      Cons(4, Nil)\
  \    in\
  \      match xs\
  \        { Nil => 401\
  \        | Cons(y, ys) => 402\
  \        }\
  \"

-- "

program501 :: Text
program501 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \type List a\
  \  = Nil\
  \  | Cons(a, List a)\
  \\r\n\
  \func main(_ : unit) : int =\
  \  print_int(runner())\
  \\r\n\
  \func runner(_ : unit) : int =\
  \  let\
  \    xs =\
  \      Cons(4, Nil)\
  \    in\
  \      match xs\
  \        { Nil => 401\
  \        | Cons(y, ys) => y\
  \        }\
  \"

-- "

program600 :: Text
program600 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \type Either a b\
  \  = Left(a)\
  \  | Right(b)\
  \\r\n\
  \func main(_ : unit) : int =\
  \  print_int(runner())\
  \\r\n\
  \func runner(_ : unit) : int =\
  \  let\
  \    xs =\
  \      Left(5)\
  \    in\
  \      match xs\
  \        { Left(a) => 1\
  \        | Right(b) => 2\
  \        }\
  \"

-- "

program601 :: Text
program601 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \type Either a b\
  \  = Left(a)\
  \  | Right(b)\
  \\r\n\
  \func main(_ : unit) : int =\
  \  print_int(runner())\
  \\r\n\
  \func runner(_ : unit) : int =\
  \  let\
  \    xs =\
  \      Right(5)\
  \    in\
  \      match xs\
  \        { Left(a) => 1\
  \        | Right(b) => 2\
  \        }\
  \"

-- "

program602 :: Text
program602 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \type Either a b\
  \  = Left(a)\
  \  | Right(b)\
  \\r\n\
  \func main(_ : unit) : int =\
  \  let\
  \    r =\
  \      runner()\
  \    in\
  \      print_int(r)\
  \\r\n\
  \func runner(_ : unit) : int =\
  \  let\
  \    ys =\
  \      Left(50)\
  \    in\
  \      let\
  \        xs =\
  \          Right(5)\
  \        in\
  \          match xs\
  \            { Left(a) => a\
  \            | Right(b) =>\
  \                match ys\
  \                  { Left(c) => c\
  \                  | Right(d) => 0\
  \                  }\
  \            }\
  \"

-- "

program603 :: Text
program603 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \type Either a b\
  \  = This(a)\
  \  | That(b)\
  \\r\n\
  \const num : int =\
  \  5\
  \\r\n\
  \func fact(n : int) : int =\
  \  if 0 == n\
  \    then\
  \      1\
  \     else\
  \      n * fact(n - 1)\
  \\r\n\
  \func main(_ : unit) : int =\
  \  let\
  \    res =\
  \      runner()\
  \    in\
  \      print_int(res)\
  \\r\n\
  \func runner(_ : unit) : int =\
  \  let\
  \    ys =\
  \      This(50)\
  \    in\
  \      let\
  \        xs =\
  \          That(5)\
  \        in\
  \          match xs\
  \            { This(a) => a\
  \            | That(b) =>\
  \                match ys\
  \                  { This(c) => 1 + fact(num)\
  \                  | That(d) => 0\
  \                  }\
  \            }\
  \"

-- "

program604 :: Text
program604 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \type Either a b\
  \  = This(a)\
  \  | That(b)\
  \\r\n\
  \const succ : int -> int =\
  \  lam(x) =>\
  \    x + 1\
  \\r\n\
  \func main(_ : unit) : int =\
  \  let\
  \    res =\
  \      runner()\
  \    in\
  \      print_int(res)\
  \\r\n\
  \func runner(_ : unit) : int =\
  \  succ(5)\
  \"

-- "

program605 :: Text
program605 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \type Either a b\
  \  = This(a)\
  \  | That(b)\
  \\r\n\
  \const succ : int -> int =\
  \  lam(x) =>\
  \    x + 1\
  \\r\n\
  \func main(_ : unit) : int =\
  \  let\
  \    res =\
  \      runner()\
  \    in\
  \      print_int(res)\
  \\r\n\
  \func runner(_ : unit) : int =\
  \  let\
  \    a =\
  \      This(122)\
  \    in\
  \      match a {\
  \        | This(x) => x + 1\
  \        | That(y) => y + 1\
  \      }\
  \"

-- "

program615 :: Text
program615 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \type Either a b\
  \  = This(a)\
  \  | That(b)\
  \\r\n\
  \func main(_ : unit) : int =\
  \  let\
  \    res =\
  \      runner()\
  \    in\
  \      print_int(res)\
  \\r\n\
  \func succ(x : int) : int =\
  \  x + 1\
  \\r\n\
  \func mapEither(f : int -> int, th : Either int int) : int =\
  \  match th {\
  \    | This(l) => f(l)\
  \    | That(r) => f(r)\
  \  }\
  \\r\n\
  \func runner(_ : unit) : int =\
  \  let\
  \    a =\
  \      This(120)\
  \    in\
  \      mapEither(succ, a)\
  \"

-- "

program616 :: Text
program616 =
  "\
  \module Main\
  \\r\n\
  \extern print_int : int -> int\
  \\r\n\
  \type Either a b\
  \  = This(a)\
  \  | That(b)\
  \\r\n\
  \func main(_ : unit) : int =\
  \  let\
  \    res =\
  \      runner()\
  \    in\
  \      print_int(res)\
  \\r\n\
  \func mapEither(f : int -> int, th : Either int int) : int =\
  \  match th {\
  \    | This(l) => f(l)\
  \    | That(r) => f(r)\
  \  }\
  \\r\n\
  \func runner(_ : unit) : int =\
  \  let\
  \    a =\
  \      This(120)\
  \    in\
  \      mapEither(lam(x) => x * 2, a)\
  \"

-- "
