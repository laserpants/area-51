{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.ThePanamaHat where

import Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
import Pong.Data
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
  \    letr\
  \      { a = y | q } =\
  \        r\
  \      in\
  \        y\
  \"

-- "

program200 :: Text
program200 =
  "def main(a : unit) : int =\
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

program201 :: Program () SourceExpr
program201 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [((), "a")])
              ( ()
              , eLet
                  ((), "xs")
                  (eCon ((), "Nil"))
                  ( ePat
                      (eVar ((), "xs"))
                      [ ([((), "Nil")], eLit (PInt 401))
                      , ([((), "Cons"), ((), "y"), ((), "ys")], eLit (PInt 402))
                      ]
                  )
              )
          )
        ]
    )

program202 :: Program MonoType TypedExpr
program202 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                  (tCon "List" [tVar 0], "xs")
                  (eCon (tCon "List" [tVar 0], "Nil"))
                  ( ePat
                      (eVar (tCon "List" [tVar 1], "xs"))
                      [ ([(tCon "List" [tVar 1], "Nil")], eLit (PInt 401))
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
    )

program203 :: Text
program203 =
  "def main(a : unit) : int =\
  \  let\
  \    xs =\
  \      Nil\
  \    in\
  \      match xs\
  \        { Nil => 401\
  \        | Cons(y, ys) => y\
  \        }\
  \"

-- "

program204 :: Program () SourceExpr
program204 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [((), "a")])
              ( ()
              , eLet
                  ((), "xs")
                  (eCon ((), "Nil"))
                  ( ePat
                      (eVar ((), "xs"))
                      [ ([((), "Nil")], eLit (PInt 401))
                      , ([((), "Cons"), ((), "y"), ((), "ys")], eVar ((), "y"))
                      ]
                  )
              )
          )
        ]
    )

program205 :: Program MonoType TypedExpr
program205 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
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
    )

program206 :: Program MonoType TypedExpr
program206 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                  (tCon "List" [tInt], "v$-xs-1")
                  (eCon (tCon "List" [tInt], "Nil"))
                  ( ePat
                      (eVar (tCon "List" [tInt], "v$-xs-1"))
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
    )

program207 :: Program MonoType TypedExpr
program207 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                  (tCon "List" [tInt], "v$-xs-1")
                  (eCon (tCon "List" [tInt], "Nil"))
                  ( eLet
                      (tCon "List" [tVar 0], "xs")
                      (eCon (tCon "List" [tVar 0], "Nil"))
                      ( ePat
                          (eVar (tCon "List" [tInt], "v$-xs-1"))
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
    )

program217 :: Program MonoType Ast
program217 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
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
    )

program208 :: Text
program208 =
  "def main(a : unit) : int =\
  \  let\
  \    xs =\
  \      Cons(5, Nil)\
  \    in\
  \      match xs\
  \        { Nil => 401\
  \        | Cons(y, ys) => y\
  \        }\
  \"

-- "

program209 :: Program () SourceExpr
program209 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [((), "a")])
              ( ()
              , eLet
                  ((), "xs")
                  (eApp () (eCon ((), "Cons")) [eLit (PInt 5), eCon ((), "Nil")])
                  ( ePat
                      (eVar ((), "xs"))
                      [ ([((), "Nil")], eLit (PInt 401))
                      , ([((), "Cons"), ((), "y"), ((), "ys")], eVar ((), "y"))
                      ]
                  )
              )
          )
        ]
    )

program210 :: Program MonoType TypedExpr
program210 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
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
    )

program271 :: Program MonoType Ast
program271 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                  (tCon "List" [tInt], "xs")
                  ( eCall
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
    )

program212 :: Text
program212 =
  "def main(a : unit) : int =\
  \  let\
  \    xs =\
  \      Cons(5, Cons(4, Cons(3, Nil)))\
  \    in\
  \      match xs\
  \        { Nil => 401\
  \        | Cons(y, ys) =>\
  \            match ys\
  \              { Nil => 404\
  \              | Cons(z, zs) => z\
  \              }\
  \        }\
  \"

-- "

-- program300 :: Text
-- program300 =
--  "\
--  \def main(a : unit) : int =\
--  \  let\
--  \    xs =\
--  \      Nil()\
--  \    in\
--  \      match xs\
--  \        { Nil => 401\
--  \        | Cons(y, ys) => 402\
--  \        }\
--  \"
--
---- "

-- program301 :: Program () SourceExpr
-- program301 =
--  Program
--    ( Map.fromList
--        [
--          ( (Scheme (tVar "a" ~> tCon "List" [tVar "a"] ~> tCon "List" [tVar "a"]), "Cons")
--          , Function
--              (fromList [(tVar 0, "x"), ((), "xs")])
--              ((), eVar ((), "foo"))
--          )
--        , ( (Scheme (tCon "List" [tVar "a"]), "Nil")
--          , Constant ((), eVar ((), "foo"))
--          )
--        , ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [((), "a")])
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

--          ( (Scheme (tVar "a" ~> tCon "List" [tVar "a"] ~> tCon "List" [tVar "a"]), "Cons")
--          , Function
--              (fromList [(tVar 0, "x"), (tCon "List" [tVar 0], "xs")])
--              ( tCon "List" [tVar 0]
--              , eVar (tCon "List" [tVar 0], "foo")
--              )
--          )
--        , ( (Scheme (tCon "List" [tVar "a"]), "Nil")
--          , Constant
--              ( tCon "List" [tVar 0]
--              , eVar (tCon "List" [tVar 0], "foo")
--              )
--          )

program302 :: Program MonoType TypedExpr
program302 =
  Program
    ( Map.fromList
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
    )

--  print_int(main1())

-- program300 :: Text
-- program300 =
--   "\
--   \extern print_int : int -> int\
--   \\r\n\
--   \const nil : List a =\
--   \  foo\
--   \\r\n\
--   \def cons(x : a, xs : List a) : List a =\
--   \  foo\
--   \\r\n\
--   \def main(a : unit) : int =\
--   \  print_int(main1( () ))\
--   \\r\n\
--   \def main1(a : unit) : int =\
--   \  let\
--   \    xs =\
--   \      Cons(111, Nil())\
--   \    in\
--   \      match xs\
--   \        { Nil => 401\
--   \        | Cons(y, ys) => 402\
--   \        }\
--   \"
--
-- -- "

program303 :: Program MonoType TypedExpr
program303 =
  Program
    ( Map.fromList
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
    )

program304 :: Program MonoType TypedExpr
program304 =
  Program
    ( Map.fromList
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
                  (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "C$-Cons-1")
                  ( eLam
                      ()
                      [(tInt, "x"), (tCon "List" [tInt], "xs")]
                      (eVar (tCon "List" [tInt], "foo"))
                  )
                  ( eLet
                      (tCon "List" [tInt], "xs")
                      ( eApp
                          (tCon "List" [tInt])
                          (eCon (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "C$-Cons-1"))
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
    )

expr305 :: Ast
expr305 =
  eLet
    (tCon "List" [tInt], "xs")
    (eVar (tCon "List" [tInt], "Nil"))
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

expr306 :: Ast
expr306 =
  eLet
    (tCon "List" [tInt], "xs")
    ( eCall
        (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
        [ eLit (PInt 5)
        , eVar (tCon "List" [tInt], "Nil")
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

program305 :: Program MonoType TypedExpr
program305 =
  Program
    ( Map.fromList
        [
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
              , eLet
                  (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "C$-Cons-1")
                  ( eLam
                      ()
                      [(tInt, "x"), (tCon "List" [tInt], "xs")]
                      ( eApp
                          (tCon "List" [tInt])
                          (eVar (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "C$-Cons-1"))
                          [eVar (tInt, "x"), eVar (tCon "List" [tInt], "xs")]
                      )
                  )
                  ( eLet
                      (tCon "List" [tInt], "xs")
                      ( eApp
                          (tCon "List" [tInt])
                          (eCon (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "C$-Cons-1"))
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
    )

program306 :: Program MonoType TypedExpr
program306 =
  Program
    ( Map.fromList
        [
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
          , Constant (tCon "List" [tVar 0], eVar (tCon "List" [tVar 0], "Nil"))
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
    )

program400 :: Text
program400 =
  "\
  \type List a\
  \  = Nil\
  \  | Cons a (List a)\
  \\r\n\
  \def main(a : unit) : int =\
  \  print_int(runner())\
  \\r\n\
  \def runner(a : unit) : int =\
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
