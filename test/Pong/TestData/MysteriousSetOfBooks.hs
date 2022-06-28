{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.MysteriousSetOfBooks where

import Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
import Pong.Data
import Pong.Lang
import Pong.Type
import Pong.Util

program20 :: Text
program20 =
  "def foo(a : unit) : int =\
  \  5\
  \\r\n\
  \def main(_ : unit) : int =\
  \  let\
  \    r =\
  \      { a = 5, b = true }\
  \    in\
  \      let\
  \        q =\
  \          { c = 1 | r }\
  \        in\
  \          letr\
  \            { a = x | s } =\
  \              q\
  \            in\
  \              if\
  \                x == foo(())\
  \                then\
  \                  100\
  \                else\
  \                  200\
  \"

-- "

program21 :: Text
program21 =
  "def baz(a : unit) : { a : int, b : bool } =\
  \  { a = 5, b = true }\
  \\r\n\
  \def foo(a : unit) : int =\
  \  5\
  \\r\n\
  \def main(a : unit) : int =\
  \  let\
  \    r =\
  \      baz(())\
  \    in\
  \      let\
  \        q =\
  \          { c = 1 | r }\
  \        in\
  \          letr\
  \            { a = x | s } =\
  \              q\
  \            in\
  \              if\
  \                x == foo(())\
  \                then\
  \                  101\
  \                else\
  \                  200\
  \"

-- "

program22 :: Text
program22 =
  "const foo : int =\
  \  5\
  \\r\n\
  \def main(a : unit) : int =\
  \  let\
  \    r =\
  \      { a = 5, b = true }\
  \    in\
  \      let\
  \        q =\
  \          { c = 1 | r }\
  \        in\
  \          letr\
  \            { a = x | s } =\
  \              q\
  \            in\
  \              if\
  \                x == foo\
  \                then\
  \                  102\
  \                else\
  \                  200\
  \"

-- "

program23 :: Text
program23 =
  "const foo : int =\
  \  5\
  \\r\n\
  \def main(a : unit) : int =\
  \  let\
  \    r =\
  \      { a = 5, b = true }\
  \    in\
  \      let\
  \        q =\
  \          { c = 1 | r }\
  \        in\
  \          letr\
  \            { a = x | s } =\
  \              q\
  \            in\
  \              letr\
  \                { c = y | t } =\
  \                  s\
  \                in\
  \                  y\
  \"

-- "

program24 :: Text
program24 =
  "def main(_ : unit) : int =\
  \  let\
  \    r =\
  \      { a = 5 }\
  \    in\
  \      letr\
  \        { a = x | s } =\
  \          r\
  \        in\
  \          x\
  \"

-- "

program25 :: Text
program25 =
  "def foo(a : unit) : int =\
  \  5\
  \\r\n\
  \def main(_ : unit) : int =\
  \  let\
  \    r =\
  \      { a = false }\
  \    in\
  \      letr\
  \        { a = x | s } =\
  \          r\
  \        in\
  \          if x\
  \            then 9\
  \            else 10\
  \"

-- "

program26 :: Text
program26 =
  "def main(_ : unit) : int =\
  \  let\
  \    r =\
  \      { a = 3.14159 }\
  \    in\
  \      letr\
  \        { a = x | s } =\
  \          r\
  \        in\
  \          if x > 3.0\
  \            then 99\
  \            else 109\
  \"

-- "

program27 :: Text
program27 =
  "def main(_ : unit) : int =\
  \  let\
  \    b =\
  \      5\
  \    in\
  \      let\
  \        a =\
  \          true\
  \        in\
  \          if not a\
  \            then 1\
  \            else 2\
  \"

-- "

program28 :: Text
program28 =
  "def main(_ : unit) : int =\
  \  let\
  \    b =\
  \      5\
  \    in\
  \      let\
  \        a =\
  \          -b\
  \        in\
  \          if -5 == a\
  \            then 1\
  \            else 2\
  \"

-- "

program29 :: Text
program29 =
  "def main(_ : unit) : int =\
  \  let\
  \    b =\
  \      5\
  \    in\
  \      let\
  \        a =\
  \          b == 5 || b == -5\
  \        in\
  \          if a\
  \            then 1\
  \            else 2\
  \"

-- "

program30 :: Text
program30 =
  "def main(_ : unit) : int =\
  \  let\
  \    b =\
  \      5\
  \    in\
  \      let\
  \        a =\
  \          b == 5 && 3 > 2\
  \        in\
  \          if a\
  \            then 1\
  \            else 2\
  \"

-- "

program31 :: Text
program31 =
  "def main(_ : unit) : int =\
  \  let\
  \    b =\
  \      5\
  \    in\
  \      let\
  \        a =\
  \          b == -5 && 3 > 2\
  \        in\
  \          if a\
  \            then 1\
  \            else 2\
  \"

-- "

program32 :: Text
program32 =
  "def main(_ : unit) : int =\
  \  let\
  \    b =\
  \      5\
  \    in\
  \      let\
  \        a =\
  \          b == -5 || b == 5\
  \        in\
  \          if a\
  \            then 1\
  \            else 2\
  \"

-- "

program33 :: Text
program33 =
  "def main(_ : unit) : int =\
  \  let f =\
  \    lam(r) =>\
  \      { x = 1 | r }\
  \   in\
  \     let\
  \       q =\
  \         f({ y = 2 })\
  \       in\
  \         3\
  \"

--  \         letr\
--  \           { x = a | s } =\
--  \             q\
--  \           in\
--  \             a\

-- "

program34 :: Program () SourceExpr
program34 =
  Program
    ( Map.fromList
        [
          (
            ( Scheme (Fix (TArr (Fix TUnit) (Fix TInt)))
            , "main"
            )
          , Function
              (fromList [((), "_")])
              ( ()
              , eLet
                  ((), "f")
                  ( eLam
                      ()
                      [((), "r")]
                      ( eRec
                          ( rExt
                              "x"
                              (eLit (PInt 1))
                              ( rVar ((), "r")
                              )
                          )
                      )
                  )
                  ( eLet
                      ((), "q")
                      ( eApp
                          ()
                          (eVar ((), "f"))
                          [ eRec
                              ( rExt
                                  "y"
                                  (eLit (PInt 2))
                                  rNil
                              )
                          ]
                      )
                      (eLit (PInt 3))
                  )
              )
          )
        ]
    )

--  let f =
--    lam(r) =>
--      { x = 1 | r }
--   in
--     let
--       q =
--         f({ y = 2 })
--       in
--         3

expr35 :: TypedExpr
expr35 =
  eLam
    ()
    [(tVar 0, "r")]
    ( eRec
        ( rExt
            "x"
            (eLit (PInt 1))
            (rVar (tVar 0, "r"))
        )
    )

row36 :: Row TypedExpr (Label MonoType)
row36 =
  rExt
    "x"
    (eLit (PInt 1))
    (rVar (tVar 0, "r"))

expr37 :: TypedExpr
expr37 =
  eLet
    (tArr (tVar 3) (tRec (rExt "x" tInt (rVar 3))), "f")
    ( eLam
        ()
        [(tVar 3, "r")]
        (eRec (rExt "x" (eLit (PInt 1)) (rVar (tVar 3, "r"))))
    )
    (eLit (PInt 3))

sub38 :: Substitution
sub38 =
  Substitution
    ( Map.fromList
        [
          ( 3
          , tVar 0
          )
        ]
    )

program341 :: Program () SourceExpr
program341 =
  Program
    ( Map.fromList
        [
          (
            ( Scheme (Fix (TArr (Fix TUnit) (Fix TInt)))
            , "main"
            )
          , Function
              (fromList [((), "_")])
              ( ()
              , eLet
                  ((), "f")
                  ( eLam
                      ()
                      [((), "r")]
                      ( eRec
                          ( rExt
                              "x"
                              (eLit (PInt 1))
                              rNil
                          )
                      )
                  )
                  (eLit (PInt 3))
              )
          )
        ]
    )

program3412 :: Program MonoType TypedExpr
program3412 =
  Program
    ( Map.fromList
        [
          (
            ( Scheme (Fix (TArr (Fix TUnit) (Fix TInt)))
            , "main"
            )
          , Function
              (fromList [(tUnit, "_")])
              ( tInt
              , eLet
                  (tVar 0 ~> tRec (rExt "x" tInt rNil), "f")
                  ( eLam
                      ()
                      [(tVar 0, "r")]
                      ( eRec
                          ( rExt
                              "x"
                              (eLit (PInt 1))
                              rNil
                          )
                      )
                  )
                  (eLit (PInt 3))
              )
          )
        ]
    )

program342 :: Program () SourceExpr
program342 =
  Program
    ( Map.fromList
        [
          (
            ( Scheme (Fix (TArr (Fix TUnit) (Fix TInt)))
            , "main"
            )
          , Function
              (fromList [((), "_")])
              ( ()
              , eLet
                  ((), "f")
                  ( eLam
                      ()
                      [((), "r")]
                      ( eRec
                          ( rExt
                              "x"
                              (eLit (PInt 1))
                              (rVar ((), "r"))
                          )
                      )
                  )
                  (eLit (PInt 3))
              )
          )
        ]
    )

program3422 :: Program MonoType TypedExpr
program3422 =
  Program
    ( Map.fromList
        [
          (
            ( Scheme (Fix (TArr (Fix TUnit) (Fix TInt)))
            , "main"
            )
          , Function
              (fromList [(tUnit, "_")])
              ( tInt
              , eLet
                  (tVar 0 ~> tRec (rExt "x" tInt (rVar 0)), "f")
                  ( eLam
                      ()
                      [(tVar 0, "r")]
                      ( eRec
                          ( rExt
                              "x"
                              (eLit (PInt 1))
                              (rVar (tVar 0, "r"))
                          )
                      )
                  )
                  (eLit (PInt 3))
              )
          )
        ]
    )

-- program343 :: Program () SourceExpr
-- program343 =
--  Program
--    ( Map.fromList
--        [
--          (
--            ( Scheme (Fix (TArr (Fix TUnit) (Fix TInt)))
--            , "main"
--            )
--          , Function
--              (fromList [((), "_")])
--              ( ()
--              , eLet
--                  ((), "f")
--                  ( eRec
--                      ( rExt
--                          "x"
--                          (eLit (PInt 1))
--                          (rVar ((), "r"))
--                      )
--                  )
--                  (eLit (PInt 3))
--              )
--          )
--        ]
--    )
--
-- program3432 :: Program MonoType TypedExpr
-- program3432 =
--  Program
--    ( Map.fromList
--        [
--          (
--            ( Scheme (Fix (TArr (Fix TUnit) (Fix TInt)))
--            , "main"
--            )
--          , Function
--              (fromList [(tUnit, "_")])
--              ( tInt
--              , eLet
--                  (tRec (rExt "x" tInt rNil), "f")
--                  ( eRec
--                      ( rExt
--                          "x"
--                          (eLit (PInt 1))
--                          (rVar (tVar 1, "r"))
--                      )
--                  )
--                  (eLit (PInt 3))
--              )
--          )
--        ]
--    )

program35 :: Program MonoType TypedExpr
program35 =
  Program
    ( Map.fromList
        [
          (
            ( Scheme (Fix (TArr (Fix TUnit) (Fix TInt)))
            , "main"
            )
          , Function
              (fromList [(tUnit, "_")])
              ( tInt
              , eLet
                  (tVar 0 ~> tRec (rExt "x" tInt (rVar 0)), "f")
                  ( eLam
                      ()
                      [(tVar 0, "r")]
                      ( eRec
                          ( rExt
                              "x"
                              (eLit (PInt 1))
                              ( rVar (tVar 0, "r")
                              )
                          )
                      )
                  )
                  ( eLet
                      (tRec (rExt "x" tInt (rExt "y" tInt rNil)), "q")
                      ( eApp
                          (tRec (rExt "x" tInt (rExt "y" tInt rNil)))
                          (eVar (tRec (rExt "y" tInt rNil) ~> tRec (rExt "x" tInt (rExt "y" tInt rNil)), "f"))
                          [ eRec
                              ( rExt
                                  "y"
                                  (eLit (PInt 2))
                                  rNil
                              )
                          ]
                      )
                      (eLit (PInt 3))
                  )
              )
          )
        ]
    )

program36 :: Text
program36 =
  "def main(_ : unit) : int =\
  \  let f =\
  \    lam(r) =>\
  \      5\
  \   in\
  \     let\
  \       q =\
  \         f('a')\
  \       in\
  \         3\
  \"
