module Data where

import Taiyaki.Data
import Taiyaki.Data.Cons
import Taiyaki.Lang

--
-- let
--   xs =
--     map((x) => plus(x, 1), [1, 2, 3])
--   in
--     xs
--
testExpr1 :: ProgExpr ()
testExpr1 =
  eLet
    ()
    (BPat () (pVar () "xs"))
    ( eApp
        ()
        (eVar () "map")
        [ eLam
            ()
            [ pVar () "x"
            ]
            ( eApp
                ()
                (eVar () "plus")
                [ eVar () "x"
                , eLit () (IInt 1)
                ]
            )
        , eList
            ()
            [ eLit () (IInt 1)
            , eLit () (IInt 2)
            , eLit () (IInt 3)
            ]
        ]
    )
    (eVar () "xs")

data Qualified t = Qty [Predicate t] t

-- map : [Functor f] => (a -> b) -> f a  -> f b
--
-- let
--   xs =
--     map((x) => plus(x, 1), [1, 2, 3])
--   in
--     xs
--
testExpr2 :: (Functor e2, Functor e3) => Expr (Qualified MonoType) [Pattern (Qualified MonoType)] e2 e3 (Binding (Qualified MonoType))
testExpr2 =
  eLet
    (Qty [] (tListApp tInt))
    (BPat (Qty [] (tListApp tInt)) (pVar (Qty [] (tListApp tInt)) "xs"))
    ( eApp
        (Qty [] (tListApp tInt))
        (eVar (Qty [InClass "Functor" tList] ((tInt ~> tInt) ~> tListApp tInt ~> tListApp tInt)) "map")
        [ eLam
            (Qty [] (tInt ~> tInt))
            [ pVar (Qty [] tInt) "x"
            ]
            ( eApp
                (Qty [] tInt)
                (eVar (Qty [] (tInt ~> tInt ~> tInt)) "plus")
                [ eVar (Qty [] tInt) "x"
                , eLit (Qty [] tInt) (IInt 1)
                ]
            )
        , eList
            (Qty [] (tListApp tInt))
            [ eLit (Qty [] tInt) (IInt 1)
            , eLit (Qty [] tInt) (IInt 2)
            , eLit (Qty [] tInt) (IInt 3)
            ]
        ]
    )
    (eVar (Qty [] (tListApp tInt)) "xs")

--
-- let
--   f =
--     map(x => plus(x, 1))
--   in
--     f([1, 2, 3])
--
testExpr3 :: ProgExpr ()
testExpr3 =
  eLet
    ()
    (BPat () (pVar () "f"))
    ( eApp
        ()
        (eVar () "map")
        [ eLam
            ()
            [ pVar () "x"
            ]
            ( eApp
                ()
                (eVar () "plus")
                [ eVar () "x"
                , eLit () (IInt 1)
                ]
            )
        ]
    )
    ( eApp
        ()
        (eVar () "f")
        [ eList
            ()
            [ eLit () (IInt 1)
            , eLit () (IInt 2)
            , eLit () (IInt 3)
            ]
        ]
    )

--
-- let
--   f =
--     map(x => plus(x, 1))
--   in
--     f([1, 2, 3])
--
testExpr4 :: (Functor e2, Functor e3) => Expr (Qualified MonoType) [Pattern (Qualified MonoType)] e2 e3 (Binding (Qualified MonoType))
testExpr4 =
  eLet
    (Qty [] (tListApp tInt))
    ( BPat
        (Qty [] (tListApp tInt ~> tListApp tInt))
        (pVar (Qty [] (tListApp tInt ~> tListApp tInt)) "f")
    )
    ( eApp
        (Qty [] (tListApp tInt ~> tListApp tInt))
        (eVar (Qty [InClass "Functor" tList] ((tInt ~> tInt) ~> tListApp tInt ~> tListApp tInt)) "map")
        [ eLam
            (Qty [] (tInt ~> tInt))
            [ pVar (Qty [] tInt) "x"
            ]
            ( eApp
                (Qty [] tInt)
                (eVar (Qty [] (tInt ~> tInt ~> tInt)) "plus")
                [ eVar (Qty [] tInt) "x"
                , eLit (Qty [] tInt) (IInt 1)
                ]
            )
        ]
    )
    ( eApp
        (Qty [] (tListApp tInt))
        (eVar (Qty [] (tListApp tInt ~> tListApp tInt)) "f")
        [ eList
            (Qty [] (tListApp tInt))
            [ eLit (Qty [] tInt) (IInt 1)
            , eLit (Qty [] tInt) (IInt 2)
            , eLit (Qty [] tInt) (IInt 3)
            ]
        ]
    )

{-

map : [Functor f] => (a -> b) -> f a  -> f b

plus1 : int -> int
plus1 x = x + 1

g = map

z =
  let
    f =
      map
    in
      f

h = map(plus1)

foo =
  let
    f =
      map
   in
     ...

baz =
  let
    f =
      map(plus1)
    in
      ...

bar =
  let
    y =
      map(plus1, [1, 2, 3])
    in
      y + 1

-}

--
-- let
--   f =
--     map
--   in
--     let
--       g =
--         f((x) => x + 1)
--       in
--         g([1, 2, 3])
--
testExpr5 :: ProgExpr ()
testExpr5 =
  eLet
    ()
    (BPat () (pVar () "f"))
    (eVar () "map")
    ( eLet
        ()
        (BPat () (pVar () "g"))
        ( eApp
            ()
            (eVar () "f")
            [ eLam
                ()
                [ pVar () "x"
                ]
                ( eApp
                    ()
                    (eVar () "plus")
                    [ eVar () "x"
                    , eLit () (IInt 1)
                    ]
                )
            ]
        )
        ( eApp
            ()
            (eVar () "g")
            [ eList
                ()
                [ eLit () (IInt 1)
                , eLit () (IInt 2)
                , eLit () (IInt 3)
                ]
            ]
        )
    )

--
-- let
--   f =
--     map
--   in
--     let
--       g =
--         f((x) => x + 1)
--       in
--         g([1, 2, 3])
--
testExpr6 :: (Functor e2, Functor e3) => Expr (Qualified MonoType) [Pattern (Qualified MonoType)] e2 e3 (Binding (Qualified MonoType))
testExpr6 =
  eLet
    undefined
    (BPat undefined (pVar undefined "f"))
    (eVar undefined "map")
    ( eLet
        undefined
        (BPat undefined (pVar undefined "g"))
        ( eApp
            undefined
            (eVar undefined "f")
            [ eLam
                undefined
                [ pVar undefined "x"
                ]
                ( eApp
                    undefined
                    (eVar undefined "plus")
                    [ eVar undefined "x"
                    , eLit undefined (IInt 1)
                    ]
                )
            ]
        )
        ( eApp
            undefined
            (eVar undefined "g")
            [ eList
                undefined
                [ eLit undefined (IInt 1)
                , eLit undefined (IInt 2)
                , eLit undefined (IInt 3)
                ]
            ]
        )
    )

-- show : [Show a] => a -> string
--
-- let
--   s =
--     show
--   in
--     s(5)
--
testExpr7 :: (Functor e2, Functor e3) => Expr (Qualified MonoType) [Pattern (Qualified MonoType)] e2 e3 (Binding (Qualified MonoType))
testExpr7 =
  eLet
    (Qty [] tString)
    (BPat (Qty [InClass "Show" tInt] (tInt ~> tString)) (pVar (Qty [InClass "Show" tInt] (tInt ~> tString)) "s"))
    (eVar (Qty [InClass "Show" tInt] (tInt ~> tString)) "show")
    ( eApp
        (Qty [] tString)
        (eVar (Qty [InClass "Show" tInt] (tInt ~> tString)) "s")
        [ eLit (Qty [] tInt) (IInt 5)
        ]
    )

-- let
--   g =
--     lam(x) =>
--       print((x, length(x)))
--   in
--     g([1, 2, 3])
--
testExpr8 :: (Functor e2, Functor e3) => Expr (Qualified MonoType) [Pattern (Qualified MonoType)] e2 e3 (Binding (Qualified MonoType))
testExpr8 =
  eLet
    undefined
    (BPat undefined (pVar undefined "g"))
    ( eLam
        undefined
        [ pVar undefined "x"
        ]
        ( eApp
            undefined
            (eVar (Qty [InClass "Show" (tup () [tListApp tInt, tInt])] (tup () [tListApp tInt, tInt] ~> tString)) "print")
            [ eTup
                undefined
                [ eVar undefined "x"
                , eApp
                    undefined
                    (eVar (Qty [] (tListApp tInt ~> tInt)) "length")
                    [ eVar (Qty [] (tListApp tInt)) "x"
                    ]
                ]
            ]
        )
    )
    ( eApp
        undefined
        (eVar undefined "g")
        [ eList
            undefined
            [ eLit undefined (IInt 1)
            , eLit undefined (IInt 2)
            , eLit undefined (IInt 3)
            ]
        ]
    )

-- let
--   g =
--     lam(x) =>
--       print((x, length(x)))
--   in
--     g
--
testExpr9 :: (Functor e2, Functor e3) => Expr (Qualified MonoType) [Pattern (Qualified MonoType)] e2 e3 (Binding (Qualified MonoType))
testExpr9 =
  eLet
    undefined
    (BPat undefined (pVar undefined "g"))
    ( eLam
        undefined
        [ pVar (Qty [] (tListApp (tVar kTyp (MonoIndex 0)))) "x"
        ]
        ( eApp
            (Qty [] tString)
            (eVar (Qty [InClass "Show" (tup () [tListApp (tVar kTyp (MonoIndex 0)), tVar kTyp (MonoIndex 0)])] (tup () [tListApp (tVar kTyp (MonoIndex 0)), tVar kTyp (MonoIndex 0)] ~> tString)) "print")
            [ eTup
                undefined
                [ eVar (Qty [] (tListApp (tVar kTyp (MonoIndex 0)))) "x"
                , eApp
                    undefined
                    (eVar (Qty [] (tListApp (tVar kTyp (MonoIndex 0)) ~> tInt)) "length")
                    [ eVar (Qty [] (tListApp (tVar kTyp (MonoIndex 0)))) "x"
                    ]
                ]
            ]
        )
    )
    (eVar undefined "g")

-- let
--   g =
--     lam(x) =>
--       print((x, x))
--   in
--     (g(5), g("foo"))
--
testExpr10 :: (Functor e2, Functor e3) => Expr (Qualified MonoType) [Pattern (Qualified MonoType)] e2 e3 (Binding (Qualified MonoType))
testExpr10 =
  eLet
    undefined
    (BPat undefined (pVar undefined "g"))
    ( eLam
        undefined
        [ pVar undefined "x"
        ]
        ( eApp
            undefined
            (eVar undefined "print")
            [ eTup
                undefined
                [ eVar undefined "x"
                , eVar undefined "x"
                ]
            ]
        )
    )
    ( eTup
        undefined
        [ eApp
            undefined
            (eVar undefined "g")
            [ eLit undefined (IInt 5)
            ]
        , eApp
            undefined
            (eVar undefined "g")
            [ eLit undefined (IString "foo")
            ]
        ]
    )

--     (g(5), g("foo"))
--
    -- (eVar (Qty [InClass "Show" (tVar kTyp (MonoIndex 0))] (tVar kTyp (MonoIndex 0) ~> tString)) "print")
