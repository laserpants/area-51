module Data where

import Taiyaki.Data
import Taiyaki.Data.Cons

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

data TI = TI [Predicate MonoType] MonoType

-- map : [Functor f] => (a -> b) -> f a  -> f b
--
-- let
--   xs =
--     map((x) => plus(x, 1), [1, 2, 3])
--   in
--     xs
--
testExpr2 :: (Functor e2, Functor e3) => Expr TI [Pattern TI] e2 e3 (Binding TI)
testExpr2 =
  eLet
    (TI [] (tList tInt))
    (BPat (TI [] (tList tInt)) (pVar (TI [] (tList tInt)) "xs"))
    ( eApp
        (TI [] (tList tInt))
        (eVar (TI [InClass "Functor" tInt] ((tInt ~> tInt) ~> tList tInt ~> tList tInt)) "map")
        [ eLam
            (TI [] (tInt ~> tInt))
            [ pVar (TI [] tInt) "x"
            ]
            ( eApp
                (TI [] tInt)
                (eVar (TI [] (tInt ~> tInt ~> tInt)) "plus")
                [ eVar (TI [] tInt) "x"
                , eLit (TI [] tInt) (IInt 1)
                ]
            )
        , eList
            (TI [] (tList tInt))
            [ eLit (TI [] tInt) (IInt 1)
            , eLit (TI [] tInt) (IInt 2)
            , eLit (TI [] tInt) (IInt 3)
            ]
        ]
    )
    (eVar (TI [] (tList tInt)) "xs")

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
    (eApp
      ()
      (eVar () "map")
      [ eLam ()
          [ pVar () "x"
          ]
            ( eApp
                ()
                (eVar () "plus")
                [ eVar () "x"
                , eLit () (IInt 1)
                ]
            )
      ])
    (eApp
      ()
      (eVar () "f")
      [ eList
            ()
            [ eLit () (IInt 1)
            , eLit () (IInt 2)
            , eLit () (IInt 3)
            ]
      ])

--
-- let
--   f =
--     map(x => plus(x, 1))
--   in
--     f([1, 2, 3])
--
testExpr4 :: (Functor e2, Functor e3) => Expr TI [Pattern TI] e2 e3 (Binding TI)
testExpr4 =
  eLet
    (TI [] (tList tInt))
    (BPat 
      (TI [] (tList tInt ~> tList tInt)) 
      (pVar (TI [] (tList tInt ~> tList tInt)) "f"))
    (eApp
      (TI [] (tList tInt ~> tList tInt))
      (eVar (TI [] ((tInt ~> tInt) ~> tList tInt ~> tList tInt)) "map")
      [ eLam (TI [] (tInt ~> tInt))
          [ pVar (TI [] tInt) "x"
          ]
            ( eApp
                (TI [] tInt)
                (eVar (TI [] (tInt ~> tInt ~> tInt)) "plus")
                [ eVar (TI [] tInt) "x"
                , eLit (TI [] tInt) (IInt 1)
                ]
            )
      ])
    (eApp
      (TI [] (tList tInt))
      (eVar (TI [] (tList tInt ~> tList tInt)) "f")
      [ eList
            (TI [] (tList tInt))
            [ eLit (TI [] tInt) (IInt 1)
            , eLit (TI [] tInt) (IInt 2)
            , eLit (TI [] tInt) (IInt 3)
            ]
      ])
