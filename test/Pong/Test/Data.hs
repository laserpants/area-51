{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Pong.Test.Data where

import Control.Monad.Fix
import Control.Monad.Reader
import Data.List.NonEmpty (fromList, toList)
import qualified Data.Map.Strict as Map
import Data.Void
import Pong.Data
import Pong.Eval
import Pong.Lang
import Pong.Util
import qualified Pong.Util.Env as Env

-- (x) = plus(x)
fragment1_0 :: Definition (Label Type) (Expr Type Type () Void)
fragment1_0 =
  Function
    (fromList [(tInt, "x")])
    ( tInt ~> tInt
    , eApp
        (tInt ~> tInt)
        (eVar (tInt ~> tInt ~> tInt, "plus"))
        [eVar (tInt, "x")])

-- (x, v0) = plus(x, v0)
fragment1_1 :: Definition (Label Type) (Expr Type Type () Void)
fragment1_1 =
  Function
    (fromList [(tInt, "x"), (tInt, ".v0")])
    ( tInt
    , eApp
        tInt
        (eVar (tInt ~> tInt ~> tInt, "plus"))
        [eVar (tInt, "x"), eVar (tInt, ".v0")])

fragment2_0 :: Definition (Label Type) (Expr Type () () Void)
fragment2_0 =
  Function
    (fromList [(tInt, "x")])
    ( tInt ~> tInt ~> tInt
    , eLam () [(tInt, "a")] (eLam () [(tInt, "b")] (eVar (tInt, "b"))))

fragment2_1 :: Definition (Label Type) (Expr Type () () Void)
fragment2_1 =
  Function
    (fromList [(tInt, "x"), (tInt, "a"), (tInt, "b")])
    (tInt, eVar (tInt, "b"))

fragment3_0 :: Expr Type Type () Void
fragment3_0 =
  eApp
    tInt
    (eApp
       (tInt ~> tInt)
       (eVar (tInt ~> tInt ~> tInt, "g"))
       [eVar (tInt, "x")])
    [eVar (tInt, "y")]

fragment3_1 :: Expr Type Type () Void
fragment3_1 =
  eApp
    tInt
    (eVar (tInt ~> tInt ~> tInt, "g"))
    [eVar (tInt, "x"), eVar (tInt, "y")]

--fragment4_0 :: Expr Type () () Void
fragment4_0 = eLam () [(tInt, "a")] (eLam () [(tInt, "b")] (eVar (tInt, "b")))

--fragment4_1 :: Expr Type () () Void
fragment4_1 = eLam () [(tInt, "a"), (tInt, "b")] (eVar (tInt, "b"))

fragment5_0 :: Expr Type () () Void
fragment5_0 =
  eLet
    (tInt, "h")
    (eOp2 oAddInt (eVar (tInt, "z")) (eLit (PInt 1)))
    (eLet
       (tVar 0 ~> tVar 0, "g")
       (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
       (eLet
          (tInt ~> tInt, "f")
          (eLam ()
             [(tInt, "y")]
             (eOp2 oAddInt (eVar (tInt, "y")) (eVar (tInt, "h"))))
          (eOp2
             oAddInt
             (eApp
                ()
                (eApp
                   ()
                   (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "g"))
                   [eVar (tInt ~> tInt, "f")])
                [eApp () (eVar (tInt ~> tInt, "g")) [eLit (PInt 5)]])
             (eApp () (eVar (tInt ~> tInt, "f")) [eLit (PInt 1)]))))

fragment5_1 :: Expr Type () () Void
fragment5_1 =
  eLet
    (tInt, "h")
    (eOp2 oAddInt (eVar (tInt, "z")) (eLit (PInt 1)))
    (eLet
       (tVar 0 ~> tVar 0, "g")
       (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
       (eLet
          (tInt ~> tInt, "f")
          (eApp
             ()
             (eLam ()
                [(tInt, "h"), (tInt, "y")]
                (eOp2 oAddInt (eVar (tInt, "y")) (eVar (tInt, "h"))))
             [eVar (tInt, "h")])
          (eOp2
             oAddInt
             (eApp
                ()
                (eApp
                   ()
                   (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "g"))
                   [eVar (tInt ~> tInt, "f")])
                [eApp () (eVar (tInt ~> tInt, "g")) [eLit (PInt 5)]])
             (eApp () (eVar (tInt ~> tInt, "f")) [eLit (PInt 1)]))))

fragment5_2 :: TypedExpr
fragment5_2 =
  eLet
    (tInt, "h")
    (eOp2 oAddInt (eVar (tInt, "z")) (eLit (PInt 1)))
    (eLet
       (tVar 0 ~> tVar 0, "g")
       (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
       (eLet
          (tInt ~> tInt, "f")
          (eLam ()
             [(tInt, "y")]
             (eOp2 oAddInt (eVar (tInt, "y")) (eVar (tInt, "h"))))
          (eOp2
             oAddInt
             (eApp
                tInt
                (eApp
                   (tInt ~> tInt)
                   (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "g"))
                   [eVar (tInt ~> tInt, "f")])
                [eApp tInt (eVar (tInt ~> tInt, "g")) [eLit (PInt 5)]])
             (eApp tInt (eVar (tInt ~> tInt, "f")) [eLit (PInt 1)]))))

fragment5_3 :: TypedExpr
fragment5_3 =
  eLet
    (tInt, "h")
    (eOp2 oAddInt (eVar (tInt, "z")) (eLit (PInt 1)))
    (eLet
       (tVar 0 ~> tVar 0, "g")
       (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
       (eLet
          (tInt ~> tInt, "f")
          (eApp
             (tInt ~> tInt)
             (eLam ()
                [(tInt, "h"), (tInt, "y")]
                (eOp2 oAddInt (eVar (tInt, "y")) (eVar (tInt, "h"))))
             [eVar (tInt, "h")])
          (eOp2
             oAddInt
             (eApp
                tInt
                (eApp
                   (tInt ~> tInt)
                   (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "g"))
                   [eVar (tInt ~> tInt, "f")])
                [eApp tInt (eVar (tInt ~> tInt, "g")) [eLit (PInt 5)]])
             (eApp tInt (eVar (tInt ~> tInt, "f")) [eLit (PInt 1)]))))

fragment6_0 :: PreAst
fragment6_0 = eApp tInt (eVar (tInt ~> tInt, "f")) [eVar (tInt, "x")]

fragment6_1 :: Ast
fragment6_1 = eCall (tInt ~> tInt, "f") [eVar (tInt, "x")]

fragment7_0 :: Expr Type () () Void
fragment7_0 =
  eLam ()
    [(tInt, "x")]
    (eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "h")))

fragment7_1 :: Expr Type () () Void
fragment7_1 =
  eApp
    ()
    (eLam ()
       [(tInt, "h"), (tInt, "x")]
       (eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "h"))))
    [eVar (tInt, "h")]

-- let
--   h =
--     z + 1
--   in
--     let 
--       g =
--         lam(x) => 
--           x
--       let
--         f =
--           lam(y) =>
--             y + h
--         in
--           g(f)(g(5)) + f(1)
--       
fragment8_0 :: Expr Type Type () Void
fragment8_0 =
  eLet
    (tInt, "h")
    (eOp2 oAddInt (eVar (tInt, "z")) (eLit (PInt 1)))
    (eLet
       (tVar 0 ~> tVar 0, "g")
       (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
       (eLet
          (tInt ~> tInt, "f")
          (eLam ()
             [(tInt, "y")]
             (eOp2 oAddInt (eVar (tInt, "y")) (eVar (tInt, "h"))))
          (eOp2
             oAddInt
             (eApp
                tInt
                (eApp
                   (tInt ~> tInt)
                   (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "g"))
                   [eVar (tInt ~> tInt, "f")])
                [eApp tInt (eVar (tInt ~> tInt, "g")) [eLit (PInt 5)]])
             (eApp tInt (eVar (tInt ~> tInt, "f")) [eLit (PInt 1)]))))

fragment8_1 :: (PreAst, [(Name, Definition (Label Type) PreAst)])
fragment8_1 =
  ( eLet
      (tInt, "h")
      (eOp2 oAddInt (eVar (tInt, "z")) (eLit (PInt 1)))
      (eOp2
         oAddInt
         (eApp
            tInt
            (eApp
               (tInt ~> tInt)
               (eVar ((tInt ~> tInt) ~> tInt ~> tInt, ".f0"))
               [eVar (tInt ~> tInt, ".f1")])
            [eApp tInt (eVar (tInt ~> tInt, ".f0")) [eLit (PInt 5)]])
         (eApp tInt (eVar (tInt ~> tInt, ".f1")) [eLit (PInt 1)]))
  , [ (".f0", Function (fromList [(tVar 0, "x")]) (tVar 0, eVar (tVar 0, "x")))
    , ( ".f1"
      , Function
          (fromList [(tInt, "y")])
          (tInt, eOp2 oAddInt (eVar (tInt, "y")) (eVar (tInt, "h"))))
    ])

fragment9_0 :: Expr Type () () Void
fragment9_0 =
  eLet
    (tInt, "x")
    (eVar (tInt, "y"))
    (eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "z")))

fragment9_1 :: Expr Type () () Void
fragment9_1 = eOp2 oAddInt (eVar (tInt, "y")) (eVar (tInt, "z"))

fragment10_0 :: Definition (Label Type) (Expr Type Type () Void)
fragment10_0 =
  Constant
    ( tInt
    , eLet
        (tInt ~> tInt ~> tInt ~> tInt, "add3")
        (eLam ()
           [(tInt, "x"), (tInt, "y"), (tInt, "z")]
           (eOp2
              oAddInt
              (eVar (tInt, "x"))
              (eOp2 oAddInt (eVar (tInt, "y")) (eVar (tInt, "z")))))
        (eLet
           (tInt ~> tInt ~> tInt ~> tInt, "f")
           (eVar (tInt ~> tInt ~> tInt ~> tInt, "add3"))
           (eLet
              (tInt ~> tInt ~> tInt, "g")
              (eApp
                 (tInt ~> tInt ~> tInt)
                 (eVar (tInt ~> tInt ~> tInt ~> tInt, "f"))
                 [eLit (PInt 5)])
              (eLet
                 (tInt ~> tInt, "h")
                 (eApp
                    (tInt ~> tInt)
                    (eVar (tInt ~> tInt ~> tInt, "g"))
                    [eLit (PInt 6)])
                 (eApp tInt (eVar (tInt ~> tInt, "h")) [eLit (PInt 7)])))))

fragment10_1 :: Definition (Label Type) (Expr Type Type () Void)
fragment10_1 =
  Constant
    ( tInt
    , eLet
        (tInt ~> tInt ~> tInt ~> tInt, "add3")
        (eLam ()
           [(tInt, "x"), (tInt, "y"), (tInt, "z")]
           (eOp2
              oAddInt
              (eVar (tInt, "x"))
              (eOp2 oAddInt (eVar (tInt, "y")) (eVar (tInt, "z")))))
        (eLet
           (tInt ~> tInt ~> tInt, "g")
           (eLam ()
              [(tInt, ".v0"), (tInt, ".v1")]
              (eApp
                 tInt
                 (eVar (tInt ~> tInt ~> tInt ~> tInt, "add3"))
                 [eLit (PInt 5), eVar (tInt, ".v0"), eVar (tInt, ".v1")]))
           (eLet
              (tInt ~> tInt, "h")
              (eLam ()
                 [(tInt, ".v0")]
                 (eApp
                    tInt
                    (eVar (tInt ~> tInt ~> tInt, "g"))
                    [eLit (PInt 6), eVar (tInt, ".v0")]))
              (eApp tInt (eVar (tInt ~> tInt, "h")) [eLit (PInt 7)]))))

fragment11_0 :: Expr Type () () Void
fragment11_0 =
  eLet
    (tInt ~> tInt ~> tInt ~> tInt, "add3")
    (eLam ()
       [(tInt, "x"), (tInt, "y"), (tInt, "z")]
       (eOp2
          oAddInt
          (eVar (tInt, "x"))
          (eOp2 oAddInt (eVar (tInt, "y")) (eVar (tInt, "z")))))
    (eLet
       (tInt ~> tInt ~> tInt ~> tInt, "f")
       (eVar (tInt ~> tInt ~> tInt ~> tInt, "add3"))
       (eLet
          (tInt ~> tInt ~> tInt, "g")
          (eApp
             ()
             (eVar (tInt ~> tInt ~> tInt ~> tInt, "f"))
             [eLit (PInt 5)])
          (eLet
             (tInt ~> tInt, "h")
             (eApp () (eVar (tInt ~> tInt ~> tInt, "g")) [eLit (PInt 6)])
             (eApp () (eVar (tInt ~> tInt, "h")) [eLit (PInt 7)]))))

fragment11_1 :: Expr Type Type () Void
fragment11_1 =
  eLet
    (tInt ~> tInt ~> tInt ~> tInt, "add3")
    (eLam ()
       [(tInt, "x"), (tInt, "y"), (tInt, "z")]
       (eOp2
          oAddInt
          (eVar (tInt, "x"))
          (eOp2 oAddInt (eVar (tInt, "y")) (eVar (tInt, "z")))))
        --
        -- let
        --   g =
        --     add3(5)
        --   in
        --     let
        --       h =
        --         g(6)
        --       in
        --         h(7)
        --
    (eLet
       (tInt ~> tInt ~> tInt, "g")
       (eApp
          (tInt ~> tInt ~> tInt)
          (eVar (tInt ~> tInt ~> tInt ~> tInt, "add3"))
          [eLit (PInt 5)])
       (eLet
          (tInt ~> tInt, "h")
          (eApp
             (tInt ~> tInt)
             (eVar (tInt ~> tInt ~> tInt, "g"))
             [eLit (PInt 6)])
          (eApp tInt (eVar (tInt ~> tInt, "h")) [eLit (PInt 7)])))

fragment11_2 :: Expr Type Type () Void
fragment11_2 =
  eLet
    (tInt ~> tInt ~> tInt ~> tInt, "add3")
    (eLam ()
       [(tInt, "x"), (tInt, "y"), (tInt, "z")]
       (eOp2
          oAddInt
          (eVar (tInt, "x"))
          (eOp2 oAddInt (eVar (tInt, "y")) (eVar (tInt, "z")))))
        --
        -- let
        --   g =
        --     lam(v0, v1) => add3(5, v0, v1)
        --   in
        --     let
        --       h =
        --         lam(v0) => g(6, v0)
        --       in
        --         h(7)
        --
    (eLet
       (tInt ~> tInt ~> tInt, "g")
       (eLam ()
          [(tInt, ".v0"), (tInt, ".v1")]
          (eApp
             tInt
             (eVar (tInt ~> tInt ~> tInt ~> tInt, "add3"))
             [eLit (PInt 5), eVar (tInt, ".v0"), eVar (tInt, ".v1")]))
       (eLet
          (tInt ~> tInt, "h")
          (eLam ()
             [(tInt, ".v0")]
             (eApp
                tInt
                (eVar (tInt ~> tInt ~> tInt, "g"))
                [eLit (PInt 6), eVar (tInt, ".v0")]))
          (eApp tInt (eVar (tInt ~> tInt, "h")) [eLit (PInt 7)])))

fragment12_0 :: Expr Type Type () Void
fragment12_0 =
  eLet
    (tInt ~> tInt, "g")
    (eApp
       (tInt ~> tInt)
       (eVar (tInt ~> tInt ~> tInt, "f"))
       [eLit (PInt 1)])
    (eApp tInt (eVar (tInt ~> tInt, "g")) [eLit (PInt 2)])

fragment12_1 :: Expr Type Type () Void
fragment12_1 =
  eLet
    (tInt ~> tInt, "g")
    (eLam ()
       [(tInt, ".v0")]
       (eApp
          tInt
          (eVar (tInt ~> tInt ~> tInt, "f"))
          [eLit (PInt 1), eVar (tInt, ".v0")]))
    (eApp tInt (eVar (tInt ~> tInt, "g")) [eLit (PInt 2)])

-- 
-- let
--   z =
--     lam(f) =>
--       lam(g) =>
--         lam(b) => 
--           f(lam(x) => x)(g(b))
--   in
--     z(lam(x) => x, lam(x) => x, 1)
fragment13_0 :: Expr () () () Void
fragment13_0 =
  eLet
    ((), "z")
    (eLam ()
       [((), "f")]
       (eLam ()
          [((), "g")]
          (eLam ()
             [((), "b")]
             (eApp
                ()
                (eApp () (eVar ((), "f")) [eLam () [((), "x")] (eVar ((), "x"))])
                [eApp () (eVar ((), "g")) [eVar ((), "b")]]))))
    (eApp
       ()
       (eVar ((), "z"))
       [ eLam () [((), "x")] (eVar ((), "x"))
       , eLam () [((), "x")] (eVar ((), "x"))
       , eLit (PInt 1)
       ])

fragment13_1 :: Expr Int Int () Void
fragment13_1 =
  eLet
    (1, "z")
    (eLam ()
       [(2, "f")]
       (eLam ()
          [(3, "g")]
          (eLam ()
             [(4, "b")]
             (eApp
                5
                (eApp 6 (eVar (7, "f")) [eLam () [(8, "x")] (eVar (9, "x"))])
                [eApp 10 (eVar (11, "g")) [eVar (12, "b")]]))))
    (eApp
       13
       (eVar (14, "z"))
       [ eLam () [(15, "x")] (eVar (16, "x"))
       , eLam () [(17, "x")] (eVar (18, "x"))
       , eLit (PInt 1)
       ])

fragment15_0 :: Expr Int () () Void
fragment15_0 = eApp () (eVar (1, "f")) [eLit (PInt 1)]

row_0, row_1, row_0_1, row_1_1, row_2, row_3, row_4, row_5, row_6, row_7, row_8, row_9, row_10, row_11, row_12, row_13, row_14, row_15, row_16, row_17, row_20, row_21, row_22, row_23, row_24, row_25, row_26, row_27, row_28, row_29, row_30, row_31, row_32, row_33, row_34, row_35, row_36, row_37, row_38, row_39, row_40, row_41 :: Row Type Int
row_0 = rExt "name" (tVar 0) (rVar 1)
row_1 = rExt "id" tInt (rExt "name" tString rNil)

row_0_1 = rVar 1
row_1_1 = rExt "id" tInt rNil

row_2 = rExt "name" tString (rExt "id" tInt rNil)
row_3 = rExt "id" tString (rExt "name" tInt rNil)

row_4 = rExt "name" tString (rExt "id" tInt rNil)
row_5 = rExt "id" tInt (rExt "name" tString rNil)

row_6 = rExt "name" tString (rVar 0)
row_7 = rExt "name" tString (rVar 0)

row_8 = rExt "a" tString (rVar 0)
row_9 = rExt "b" tString (rVar 0)

row_10 = rExt "name" tString (rVar 0)
row_11 = rExt "name" tString (rVar 1)

row_12 = rExt "id" tInt (rExt "pw" tString (rExt "name" tString rNil))
row_13 = rExt "id" tInt (rVar 0)

row_14 = rExt "pw" tString (rExt "name" tString (rVar 0))
row_15 = rVar 0

row_16 = rExt "id" tInt (rExt "pw" tString (rExt "name" tString (rVar 0)))
row_17 = rExt "id" tInt (rVar 0)

row_20 = rExt "name" tString (rExt "id" tInt (rExt "shoeSize" tFloat rNil))
row_21 = rExt "shoeSize" tFloat (rExt "id" tInt (rExt "name" tString rNil))

row_22 = rExt "name" tString (rExt "shoeSize" tFloat rNil)
row_23 = rExt "shoeSize" tFloat (rVar 0)

row_24 = rExt "name" tString (rExt "id" tInt (rExt "shoeSize" tFloat rNil))
row_25 = rExt "shoeSize" tFloat (rExt "id" tInt (rVar 0))

row_26 = rExt "name" tString (rExt "id" tInt (rExt "shoeSize" tFloat rNil))
row_27 = rVar 0

row_28 = rExt "shoeSize" tFloat (rExt "name" tString (rExt "id" tInt rNil))
row_29 = rExt "shoeSize" tFloat (rExt "id" tInt (rVar 0))

row_30 = rExt "shoeSize" tBool (rExt "name" tString (rExt "id" tInt rNil))
row_31 = rExt "shoeSize" tFloat (rExt "id" tInt (rVar 0))

row_32 = rVar 0
row_33 = rVar 1

row_34 = rExt "a" tInt (rVar 0)
row_35 = rExt "a" tInt (rVar 0)

row_36 = rExt "a" tInt (rVar 0)
row_37 = rExt "a" tInt (rVar 1)

row_38 = rExt "a" tInt rNil
row_39 = rExt "a" tInt (rVar 1)

row_40 = rExt "a" tInt rNil
row_41 = rExt "a" tInt rNil

type_0, type_1, type_2, type_3 :: Type
type_0 = tRow (rExt "name" (tVar 0) (rVar 1))
type_1 = tRow (rExt "id" tInt (rExt "name" tString rNil))
type_2 = tRow (rExt "name" (tVar 0) (rVar 1)) ~> tVar 2
type_3 = tRow (rExt "id" tInt (rExt "name" tString rNil)) ~> tInt

--fragment13_2 :: Expr Type () () Void
--fragment13_2 =
--  eLet
--    (undefined, "z")
--    (eLam
--       [(undefined, "f")]
--       (eLam
--          [(undefined, "g")]
--          (eLam
--             [(undefined, "b")]
--             (eApp
--                undefined
--                (eApp
--                   undefined
--                   (eVar (undefined, "f"))
--                   [eLam [(undefined, "x")] (eVar (undefined, "x"))])
--                [eApp undefined (eVar (undefined, "g")) [eVar (undefined, "b")]]))))
--    (eApp
--       undefined
--       (eVar (undefined, "z"))
--       [ eLam [(undefined, "x")] (eVar (undefined, "x"))
--       , eLam [(undefined, "x")] (eVar (undefined, "x"))
--       , eLit (PInt 1)
--       ])
fragment14_0 :: Type
fragment14_0 = tCon "Cons" [tVar 0, tVar 1]

fragment14_1 :: Type
fragment14_1 = tCon "Cons" [tInt, tCon "Cons" [tInt, tCon "Nil" []]]

fragment15_1 :: Expr Type () () Void
fragment15_1 =
  eLet
    (tInt ~> tInt ~> tInt ~> tInt, "add3")
    (eLam ()
       [(tInt, "x"), (tInt, "y"), (tInt, "z")]
       (eOp2
          oAddInt
          (eVar (tInt, "x"))
          (eOp2 oAddInt (eVar (tInt, "y")) (eVar (tInt, "z")))))
        --
        -- let
        --   g =
        --     add3(5)
        --   in
        --     let
        --       h =
        --         g(6)
        --       in
        --         h(7)
        --
    (eLet
       (tInt ~> tInt ~> tInt, "g")
       (eApp
          ()
          (eVar (tInt ~> tInt ~> tInt ~> tInt, "add3"))
          [eLit (PInt 5)])
       (eLet
          (tInt ~> tInt, "h")
          (eApp () (eVar (tInt ~> tInt ~> tInt, "g")) [eLit (PInt 6)])
          (eApp () (eVar (tInt ~> tInt, "h")) [eLit (PInt 7)])))

fragment16_1 :: Expr Int Int () Void
fragment16_1 =
  eLet
    (1, "id")
    (eLam () [(2, "x")] (eVar (3, "x")))
    (eApp 4 (eApp 5 (eVar (6, "id")) [eVar (7, "id")]) [eLit (PInt 1)])

fragment16_2 :: Expr Type Type () Void
fragment16_2 =
  eLet
    (tVar 2 ~> tVar 2, "id")
    (eLam () [(tVar 2, "x")] (eVar (tVar 2, "x")))
    (eApp
       tInt
       (eApp
          (tInt ~> tInt)
          (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id"))
          [eVar (tInt ~> tInt, "id")])
       [eLit (PInt 1)])

--
-- let
--   id =
--     lam(x) => 
--       x
--   in
--     let
--       f =
--         lam(x) =>
--           lam(y) => 
--             x + y
--       in
--         let
--           g =
--             f(2)
--           in
--             id(g)(id(3)) + f(4, 5)
--
fragment17_1 :: Expr () () () Void
fragment17_1 =
  eLet
    ((), "id")
    (eLam () [((), "x")] (eVar ((), "x")))
    (eLet
       ((), "f")
       (eLam ()
          [((), "x")]
          (eLam () [((), "y")] (eOp2 (Op2 OAdd ()) (eVar ((), "x")) (eVar ((), "y")))))
       (eLet
          ((), "g")
          (eApp () (eVar ((), "f")) [eLit (PInt 2)])
          (eOp2
             (Op2 OAdd ())
             (eApp
                ()
                (eApp () (eVar ((), "id")) [eVar ((), "g")])
                [eApp () (eVar ((), "id")) [eLit (PInt 3)]])
             (eApp () (eVar ((), "f")) [eLit (PInt 4), eLit (PInt 5)]))))

--fragment17_2 :: Expr Type Type () Void
fragment17_2 =
  eLet
    (tVar 2 ~> tVar 2, "id")
    (eLam () [(tVar 2, "x")] (eVar (tVar 2, "x")))
    (eLet
       (tVar 22 ~> tVar 22 ~> tVar 22, "f")
       (eLam ()
          [(tVar 22, "x")]
          (eLam ()
             [(tVar 22, "y")]
             (eOp2 (Op2 OAdd (tVar 22 ~> tVar 22 ~> tVar 22)) (eVar (tVar 22, "x")) (eVar (tVar 22, "y")))))
       (eLet
          (tInt ~> tInt, "g")
          (eApp
             (tInt ~> tInt)
             (eVar (tInt ~> tInt ~> tInt, "f"))
             [eLit (PInt 2)])
          (eOp2
             oAddInt
             (eApp
                tInt
                (eApp
                   (tInt ~> tInt)
                   (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id"))
                   [eVar (tInt ~> tInt, "g")])
                [eApp tInt (eVar (tInt ~> tInt, "id")) [eLit (PInt 3)]])
             (eApp
                tInt
                (eVar (tInt ~> tInt ~> tInt, "f"))
                [eLit (PInt 4), eLit (PInt 5)]))))

--
-- let
--   id =
--     lam(x) => 
--       x
--   in
--     let
--       f =
--         lam[x, y] => 
--           x + y
--       in
--         let
--           g =
--             f(2)
--           in
--             id(g)(id(3)) + f(4, 5)
--
--fragment17_3 :: Expr Type Type () Void
fragment17_3 =
  eLet
    (tVar 2 ~> tVar 2, "id")
    (eLam () [(tVar 2, "x")] (eVar (tVar 2, "x")))
    (eLet
       (tVar 22 ~> tVar 22 ~> tVar 22, "f")
       (eLam ()
          [(tVar 22, "x"), (tVar 22, "y")]
          (eOp2 (Op2 OAdd (tVar 22 ~> tVar 22 ~> tVar 22)) (eVar (tVar 22, "x")) (eVar (tVar 22, "y"))))
       (eLet
          (tInt ~> tInt, "g")
          (eApp
             (tInt ~> tInt)
             (eVar (tInt ~> tInt ~> tInt, "f"))
             [eLit (PInt 2)])
          (eOp2
             oAddInt
             (eApp
                tInt
                (eApp
                   (tInt ~> tInt)
                   (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id"))
                   [eVar (tInt ~> tInt, "g")])
                [eApp tInt (eVar (tInt ~> tInt, "id")) [eLit (PInt 3)]])
             (eApp
                tInt
                (eVar (tInt ~> tInt ~> tInt, "f"))
                [eLit (PInt 4), eLit (PInt 5)]))))

--
-- let
--   id =
--     lam(x) => 
--       x
--   in
--     let
--       f =
--         lam[x, y] => 
--           x + y
--       in
--         let
--           g =
--             (lamv(v0) => f(2, v0))
--           in
--             (lam(v0) => id(g, v0))(id(3)) + f(4, 5)
--
fragment17_4 :: Expr Type Type () Void
fragment17_4 =
  eLet
    (tVar 2 ~> tVar 2, "id")
    (eLam () [(tVar 2, "x")] (eVar (tVar 2, "x")))
    (eLet
       (tVar 22 ~> tVar 22 ~> tVar 22, "f")
       (eLam ()
          [(tVar 22, "x"), (tVar 22, "y")]
          (eOp2 (Op2 OAdd (tVar 22 ~> tVar 22 ~> tVar 22)) (eVar (tVar 22, "x")) (eVar (tVar 22, "y"))))
       (eLet
          (tInt ~> tInt, "g")
          (eLam ()
             [(tInt, ".v0")]
             (eApp
                tInt
                (eVar (tInt ~> tInt ~> tInt, "f"))
                [eLit (PInt 2), eVar (tInt, ".v0")]))
          (eOp2
             oAddInt
             (eApp
                tInt
                (eLam ()
                   [(tInt, ".v0")]
                   (eApp
                      tInt
                      (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id"))
                      [eVar (tInt ~> tInt, "g"), eVar (tInt, ".v0")]))
                [eApp tInt (eVar (tInt ~> tInt, "id")) [eLit (PInt 3)]])
             (eApp
                tInt
                (eVar (tInt ~> tInt ~> tInt, "f"))
                [eLit (PInt 4), eLit (PInt 5)]))))

--
-- .f0(x) = x
-- .f1(x, y) = x + y
-- .f2(.v0) = .f1(2, .v0)
-- .f3(.v0) = .f0(.f2, .v0)
--
-- .f3(.f0(3)) + .f1(4, 5)
--
fragment17_5 :: (PreAst, [(Name, Definition (Label Type) PreAst)])
fragment17_5 =
  ( eOp2
      oAddInt
      (eApp
         tInt
         (eVar (tInt ~> tInt, ".f3"))
         [eApp tInt (eVar (tInt ~> tInt, ".f0")) [eLit (PInt 3)]])
      (eApp
         tInt
         (eVar (tInt ~> tInt ~> tInt, ".f1"))
         [eLit (PInt 4), eLit (PInt 5)])
  , [ (".f0", Function (fromList [(tVar 2, "x")]) (tVar 2, eVar (tVar 2, "x")))
    , ( ".f1"
      , Function
          (fromList [(tVar 22, "x"), (tVar 22, "y")])
          (tVar 22, eOp2 (Op2 OAdd (tVar 22 ~> tVar 22 ~> tVar 22)) (eVar (tVar 22, "x")) (eVar (tVar 22, "y"))))
    , ( ".f2"
      , Function
          (fromList [(tInt, ".v0")])
          ( tInt
          , eApp
              tInt
              (eVar (tInt ~> tInt ~> tInt, ".f1"))
              [eLit (PInt 2), eVar (tInt, ".v0")]))
    , ( ".f3"
      , Function
          (fromList [(tInt, ".v0")])
          ( tInt
          , eApp
              tInt
              (eVar ((tInt ~> tInt) ~> tInt ~> tInt, ".f0"))
              [eVar (tInt ~> tInt, ".f2"), eVar (tInt, ".v0")]))
    ])


--
-- .f0(x) = x
-- .f1(x, y) = x + y
-- .f2(.v0) = .g0(2, .v0)
-- .f3(.v0) = .g1(.f2, .v0)
-- .g0(x, y) = x + y
-- .g1(x, .v0) = x(.v0)
-- .g2(x) = x
-- .g3(x, y) = x + y
--
-- .f3(.g2(3)) + .g3(4, 5)
--
fragment17_6 :: (PreAst, [(Name, Definition (Label Type) PreAst)])
fragment17_6 =
  ( eOp2
      oAddInt
      (eApp
         tInt
         (eVar (tInt ~> tInt, ".f3"))
         [eApp tInt (eVar (tInt ~> tInt, ".g2")) [eLit (PInt 3)]])
      (eApp
         tInt
         (eVar (tInt ~> tInt ~> tInt, ".g3"))
         [eLit (PInt 4), eLit (PInt 5)])
  , [ (".f0", Function (fromList [(tVar 2, "x")]) (tVar 2, eVar (tVar 2, "x")))
    , ( ".f1"
      , Function
          (fromList [(tVar 22, "x"), (tVar 22, "y")])
          (tVar 22, eOp2 (Op2 OAdd (tVar 22 ~> tVar 22 ~> tVar 22)) (eVar (tVar 22, "x")) (eVar (tVar 22, "y"))))
    , ( ".f2"
      , Function
          (fromList [(tInt, ".v0")])
          ( tInt
          , eApp
              tInt
              (eVar (tInt ~> tInt ~> tInt, ".g0"))
              [eLit (PInt 2), eVar (tInt, ".v0")]))
    , ( ".f3"
      , Function
          (fromList [(tInt, ".v0")])
          ( tInt
          , eApp
              tInt
              (eVar ((tInt ~> tInt) ~> tInt ~> tInt, ".g1"))
              [eVar (tInt ~> tInt, ".f2"), eVar (tInt, ".v0")]))
    , ( ".g0"
      , Function
          (fromList [(tInt, "x"), (tInt, "y")])
          (tInt, eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))))
    , ( ".g1"
      , Function
          (fromList [(tInt ~> tInt, "x"), (tInt, ".v0")])
          ( tInt
          , eApp tInt (eVar (tInt ~> tInt, "x")) [eVar (tInt, ".v0")]))
    , (".g2", Function (fromList [(tInt, "x")]) (tInt, eVar (tInt, "x")))
    , ( ".g3"
      , Function
          (fromList [(tInt, "x"), (tInt, "y")])
          (tInt, eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))))
    ])

-- .f0(x) = x
-- .f1(x, y) = x + y
-- .f2(.v0) = .g0(2, .v0)
-- .f3(.v0) = .g1(.f2, .v0)
-- .g0(x, y) = x + y
-- .g1(x, .v0) = x(.v0)
-- .g2(x) = x
-- .g3(x, y) = x + y
-- .h0(.v0) = .f2(.v0)
--
-- .f3(.g2(3)) + .g3(4, 5)
fragment17_7 ::
     ( Expr Type Type a1 a2
     , [(Name, Definition (Label Type) (Expr Type Type a1 a2))])
fragment17_7 =
  ( eOp2
      oAddInt
      (eApp
         tInt
         (eVar (tInt ~> tInt, ".f3"))
         [eApp tInt (eVar (tInt ~> tInt, ".g2")) [eLit (PInt 3)]])
      (eApp
         tInt
         (eVar (tInt ~> tInt ~> tInt, ".g3"))
         [eLit (PInt 4), eLit (PInt 5)])
  , [ (".f0", Function (fromList [(tVar 2, "x")]) (tVar 2, eVar (tVar 2, "x")))
    , ( ".f1"
      , Function
          (fromList [(tVar 22, "x"), (tVar 22, "y")])
          (tVar 22, eOp2 (Op2 OAdd (tVar 22 ~> tVar 22 ~> tVar 22)) (eVar (tVar 22, "x")) (eVar (tVar 22, "y"))))
    , ( ".f2"
      , Function
          (fromList [(tInt, ".v0")])
          ( tInt
          , eApp
              tInt
              (eVar (tInt ~> tInt ~> tInt, ".g0"))
              [eLit (PInt 2), eVar (tInt, ".v0")]))
    , ( ".f3"
      , Function
          (fromList [(tInt, ".v0")])
          ( tInt
          , eApp
              tInt
              (eVar (tInt ~> tInt, ".h0"))
              [eVar (tInt, ".v0")]))
    , ( ".g0"
      , Function
          (fromList [(tInt, "x"), (tInt, "y")])
          (tInt, eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))))
    , ( ".g1"
      , Function
          (fromList [(tInt ~> tInt, "x"), (tInt, ".v0")])
          ( tInt
          , eApp tInt (eVar (tInt ~> tInt, "x")) [eVar (tInt, ".v0")]))
    , (".g2", Function (fromList [(tInt, "x")]) (tInt, eVar (tInt, "x")))
    , ( ".g3"
      , Function
          (fromList [(tInt, "x"), (tInt, "y")])
          (tInt, eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))))
    , ( ".h0"
      , Function
          (fromList [(tInt, ".v0")])
          ( tInt
          , eApp tInt (eVar (tInt ~> tInt, ".f2")) [eVar (tInt, ".v0")]))
    ])


fragment17_8 :: (Ast, [(Name, Definition (Label Type) Ast)])
fragment17_8 =
  ( eOp2
      oAddInt
      (eCall (tInt ~> tInt, ".f3")
         [eCall (tInt ~> tInt, ".g2") [eLit (PInt 3)]])
      (eCall
         (tInt ~> tInt ~> tInt, ".g3")
         [eLit (PInt 4), eLit (PInt 5)])
  , [ (".f0", Function (fromList [(tVar 2, "x")]) (tVar 2, eVar (tVar 2, "x")))
    , ( ".f1"
      , Function
          (fromList [(tVar 22, "x"), (tVar 22, "y")])
          (tVar 22, eOp2 (Op2 OAdd (tVar 22 ~> tVar 22 ~> tVar 22)) (eVar (tVar 22, "x")) (eVar (tVar 22, "y"))))
    , ( ".f2"
      , Function
          (fromList [(tInt, ".v0")])
          ( tInt
          , eCall
              (tInt ~> tInt ~> tInt, ".g0")
              [eLit (PInt 2), eVar (tInt, ".v0")]))
    , ( ".f3"
      , Function
          (fromList [(tInt, ".v0")])
          ( tInt
          , eCall
              (tInt ~> tInt, ".h0")
              [eVar (tInt, ".v0")]))
    , ( ".g0"
      , Function
          (fromList [(tInt, "x"), (tInt, "y")])
          (tInt, eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))))
    , ( ".g1"
      , Function
          (fromList [(tInt ~> tInt, "x"), (tInt, ".v0")])
          ( tInt
          , eCall (tInt ~> tInt, "x") [eVar (tInt, ".v0")]))
    , (".g2", Function (fromList [(tInt, "x")]) (tInt, eVar (tInt, "x")))
    , ( ".g3"
      , Function
          (fromList [(tInt, "x"), (tInt, "y")])
          (tInt, eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))))
    , ( ".h0"
      , Function
          (fromList [(tInt, ".v0")])
          ( tInt
          , eCall (tInt ~> tInt, ".f2") [eVar (tInt, ".v0")]))
    ])


-- let
--   f =
--     lam(n) => 
--       if 0 == n
--         then
--           1
--         else
--           n * f(n - 1)
--   in
--     f(5)
--
fragment18_1 :: Expr () () () Void
fragment18_1 =
  eLet
    ((), "f")
    (eLam ()
       [((), "n")]
       (eIf
          (eOp2 (Op2 OEq ()) (eLit (PInt 0)) (eVar ((), "n")))
          (eLit (PInt 1))
          (eOp2
             (Op2 OMul ())
             (eVar ((), "n"))
             (eApp
                ()
                (eVar ((), "f"))
                [eOp2 (Op2 OSub ()) (eVar ((), "n")) (eLit (PInt 1))]))))
    (eApp () (eVar ((), "f")) [eLit (PInt 5)])

-- let
--   f =
--     lam(n) => 
--       if 0 == n
--         then
--           1
--         else
--           n * f(n - 1)
--   in
--     f(5)
--
fragment18_2 :: Expr Type Type () Void
fragment18_2 =
  eLet
    (tInt ~> tInt, "f")
    (eLam ()
       [(tInt, "n")]
       (eIf
          (eOp2 oEqInt (eLit (PInt 0)) (eVar (tInt, "n")))
          (eLit (PInt 1))
          (eOp2
             oMulInt
             (eVar (tInt, "n"))
             (eApp
                tInt
                (eVar (tInt ~> tInt, "f"))
                [eOp2 oSubInt (eVar (tInt, "n")) (eLit (PInt 1))]))))
    (eApp tInt (eVar (tInt ~> tInt, "f")) [eLit (PInt 5)])

fragment18_3 :: (PreAst, [(Name, Definition (Label Type) PreAst)])
fragment18_3 =
  ( eApp tInt (eVar (tInt ~> tInt, ".f0")) [eLit (PInt 5)]
  , [ ( ".f0"
      , Function
          (fromList [(tInt, "n")])
          ( tInt
          , eIf
              (eOp2 oEqInt (eLit (PInt 0)) (eVar (tInt, "n")))
              (eLit (PInt 1))
              (eOp2
                 oMulInt
                 (eVar (tInt, "n"))
                 (eApp
                    tInt
                    (eVar (tInt ~> tInt, ".f0"))
                    [eOp2 oSubInt (eVar (tInt, "n")) (eLit (PInt 1))]))))
    ])

fragment18_4 :: (Ast, [(Name, Definition (Label Type) Ast)])
fragment18_4 =
  ( eCall (tInt ~> tInt, ".f0") [eLit (PInt 5)]
  , [ ( ".f0"
      , Function
          (fromList [(tInt, "n")])
          ( tInt
          , eIf
              (eOp2 oEqInt (eLit (PInt 0)) (eVar (tInt, "n")))
              (eLit (PInt 1))
              (eOp2
                 oMulInt
                 (eVar (tInt, "n"))
                 (eCall
                    (tInt ~> tInt, ".f0")
                    [eOp2 oSubInt (eVar (tInt, "n")) (eLit (PInt 1))]))))
    ])

--
-- f(x) = x
-- g(y) = y + 1
-- z(h, n, j) = h(j(n))
--
-- z(f, 5, g)
--
fragment19_2 :: (PreAst, [(Name, Definition (Label Type) PreAst)])
fragment19_2 =
  ( eApp
      tInt
      (eVar (tInt, "z"))
      [ eVar (tInt ~> tInt, "f")
      , eLit (PInt 5)
      , eVar (tInt ~> tInt, "g")
      ]
  , [ ("f", Function (fromList [(tInt, "x")]) (tInt, eVar (tInt, "x")))
    , ( "g"
      , Function
          (fromList [(tInt, "y")])
          (tInt, eOp2 oAddInt (eVar (tInt, "y")) (eLit (PInt 1))))
    , ( "z"
      , Function
          (fromList
             [(tInt ~> tInt, "h"), (tInt, "n"), (tInt ~> tInt, "j")])
          ( tInt
          , eApp
              tInt
              (eVar (tInt ~> tInt, "h"))
              [eApp tInt (eVar (tInt ~> tInt, "j")) [eVar (tInt, "n")]]))
    ])

--
-- f(x) = x
-- g(y) = y + 1
-- z(h, n, j) = h(j(n))
-- .h0(n) = f(g(n))
--
-- .h0(5)
--
fragment19_3 :: (PreAst, [(Name, Definition (Label Type) PreAst)])
fragment19_3 =
  ( eApp tInt (eVar (tInt ~> tInt, ".h0")) [eLit (PInt 5)]
  , [ ( ".h0"
      , Function
          (fromList [(tInt, "n")])
          ( tInt
          , eApp
              tInt
              (eVar (tInt ~> tInt, "f"))
              [eApp tInt (eVar (tInt ~> tInt, "g")) [eVar (tInt, "n")]]))
    , ("f", Function (fromList [(tInt, "x")]) (tInt, eVar (tInt, "x")))
    , ( "g"
      , Function
          (fromList [(tInt, "y")])
          (tInt, eOp2 oAddInt (eVar (tInt, "y")) (eLit (PInt 1))))
    , ( "z"
      , Function
          (fromList
             [(tInt ~> tInt, "h"), (tInt, "n"), (tInt ~> tInt, "j")])
          ( tInt
          , eApp
              tInt
              (eVar (tInt ~> tInt, "h"))
              [eApp tInt (eVar (tInt ~> tInt, "j")) [eVar (tInt, "n")]]))
    ])

fragment20_1 :: Value 
fragment20_1 = ConValue "Cons" [LitValue (PInt 5), ConValue "Nil" []]

--fragment20_2 ::
--     ( MonadFix m
--     , MonadReader ( Environment (Definition (Label Type) Ast)
--                   , Environment (Value m)) m
--     )
--  => m (Value m)
--fragment20_2 =
--  evalCase
--    fragment20_1
--    [ ( [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
--        , (tInt, "x")
--        , (tCon "List" [tInt], "xs")
--        ]
--      , pure (LitValue (PInt 100)))
--    ]

fragment20_3 :: (Ast, [(Name, Definition (Label Type) Ast)])
fragment20_3 =
  ( eCase
      (eCall
         (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
         [eLit (PInt 5), eCall (tCon "List" [tInt], "Nil") []])
      [ ( [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
          , (tInt, "x")
          , (tCon "List" [tInt], "xs")
          ]
        , eVar (tInt, "x"))
      ]
  , [])

fragment20_4 :: (Ast, [(Name, Definition (Label Type) Ast)])
fragment20_4 =
  ( eCase
      (eCall
         (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
         [eLit (PInt 5), eCall (tCon "List" [tInt], "Nil") []])
      [ ( [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
          , (tInt, "x")
          , (tCon "List" [tInt], "xs")
          ]
        , eVar (tInt, "x"))
      , ([(tCon "List" [tInt], "Nil")], eLit (PInt 0))
      ]
  , [])

fragment20_5 :: (Ast, [(Name, Definition (Label Type) Ast)])
fragment20_5 =
  ( eCase
      (eCall
         (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
         [eLit (PInt 5), eCall (tCon "List" [tInt], "Nil") []])
      [ ([(tCon "List" [tInt], "Nil")], eLit (PInt 0))
      , ( [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
          , (tInt, "x")
          , (tCon "List" [tInt], "xs")
          ]
        , eVar (tInt, "x"))
      ]
  , [])


fragment20_6 :: (Ast, [(Name, Definition (Label Type) Ast)])
fragment20_6 =
  ( eCase
      (eCall (tCon "List" [tInt], "Nil") [])
      [ ( [ (tInt ~> tCon "List" [tInt] ~> tCon "List" [tInt], "Cons")
          , (tInt, "x")
          , (tCon "List" [tInt], "xs")
          ]
        , eVar (tInt, "x"))
      , ([(tCon "List" [tInt], "Nil")], eLit (PInt 0))
      ]
  , [])

-- 
-- match ( a = 1, b = 2, c = 3 } {
--   | { b = x | r } =>
--     x
-- }
--
-- field { b = x | r } = ( a = 1, b = 2, c = 3 } in x
--
fragment21_0 :: Expr () a0 a1 a2
fragment21_0 = 
  eField 
      [((), "{b}"), ((), "x"), ((), "r")]
      (eRow (rExt "a" (eLit PUnit) (rExt "b" (eLit (PInt 2)) (rExt "c" (eLit (PBool True)) rNil))))
      (eVar ((), "x"))
        

-- match ( a = 1, b = 2, c = 3 } {
--   | q =>
--     1
-- }
--
--
fragment22_0 :: Expr () a0 a1 a2
fragment22_0 = 
  eCase 
      (eRow (rExt "a" (eLit PUnit) (rExt "b" (eLit (PInt 2)) (rExt "c" (eLit (PBool True)) rNil))))
        [ ([((), "q")], eLit (PInt 1)) 
        ]


fragment21_1 :: TypedExpr
fragment21_1 = 
  eField 
      [(tInt ~> tRow (rExt "a" tUnit (rExt "c" tBool rNil)) ~> tRow (rExt "a" tUnit (rExt "b" tInt (rExt "c" tBool rNil))), "{b}"), (tInt, "x"), (tRow (rExt "a" tUnit (rExt "c" tBool rNil)), "r")]
      (eRow (rExt "a" (eLit PUnit) (rExt "b" (eLit (PInt 2)) (rExt "c" (eLit (PBool True)) rNil))))
      (eVar (tInt, "x"))

--  eCase 
--      (eRow (rExt "a" (eLit PUnit) (rExt "b" (eLit (PInt 2)) (rExt "c" (eLit (PBool True)) rNil))))
--        [ ([(tInt ~> tRow (rExt "a" tUnit (rExt "c" tBool rNil)) ~> tRow (rExt "a" tUnit (rExt "b" tInt (rExt "c" tBool rNil))), "{b}"), (tInt, "x"), (tRow (rExt "a" tUnit (rExt "c" tBool rNil)), "r")], eVar (tInt, "x")) 
--        ]

fragment21_2 :: Ast
fragment21_2 = 
  eField
      [(tInt ~> tRow (rExt "a" tUnit (rExt "c" tBool rNil)) ~> tRow (rExt "a" tUnit (rExt "b" tInt (rExt "c" tBool rNil))), "{b}"), (tInt, "x"), (tRow (rExt "a" tUnit (rExt "c" tBool rNil)), "r")] 
      (eRow (rExt "a" (eLit PUnit) (rExt "b" (eLit (PInt 2)) (rExt "c" (eLit (PBool True)) rNil))))
      (eVar (tInt, "x")) 

--  eCase 
--      (eRow (rExt "a" (eLit PUnit) (rExt "b" (eLit (PInt 2)) (rExt "c" (eLit (PBool True)) rNil))))
--        [ ([(tInt ~> tRow (rExt "a" tUnit (rExt "c" tBool rNil)) ~> tRow (rExt "a" tUnit (rExt "b" tInt (rExt "c" tBool rNil))), "{b}"), (tInt, "x"), (tRow (rExt "a" tUnit (rExt "c" tBool rNil)), "r")], eVar (tInt, "x")) 
--        ]


--frag22 = rExt "a" (eLit (PInt 1)) (rExt "b" (eLit (PInt 2)) (rExt "c" (eLit (PInt 3)) rNil))


--testx123 = evalProgram_
--  ( eCase 
--      (eRow (rExt "a" (eLit (PInt 1)) (rExt "b" (eLit (PInt 2)) (rExt "c" (eLit (PInt 3)) rNil))))
--        [ ([(undefined, "{b}"), (tInt, "x"), (undefined, "r")], undefined) 
--        ]
--  , [])


--test456 = runReader fragment20_2 mempty == LitValue (PInt 1)
--test457 = runReader fragment20_3 mempty == LitValue (PInt 5)
--fragment20_1 :: Value
--fragment20_1 = ConValue "Cons" [LitValue (PInt 5), ConValue "Nil" []]
--fragment16_1 :: Expr Int Int () Void
--fragment16_1 =  
--  eLet
--    (1, "f")
--    (eLam [(2, "x")] (eVar (3, "x")))
--    (eVar (1, "f"))
--        (eLam [(3, "g")] 
--          (eLam [(4, "b")] 
--            (eApp 5 (eApp 6 (eVar (7, "f")) [eLam [(8, "x")] (eVar (9, "x"))]) 
--              [eApp 10 (eVar (11, "g")) [eVar (12, "b")]])
--          )))
--    (eApp 13 (eVar (14, "z")) 
--        [eLam [(15, "x")] (eVar (16, "x")), eLam [(17, "x")] (eVar (18, "x")), eLit (PInt 1)])
--i32 :: Type
--i32 = tInt
--
--input1 :: Expr () () () () a3
--input1 =
--  let_
--    ((), "f")
--    (var ((), "foo"))
--    (lam [((), "x")] (app (var ((), "f")) [var ((), "x")]))
--
--input1Typed :: Expr Type () () () Void
--input1Typed =
--  let_
--    (i32 ~> i32, "f")
--    (var (i32 ~> i32, "foo"))
--    (lam [(i32, "x")] (app (var (i32 ~> i32, "f")) [var (i32, "x")]))
--
--input2 :: Expr () a0 a1 a2 a3
--input2 = op2 oAddInt (var ((), "x")) (var ((), "y"))
--
--input2Typed :: Expr Type () () () Void
--input2Typed = op2 oAddInt (var (i32, "x")) (var (i32, "y"))
--
--input3 :: Expr () () a1 a2 a3
--input3 =
--  case_
--    (var ((), "xs"))
--    [([((), "Cons"), ((), "x"), ((), "ys")], var ((), "x"))]
--
--input4 :: Expr () a0 a1 a2 a3
--input4 =
--  case_
--    (var ((), "xs"))
--    [([((), "Cons"), ((), "x"), ((), "ys")], var ((), "y"))]
--
--input5 :: Expr () a0 a1 a2 a3
--input5 =
--  case_
--    (var ((), "xs"))
--    [ ([((), "Nil")], var ((), "y"))
--    , ([((), "Cons"), ((), "x"), ((), "ys")], var ((), "x"))
--    ]
--
--input6 :: Expr Type () () () a3
--input6 =
--  let_
--    (i32, "sum")
--    (lam
--       [(i32, "m")]
--       (let_
--          (i32, "p")
--          (lit (PInt 3))
--          (lam
--             [(i32, "n")]
--             (op2
--                oAddInt
--                (op2 oAddInt (var (i32, "m")) (var (i32, "n")))
--                (var (i32, "p"))))))
--    (var (i32 ~> i32 ~> i32, "sum"))
--
--input6NoLetBindings :: Expr Type a0 () () a3
--input6NoLetBindings =
--  lam
--    [(i32, "m")]
--    (lam
--       [(i32, "n")]
--       (op2
--          oAddInt
--          (op2 oAddInt (var (i32, "m")) (var (i32, "n")))
--          (lit (PInt 3))))
--
--input7 :: Expr Type () () () a3
--input7 =
--  let_
--    (i32, "x")
--    (app (var (i32 ~> i32, "foo")) [var (i32, "x")])
--    (op2 oAddInt (var (i32, "x")) (lit (PInt 3)))
--
--input7NoLetBindings :: Expr Type a0 () () a3
--input7NoLetBindings =
--  app
--    (lam [(i32, "x")] (op2 oAddInt (var (i32, "x")) (lit (PInt 3))))
--    [app (var (i32 ~> i32, "foo")) [var (i32, "x")]]
--
--input8 :: Expr Type a0 () () a3
--input8 =
--  lam
--    [(i32, "m")]
--    (lam
--       [(i32, "n")]
--       (op2
--          oAddInt
--          (op2 oAddInt (var (i32, "m")) (var (i32, "n")))
--          (lit (PInt 3))))
--
--input8Converted :: Expr Type a0 () () a3
--input8Converted =
--  lam
--    [(i32, "m"), (i32, "n")]
--    (op2
--       oAddInt
--       (op2 oAddInt (var (i32, "m")) (var (i32, "n")))
--       (lit (PInt 3)))
--
--input9 :: Expr Type a0 () () a3
--input9 = lam [(i32, "p")] (lam [(i32, "x")] (var (i32, "p")))
--
--input9ClosuresConverted :: Expr Type a0 () () a3
--input9ClosuresConverted =
--  lam
--    [(i32, "p")]
--    (app (lam [(i32, "p"), (i32, "x")] (var (i32, "p"))) [var (i32, "p")])
--
--input10 :: Program
--input10 =
--  Program
--    { count = 0
--    , definitions =
--        Map.fromList
--          [ ("foo", Function (Signature [(i32, "x")] (i32, lit (PInt 123))))
--          , ("baz", Function (Signature [(i32, "x")] (i32, lit (PInt 123))))
--          ]
--    }
--
----input11 :: Program
----input11 =
----  Program
----    { count = 0
----    , definitions =
----        Map.fromList
----          [ ( "plus"
----            , Function
----                (Signature
----                   [(i32, "x"), (i32, "y")]
----                   (i32, bOp2 oAddInt (bVar "x") (bVar "y"))))
----          , ( "fun"
----            , Function
----                (Signature [(i32, "x")] (i32 ~> i32, bCall "plus" [bVar "x"])))
----          ]
----    }
--input12 :: Expr () () () () a3
--input12 = lam [((), "x")] (app (var ((), "plus")) [var ((), "x")])
--
--input13 :: TypeEnv
--input13 = Env.fromList [("plus", i32 ~> i32 ~> i32)]
--
----input14 :: Body
----input14 = bCase (bVar "xs") [(["Cons", "x", "ys"], bVar "x")]
--input15 :: Expr Type a0 () () a3
--input15 =
--  lam
--    [(i32, "x")]
--    (lam [(i32, "y")] (op2 oAddInt (var (i32, "x")) (var (i32, "y"))))
--
--input15ClosuresConverted :: Expr Type a0 () () a3
--input15ClosuresConverted =
--  lam
--    [(i32, "x")]
--    (app
--       (lam
--          [(i32, "x"), (i32, "y")]
--          (op2 oAddInt (var (i32, "x")) (var (i32, "y"))))
--       [var (i32, "x")])
--
---- (\xs : List -> foo(xs))(Cons(5, Nil))
--input16 :: Expr Type () a1 () a3
--input16 =
--  let_
--    (tData "List", "xs")
--    (app
--       (var (tInt ~> tData "List" ~> tData "List", "Cons"))
--       [lit (PInt 5), var (tData "List", "Nil")])
--    (app (var (tData "List" ~> tInt, "foo")) [var (tData "List", "xs")])
--
---- (\xs : List -> foo(xs))(Cons(5, Nil))
--input16NoLetBindings :: Expr Type a0 () () a3
--input16NoLetBindings =
--  app
--    (lam
--       [(tData "List", "xs")]
--       (app (var (tData "List" ~> tInt, "foo")) [var (tData "List", "xs")]))
--    [ app
--        (var (tInt ~> tData "List" ~> tData "List", "Cons"))
--        [lit (PInt 5), var (tData "List", "Nil")]
--    ]
--
--input160 :: [(Name, Definition TypedExpr)]
--input160 =
--  [ ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tInt, tData "List"]])
--  , ("foo", Function (Signature [] (tInt, fooAst)))
--  , ("main", Function (Signature [] (tInt, mainAst)))
--  ]
--  where
--    fooAst = let_ undefined undefined undefined
--    mainAst = undefined -- app (var ((), "foo")) []
--
----        ((), "xs")
----        (app () (var ((), "Cons")) [lit (PInt 5), app () (var ((), "Nil")) []])
----        (let_
----           ((), "ys")
----           (app () (var ((), "Cons")) [lit (PInt 5), var ((), "xs")])
----           (case_
----              (var ((), "ys"))
----              [ ([((), "Nil")], lit (PInt 1))
----              , ( [((), "Cons"), ((), "_"), ((), "zs")]
----                , case_
----                    (var ((), "zs"))
----                    [ ([((), "Nil")], lit (PInt 2))
----                    , ([((), "Cons"), ((), "_"), ((), "_")], lit (PInt 3))
----                    ])
----              ]))
--input160Compiled :: [(Name, Definition Ast)]
--input160Compiled =
--  [ ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tInt, tData "List"]])
--  , ( "def_0"
--    , Function
--        (Signature
--           [ (tInt ~> tData "List" ~> tData "List", "Cons")
--           , (tData "List", "xs")
--           ]
--           ( tInt
--           , call_
--               (undefined, "def_1")
--               [ call_
--                   (undefined, "Cons")
--                   [lit (PInt 5), var (undefined, "xs")]
--               ])))
--  , ( "def_1"
--    , Function
--        (Signature
--           [(tData "List", "ys")]
--           ( tInt
--           , case_
--               (var (undefined, "ys"))
--               [ ([(undefined, "Nil")], lit (PInt 1))
--               , ( [(undefined, "Cons"), (undefined, "_"), (undefined, "zs")]
--                 , case_
--                     (var (undefined, "zs"))
--                     [ ([(undefined, "Nil")], lit (PInt 2))
--                     , ( [ (undefined, "Cons")
--                         , (undefined, "_")
--                         , (undefined, "_")
--                         ]
--                       , lit (PInt 3))
--                     ])
--               ])))
--  , ( "foo"
--    , Function
--        (Signature
--           []
--           ( tInt
--           , call_
--               (undefined, "def_0")
--               [ var (undefined, "Cons")
--               , call_
--                   (undefined, "Cons")
--                   [lit (PInt 5), call_ (undefined, "Nil") []]
--               ])))
--  , ("main", Function (Signature [] (tInt, call_ (undefined, "foo") [])))
--  ]
--
--program1 :: [(Name, Definition (SourceExpr ()))]
--program1 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt] tInt))
--  , ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tVar 0, tData "List"]])
--  , ( "foo"
--    , Function
--        (Signature
--           [(tUnit, "_")]
--           ( tInt
--           , let_
--               ((), "foo")
--               (app
--                  (var ((), "Cons"))
--                  [lit (PInt 5), app (var ((), "Nil")) []])
--               (case_
--                  (var ((), "foo"))
--                  [ ([((), "Cons"), ((), "x"), ((), "xs")], var ((), "x"))
--                  , ([((), "Nil")], lit (PInt 9))
--                  ]))))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt
--           , app (var ((), "print_int32")) [app (var ((), "foo")) [lit PUnit]])))
--  ]
--
--program2 :: [(Name, Definition (SourceExpr ()))]
--program2 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt] tInt))
--  , ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tVar 0, tData "List"]])
--  , ( "foo"
--    , Function
--        (Signature
--           [(tUnit, "_")]
--           ( tInt
--           , let_
--               ((), "foo")
--               (app (var ((), "Nil")) [])
--               (case_
--                  (var ((), "foo"))
--                  [ ([((), "Cons"), ((), "x"), ((), "xs")], var ((), "x"))
--                  , ([((), "Nil")], lit (PInt 9))
--                  ]))))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt
--           , app (var ((), "print_int32")) [app (var ((), "foo")) [lit PUnit]])))
--  ]
--
--program3 :: [(Name, Definition (SourceExpr ()))]
--program3 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt] tInt))
--  , ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tVar 0, tData "List"]])
--  , ( "foo"
--    , Function
--        (Signature
--           [(tUnit, "_")]
--           ( tInt
--           , let_
--               ((), "foo")
--               (app
--                  (var ((), "Cons"))
--                  [lit (PInt 5), app (var ((), "Nil")) []])
--               (case_
--                  (var ((), "foo"))
--                  [ ( [((), "Cons"), ((), "x"), ((), "xs")]
--                    , op2 oAddInt (var ((), "x")) (lit (PInt 1)))
--                  , ([((), "Nil")], lit (PInt 9))
--                  ]))))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt
--           , app (var ((), "print_int32")) [app (var ((), "foo")) [lit PUnit]])))
--  ]
--
--program4 :: [(Name, Definition (SourceExpr ()))]
--program4 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt] tInt))
--  , ( "add"
--    , Function
--        (Signature
--           [(tInt, "x"), (tInt, "y")]
--           (tInt, op2 oAddInt (var ((), "x")) (var ((), "y")))))
--  , ( "add5"
--    , Function
--        (Signature [] (tInt ~> tInt, app (var ((), "add")) [lit (PInt 5)])))
--  , ( "add5_2"
--    , Function
--        (Signature
--           [(tUnit, "_")]
--           (tInt, app (var ((), "add5")) [lit (PInt 2)])))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt
--           , app
--               (var ((), "print_int32"))
--               [app (var ((), "add5_2")) [lit PUnit]])))
--  ]
--
--program5 :: [(Name, Definition (SourceExpr ()))]
--program5 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt] tInt))
--  , ( "add"
--    , Function
--        (Signature
--           [(tInt, "x"), (tInt, "y")]
--           (tInt, op2 oAddInt (var ((), "x")) (var ((), "y")))))
--  , ( "add55"
--    , Function
--        (Signature
--           []
--           (tInt ~> tInt, app (var ((), "add")) [lit (PInt 55)])))
--  , ( "foo"
--    , Function
--        (Signature
--           [(tInt ~> tInt, "f")]
--           (tInt, app (var ((), "f")) [lit (PInt 2)])))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt
--           , app
--               (var ((), "print_int32"))
--               [app (var ((), "foo")) [var ((), "add55")]])))
--  ]
--
--program6 :: [(Name, Definition (SourceExpr ()))]
--program6 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt] tInt))
--  , ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tVar 0, tData "List"]])
--  , ( "foo"
--    , Function
--        (Signature
--           [(tUnit, "_")]
--           ( tInt
--           , let_
--               ((), "abc")
--               (app (var ((), "Cons")) [lit (PInt 5)])
--               (case_
--                  (app (var ((), "abc")) [app (var ((), "Nil")) []])
--                  [ ([((), "Cons"), ((), "x"), ((), "xs")], var ((), "x"))
--                  , ([((), "Nil")], lit (PInt 9))
--                  ]))))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt
--           , app (var ((), "print_int32")) [app (var ((), "foo")) [lit PUnit]])))
--  ]
--
--program7 :: [(Name, Definition (SourceExpr ()))]
--program7 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt] tInt))
--  , ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tVar 0, tData "List"]])
--  , ( "foo"
--    , Function
--        (Signature
--
----           [(tUnit, "_")]
--           ( tInt
--           , let_
--               ((), "abc")
--               (app (var ((), "Cons")) [lit (PInt 5)])
--               (case_
--                  (app (var ((), "abc")) [var ((), "Nil")])
--                  [ ([((), "Cons"), ((), "x"), ((), "xs")], var ((), "x"))
--                  , ([((), "Nil")], lit (PInt 9))
--                  ]))))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt
--           , app (var ((), "print_int32")) [app (var ((), "foo")) [lit PUnit]])))
--  ]
--
----
---- let
----   id =
----     lam(x) => x
----   in
----     let 
----       f =
----         lam(y) => y + 1
----       in
----         (id(f))(id(5))
----
--input170 :: SourceExpr ()
--input170 =
--  let_
--    ((), "id")
--    (lam [((), "x")] (var ((), "x")))
--    (let_
--      ((), "f")
--      (lam [((), "y")] (op2 oAddInt (var ((), "y")) (lit (PInt 1))))
--      (app (app (var ((), "id")) [var ((), "f")]) [app (var ((), "id")) [lit (PInt 5)]]))
--
--input171 :: TypedExpr
--input171 =
--  let_
--    (tVar 0 ~> tVar 0, "id")
--    (lam [(tVar 0, "x")] (var (tVar 0, "x")))
--    (let_
--      (i32 ~> i32, "f")
--      (lam [(i32, "y")] (op2 oAddInt (var (i32, "y")) (lit (PInt 1))))
--      (app (app (var ((i32 ~> i32) ~> i32 ~> i32, "id")) [var (i32 ~> i32, "f")]) [app (var (i32 ~> i32, "id")) [lit (PInt 5)]]))
--
