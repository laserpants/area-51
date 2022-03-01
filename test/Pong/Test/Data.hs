{-# LANGUAGE OverloadedStrings #-}

module Pong.Test.Data where

import Data.List.NonEmpty (fromList, toList)
import qualified Data.Map.Strict as Map
import Data.Void
import Pong.Data
import Pong.Lang
import Pong.Util
import qualified Pong.Util.Env as Env

-- (x) = plus(x)
fragment1_0 :: Definition (Label Type) (Expr Type Type () Void)
fragment1_0 =
  Function
    (fromList [(tInt32, "x")])
    ( tInt32 ~> tInt32
    , eApp
        (tInt32 ~> tInt32)
        (eVar (tInt32 ~> tInt32 ~> tInt32, "plus"))
        [eVar (tInt32, "x")])

-- (x, v0) = plus(x, v0)
fragment1_1 :: Definition (Label Type) (Expr Type Type () Void)
fragment1_1 =
  Function
    (fromList [(tInt32, "x"), (tInt32, ".v0")])
    ( tInt32
    , eApp
        tInt32
        (eVar (tInt32 ~> tInt32 ~> tInt32, "plus"))
        [eVar (tInt32, "x"), eVar (tInt32, ".v0")])

fragment2_0 :: Definition (Label Type) (Expr Type () () Void)
fragment2_0 =
  Function
    (fromList [(tInt32, "x")])
    ( tInt32 ~> tInt32 ~> tInt32
    , eLam [(tInt32, "a")] (eLam [(tInt32, "b")] (eVar (tInt32, "b"))))

fragment2_1 :: Definition (Label Type) (Expr Type () () Void)
fragment2_1 =
  Function
    (fromList [(tInt32, "x"), (tInt32, "a"), (tInt32, "b")])
    (tInt32, eVar (tInt32, "b"))

fragment3_0 :: Expr Type Type () Void
fragment3_0 =
  eApp
    tInt32
    (eApp
       (tInt32 ~> tInt32)
       (eVar (tInt32 ~> tInt32 ~> tInt32, "g"))
       [eVar (tInt32, "x")])
    [eVar (tInt32, "y")]

fragment3_1 :: Expr Type Type () Void
fragment3_1 =
  eApp
    tInt32
    (eVar (tInt32 ~> tInt32 ~> tInt32, "g"))
    [eVar (tInt32, "x"), eVar (tInt32, "y")]

fragment4_0 :: Expr Type () () Void
fragment4_0 = eLam [(tInt32, "a")] (eLam [(tInt32, "b")] (eVar (tInt32, "b")))

fragment4_1 :: Expr Type () () Void
fragment4_1 = eLam [(tInt32, "a"), (tInt32, "b")] (eVar (tInt32, "b"))

fragment5_0 :: Expr Type () () Void
fragment5_0 =
  eLet
    (tInt32, "h")
    (eOp2 OAddInt32 (eVar (tInt32, "z")) (eLit (LInt32 1)))
    (eLet
       (tVar 0 ~> tVar 0, "g")
       (eLam [(tVar 0, "x")] (eVar (tVar 0, "x")))
       (eLet
          (tInt32 ~> tInt32, "f")
          (eLam
             [(tInt32, "y")]
             (eOp2 OAddInt32 (eVar (tInt32, "y")) (eVar (tInt32, "h"))))
          (eOp2
             OAddInt32
             (eApp
                ()
                (eApp
                   ()
                   (eVar ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32, "g"))
                   [eVar (tInt32 ~> tInt32, "f")])
                [eApp () (eVar (tInt32 ~> tInt32, "g")) [eLit (LInt32 5)]])
             (eApp () (eVar (tInt32 ~> tInt32, "f")) [eLit (LInt32 1)]))))

fragment5_1 :: Expr Type () () Void
fragment5_1 =
  eLet
    (tInt32, "h")
    (eOp2 OAddInt32 (eVar (tInt32, "z")) (eLit (LInt32 1)))
    (eLet
       (tVar 0 ~> tVar 0, "g")
       (eLam [(tVar 0, "x")] (eVar (tVar 0, "x")))
       (eLet
          (tInt32 ~> tInt32, "f")
          (eApp
             ()
             (eLam
                [(tInt32, "h"), (tInt32, "y")]
                (eOp2 OAddInt32 (eVar (tInt32, "y")) (eVar (tInt32, "h"))))
             [eVar (tInt32, "h")])
          (eOp2
             OAddInt32
             (eApp
                ()
                (eApp
                   ()
                   (eVar ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32, "g"))
                   [eVar (tInt32 ~> tInt32, "f")])
                [eApp () (eVar (tInt32 ~> tInt32, "g")) [eLit (LInt32 5)]])
             (eApp () (eVar (tInt32 ~> tInt32, "f")) [eLit (LInt32 1)]))))

fragment6_0 :: PreAst
fragment6_0 = eApp tInt32 (eVar (tInt32 ~> tInt32, "f")) [eVar (tInt32, "x")]

fragment6_1 :: Ast
fragment6_1 = eCall (tInt32 ~> tInt32, "f") [eVar (tInt32, "x")]

fragment7_0 :: Expr Type () () Void
fragment7_0 =
  eLam
    [(tInt32, "x")]
    (eOp2 OAddInt32 (eVar (tInt32, "x")) (eVar (tInt32, "h")))

fragment7_1 :: Expr Type () () Void
fragment7_1 =
  eApp
    ()
    (eLam
       [(tInt32, "h"), (tInt32, "x")]
       (eOp2 OAddInt32 (eVar (tInt32, "x")) (eVar (tInt32, "h"))))
    [eVar (tInt32, "h")]

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
    (tInt32, "h")
    (eOp2 OAddInt32 (eVar (tInt32, "z")) (eLit (LInt32 1)))
    (eLet
       (tVar 0 ~> tVar 0, "g")
       (eLam [(tVar 0, "x")] (eVar (tVar 0, "x")))
       (eLet
          (tInt32 ~> tInt32, "f")
          (eLam
             [(tInt32, "y")]
             (eOp2 OAddInt32 (eVar (tInt32, "y")) (eVar (tInt32, "h"))))
          (eOp2
             OAddInt32
             (eApp
                tInt32
                (eApp
                   (tInt32 ~> tInt32)
                   (eVar ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32, "g"))
                   [eVar (tInt32 ~> tInt32, "f")])
                [eApp tInt32 (eVar (tInt32 ~> tInt32, "g")) [eLit (LInt32 5)]])
             (eApp tInt32 (eVar (tInt32 ~> tInt32, "f")) [eLit (LInt32 1)]))))

fragment8_1 ::
     ( Expr Type Type () Void
     , [(Name, Definition (Label Type) (Expr Type Type () a2))])
fragment8_1 =
  ( eLet
      (tInt32, "h")
      (eOp2 OAddInt32 (eVar (tInt32, "z")) (eLit (LInt32 1)))
      (eOp2
         OAddInt32
         (eApp
            tInt32
            (eApp
               (tInt32 ~> tInt32)
               (eVar ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32, ".f0"))
               [eVar (tInt32 ~> tInt32, ".f1")])
            [eApp tInt32 (eVar (tInt32 ~> tInt32, ".f0")) [eLit (LInt32 5)]])
         (eApp tInt32 (eVar (tInt32 ~> tInt32, ".f1")) [eLit (LInt32 1)]))
  , [ (".f0", Function (fromList [(tVar 0, "x")]) (tVar 0, eVar (tVar 0, "x")))
    , ( ".f1"
      , Function
          (fromList [(tInt32, "y")])
          (tInt32, eOp2 OAddInt32 (eVar (tInt32, "y")) (eVar (tInt32, "h"))))
    ])

fragment9_0 :: Expr Type () () Void
fragment9_0 =
  eLet
    (tInt32, "x")
    (eVar (tInt32, "y"))
    (eOp2 OAddInt32 (eVar (tInt32, "x")) (eVar (tInt32, "z")))

fragment9_1 :: Expr Type () () Void
fragment9_1 = eOp2 OAddInt32 (eVar (tInt32, "y")) (eVar (tInt32, "z"))

fragment10_0 :: Definition (Label Type) (Expr Type Type () Void)
fragment10_0 =
  Constant
    ( tInt32
    , eLet
        (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "add3")
        (eLam
           [(tInt32, "x"), (tInt32, "y"), (tInt32, "z")]
           (eOp2
              OAddInt32
              (eVar (tInt32, "x"))
              (eOp2 OAddInt32 (eVar (tInt32, "y")) (eVar (tInt32, "z")))))
        (eLet
           (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "f")
           (eVar (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "add3"))
           (eLet
              (tInt32 ~> tInt32 ~> tInt32, "g")
              (eApp
                 (tInt32 ~> tInt32 ~> tInt32)
                 (eVar (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "f"))
                 [eLit (LInt32 5)])
              (eLet
                 (tInt32 ~> tInt32, "h")
                 (eApp
                    (tInt32 ~> tInt32)
                    (eVar (tInt32 ~> tInt32 ~> tInt32, "g"))
                    [eLit (LInt32 6)])
                 (eApp tInt32 (eVar (tInt32 ~> tInt32, "h")) [eLit (LInt32 7)])))))

fragment10_1 :: Definition (Label Type) (Expr Type Type () Void)
fragment10_1 =
  Constant
    ( tInt32
    , eLet
        (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "add3")
        (eLam
           [(tInt32, "x"), (tInt32, "y"), (tInt32, "z")]
           (eOp2
              OAddInt32
              (eVar (tInt32, "x"))
              (eOp2 OAddInt32 (eVar (tInt32, "y")) (eVar (tInt32, "z")))))
        (eLet
           (tInt32 ~> tInt32 ~> tInt32, "g")
           (eLam
              [(tInt32, ".v0"), (tInt32, ".v1")]
              (eApp
                 tInt32
                 (eVar (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "add3"))
                 [eLit (LInt32 5), eVar (tInt32, ".v0"), eVar (tInt32, ".v1")]))
           (eLet
              (tInt32 ~> tInt32, "h")
              (eLam
                 [(tInt32, ".v0")]
                 (eApp
                    tInt32
                    (eVar (tInt32 ~> tInt32 ~> tInt32, "g"))
                    [eLit (LInt32 6), eVar (tInt32, ".v0")]))
              (eApp tInt32 (eVar (tInt32 ~> tInt32, "h")) [eLit (LInt32 7)]))))

fragment11_0 :: Expr Type () () Void
fragment11_0 =
  eLet
    (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "add3")
    (eLam
       [(tInt32, "x"), (tInt32, "y"), (tInt32, "z")]
       (eOp2
          OAddInt32
          (eVar (tInt32, "x"))
          (eOp2 OAddInt32 (eVar (tInt32, "y")) (eVar (tInt32, "z")))))
    (eLet
       (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "f")
       (eVar (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "add3"))
       (eLet
          (tInt32 ~> tInt32 ~> tInt32, "g")
          (eApp
             ()
             (eVar (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "f"))
             [eLit (LInt32 5)])
          (eLet
             (tInt32 ~> tInt32, "h")
             (eApp () (eVar (tInt32 ~> tInt32 ~> tInt32, "g")) [eLit (LInt32 6)])
             (eApp () (eVar (tInt32 ~> tInt32, "h")) [eLit (LInt32 7)]))))

fragment11_1 :: Expr Type Type () Void
fragment11_1 =
  eLet
    (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "add3")
    (eLam
       [(tInt32, "x"), (tInt32, "y"), (tInt32, "z")]
       (eOp2
          OAddInt32
          (eVar (tInt32, "x"))
          (eOp2 OAddInt32 (eVar (tInt32, "y")) (eVar (tInt32, "z")))))
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
       (tInt32 ~> tInt32 ~> tInt32, "g")
       (eApp
          (tInt32 ~> tInt32 ~> tInt32)
          (eVar (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "add3"))
          [eLit (LInt32 5)])
       (eLet
          (tInt32 ~> tInt32, "h")
          (eApp
             (tInt32 ~> tInt32)
             (eVar (tInt32 ~> tInt32 ~> tInt32, "g"))
             [eLit (LInt32 6)])
          (eApp tInt32 (eVar (tInt32 ~> tInt32, "h")) [eLit (LInt32 7)])))

fragment11_2 :: Expr Type Type () Void
fragment11_2 =
  eLet
    (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "add3")
    (eLam
       [(tInt32, "x"), (tInt32, "y"), (tInt32, "z")]
       (eOp2
          OAddInt32
          (eVar (tInt32, "x"))
          (eOp2 OAddInt32 (eVar (tInt32, "y")) (eVar (tInt32, "z")))))
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
       (tInt32 ~> tInt32 ~> tInt32, "g")
       (eLam
          [(tInt32, ".v0"), (tInt32, ".v1")]
          (eApp
             tInt32
             (eVar (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "add3"))
             [eLit (LInt32 5), eVar (tInt32, ".v0"), eVar (tInt32, ".v1")]))
       (eLet
          (tInt32 ~> tInt32, "h")
          (eLam
             [(tInt32, ".v0")]
             (eApp
                tInt32
                (eVar (tInt32 ~> tInt32 ~> tInt32, "g"))
                [eLit (LInt32 6), eVar (tInt32, ".v0")]))
          (eApp tInt32 (eVar (tInt32 ~> tInt32, "h")) [eLit (LInt32 7)])))

fragment12_0 :: Expr Type Type () Void
fragment12_0 =
  eLet
    (tInt32 ~> tInt32, "g")
    (eApp
       (tInt32 ~> tInt32)
       (eVar (tInt32 ~> tInt32 ~> tInt32, "f"))
       [eLit (LInt32 1)])
    (eApp tInt32 (eVar (tInt32 ~> tInt32, "g")) [eLit (LInt32 2)])

fragment12_1 :: Expr Type Type () Void
fragment12_1 =
  eLet
    (tInt32 ~> tInt32, "g")
    (eLam
       [(tInt32, ".v0")]
       (eApp
          tInt32
          (eVar (tInt32 ~> tInt32 ~> tInt32, "f"))
          [eLit (LInt32 1), eVar (tInt32, ".v0")]))
    (eApp tInt32 (eVar (tInt32 ~> tInt32, "g")) [eLit (LInt32 2)])

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
    (eLam
       [((), "f")]
       (eLam
          [((), "g")]
          (eLam
             [((), "b")]
             (eApp
                ()
                (eApp () (eVar ((), "f")) [eLam [((), "x")] (eVar ((), "x"))])
                [eApp () (eVar ((), "g")) [eVar ((), "b")]]))))
    (eApp
       ()
       (eVar ((), "z"))
       [ eLam [((), "x")] (eVar ((), "x"))
       , eLam [((), "x")] (eVar ((), "x"))
       , eLit (LInt32 1)
       ])

fragment13_1 :: Expr Int Int () Void
fragment13_1 =
  eLet
    (1, "z")
    (eLam
       [(2, "f")]
       (eLam
          [(3, "g")]
          (eLam
             [(4, "b")]
             (eApp
                5
                (eApp 6 (eVar (7, "f")) [eLam [(8, "x")] (eVar (9, "x"))])
                [eApp 10 (eVar (11, "g")) [eVar (12, "b")]]))))
    (eApp
       13
       (eVar (14, "z"))
       [ eLam [(15, "x")] (eVar (16, "x"))
       , eLam [(17, "x")] (eVar (18, "x"))
       , eLit (LInt32 1)
       ])

fragment15_0 :: Expr Int () () Void
fragment15_0 = eApp () (eVar (1, "f")) [eLit (LInt32 1)]

fragment13_2 :: Expr Type () () Void
fragment13_2 =
  eLet
    (undefined, "z")
    (eLam
       [(undefined, "f")]
       (eLam
          [(undefined, "g")]
          (eLam
             [(undefined, "b")]
             (eApp
                undefined
                (eApp
                   undefined
                   (eVar (undefined, "f"))
                   [eLam [(undefined, "x")] (eVar (undefined, "x"))])
                [eApp undefined (eVar (undefined, "g")) [eVar (undefined, "b")]]))))
    (eApp
       undefined
       (eVar (undefined, "z"))
       [ eLam [(undefined, "x")] (eVar (undefined, "x"))
       , eLam [(undefined, "x")] (eVar (undefined, "x"))
       , eLit (LInt32 1)
       ])

fragment14_0 :: Type
fragment14_0 = tCon "Cons" [tVar 0, tVar 1]

fragment14_1 :: Type
fragment14_1 = tCon "Cons" [tInt32, tCon "Cons" [tInt32, tCon "Nil" []]]

fragment15_1 :: Expr Type () () Void
fragment15_1 =
  eLet
    (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "add3")
    (eLam
       [(tInt32, "x"), (tInt32, "y"), (tInt32, "z")]
       (eOp2
          OAddInt32
          (eVar (tInt32, "x"))
          (eOp2 OAddInt32 (eVar (tInt32, "y")) (eVar (tInt32, "z")))))
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
       (tInt32 ~> tInt32 ~> tInt32, "g")
       (eApp
          ()
          (eVar (tInt32 ~> tInt32 ~> tInt32 ~> tInt32, "add3"))
          [eLit (LInt32 5)])
       (eLet
          (tInt32 ~> tInt32, "h")
          (eApp () (eVar (tInt32 ~> tInt32 ~> tInt32, "g")) [eLit (LInt32 6)])
          (eApp () (eVar (tInt32 ~> tInt32, "h")) [eLit (LInt32 7)])))

fragment16_1 :: Expr Int Int () Void
fragment16_1 =
  eLet
    (1, "id")
    (eLam [(2, "x")] (eVar (3, "x")))
    (eApp 4 (eApp 5 (eVar (6, "id")) [eVar (7, "id")]) [eLit (LInt32 1)])

fragment16_2 :: Expr Type Type () Void
fragment16_2 =
  eLet
    (tVar 2 ~> tVar 2, "id")
    (eLam [(tVar 2, "x")] (eVar (tVar 2, "x")))
    (eApp
       tInt32
       (eApp
          (tInt32 ~> tInt32)
          (eVar ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32, "id"))
          [eVar (tInt32 ~> tInt32, "id")])
       [eLit (LInt32 1)])

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
    (eLam [((), "x")] (eVar ((), "x")))
    (eLet
       ((), "f")
       (eLam
          [((), "x")]
          (eLam [((), "y")] (eOp2 OAddInt32 (eVar ((), "x")) (eVar ((), "y")))))
       (eLet
          ((), "g")
          (eApp () (eVar ((), "f")) [eLit (LInt32 2)])
          (eOp2
             OAddInt32
             (eApp
                ()
                (eApp () (eVar ((), "id")) [eVar ((), "g")])
                [eApp () (eVar ((), "id")) [eLit (LInt32 3)]])
             (eApp () (eVar ((), "f")) [eLit (LInt32 4), eLit (LInt32 5)]))))

fragment17_2 :: Expr Type Type () Void
fragment17_2 =
  eLet
    (tVar 2 ~> tVar 2, "id")
    (eLam [(tVar 2, "x")] (eVar (tVar 2, "x")))
    (eLet
       (tInt32 ~> tInt32 ~> tInt32, "f")
       (eLam
          [(tInt32, "x")]
          (eLam [(tInt32, "y")] (eOp2 OAddInt32 (eVar (tInt32, "x")) (eVar (tInt32, "y")))))
       (eLet
          (tInt32 ~> tInt32, "g")
          (eApp (tInt32 ~> tInt32) (eVar (tInt32 ~> tInt32 ~> tInt32, "f")) [eLit (LInt32 2)])
          (eOp2
             OAddInt32
             (eApp tInt32
                (eApp (tInt32 ~> tInt32) (eVar ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32, "id")) [eVar (tInt32 ~> tInt32, "g")])
                [eApp tInt32 (eVar (tInt32 ~> tInt32, "id")) [eLit (LInt32 3)]])
             (eApp tInt32 (eVar (tInt32 ~> tInt32 ~> tInt32, "f")) [eLit (LInt32 4), eLit (LInt32 5)]))))

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
fragment17_3 :: Expr Type Type () Void
fragment17_3 =
  eLet
    (tVar 2 ~> tVar 2, "id")
    (eLam [(tVar 2, "x")] (eVar (tVar 2, "x")))
    (eLet
       (tInt32 ~> tInt32 ~> tInt32, "f")
       (eLam
          [(tInt32, "x"), (tInt32, "y")] (eOp2 OAddInt32 (eVar (tInt32, "x")) (eVar (tInt32, "y"))))
       (eLet
          (tInt32 ~> tInt32, "g")
          (eApp (tInt32 ~> tInt32) (eVar (tInt32 ~> tInt32 ~> tInt32, "f")) [eLit (LInt32 2)])
          (eOp2
             OAddInt32
             (eApp tInt32
                (eApp (tInt32 ~> tInt32) (eVar ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32, "id")) [eVar (tInt32 ~> tInt32, "g")])
                [eApp tInt32 (eVar (tInt32 ~> tInt32, "id")) [eLit (LInt32 3)]])
             (eApp tInt32 (eVar (tInt32 ~> tInt32 ~> tInt32, "f")) [eLit (LInt32 4), eLit (LInt32 5)]))))

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
    (eLam [(tVar 2, "x")] (eVar (tVar 2, "x")))
    (eLet
       (tInt32 ~> tInt32 ~> tInt32, "f")
       (eLam
          [(tInt32, "x"), (tInt32, "y")] (eOp2 OAddInt32 (eVar (tInt32, "x")) (eVar (tInt32, "y"))))
       (eLet
          (tInt32 ~> tInt32, "g")
          (eLam [(tInt32, ".v0")]
              (eApp tInt32 (eVar (tInt32 ~> tInt32 ~> tInt32, "f")) [eLit (LInt32 2), eVar (tInt32, ".v0")]))
          (eOp2
             OAddInt32
              (eApp tInt32
                (eLam [(tInt32, ".v0")] (eApp tInt32 (eVar ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32, "id")) [eVar (tInt32 ~> tInt32, "g"), eVar (tInt32, ".v0")]))
                [eApp tInt32 (eVar (tInt32 ~> tInt32, "id")) [eLit (LInt32 3)]])
             (eApp tInt32 (eVar (tInt32 ~> tInt32 ~> tInt32, "f")) [eLit (LInt32 4), eLit (LInt32 5)]))))

--
-- .f0(x) = x
-- .f1(x, y) = x + y
-- .f2(.v0) = .f1(2, .v0)
-- .f3(.v0) = .f0(.f2, .v0)
--
-- .f3(.f0(3)) + .f1(4, 5)
--
fragment17_5 :: (Expr Type Type () Void, [(Name, Definition (Label Type) (Expr Type Type () Void))])
fragment17_5 = 
  ( eOp2 OAddInt32 
      (eApp tInt32 (eVar (tInt32 ~> tInt32, ".f3")) [eApp tInt32 (eVar (tInt32 ~> tInt32, ".f0")) [eLit (LInt32 3)]]) 
      (eApp tInt32 (eVar (tInt32 ~> tInt32 ~> tInt32, ".f1")) [eLit (LInt32 4), eLit (LInt32 5)])
  , [ ( ".f0", Function (fromList [(tVar 2, "x")]) (tVar 2, eVar (tVar 2, "x"))
      )
    , ( ".f1", Function (fromList [(tInt32, "x"), (tInt32, "y")]) (tInt32, eOp2 OAddInt32 (eVar (tInt32, "x")) (eVar (tInt32, "y")))
      )
    , ( ".f2", Function (fromList [(tInt32, ".v0")]) (tInt32, eApp tInt32 (eVar (tInt32 ~> tInt32 ~> tInt32, ".f1")) [eLit (LInt32 2), eVar (tInt32, ".v0")])
      )
    , ( ".f3", Function (fromList [(tInt32, ".v0")]) (tInt32, eApp tInt32 (eVar ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32, ".f0")) [eVar (tInt32 ~> tInt32, ".f2"), eVar (tInt32, ".v0")])
      )
    ]
  )

--
-- .f0(x) = x
-- .f1(x, y) = x + y
-- .f2(.v0) = .f1(2, .v0)
-- .f3(.v0) = .g4(.f2, .v0)
-- .g4(x, .v0) = x(.v0)
-- .g5(x) = x
--
-- .f3(.g5(3)) + .f1(4, 5)
--
fragment17_6 :: (Expr Type Type () Void, [(Name, Definition (Label Type) (Expr Type Type () Void))])
fragment17_6 = 
  ( eOp2 OAddInt32 
      (eApp tInt32 (eVar (tInt32 ~> tInt32, ".f3")) [eApp tInt32 (eVar (tInt32 ~> tInt32, ".g5")) [eLit (LInt32 3)]]) 
      (eApp tInt32 (eVar (tInt32 ~> tInt32 ~> tInt32, ".f1")) [eLit (LInt32 4), eLit (LInt32 5)])
  , [ ( ".f0", Function (fromList [(tVar 2, "x")]) (tVar 2, eVar (tVar 2, "x"))
      )
    , ( ".f1", Function (fromList [(tInt32, "x"), (tInt32, "y")]) (tInt32, eOp2 OAddInt32 (eVar (tInt32, "x")) (eVar (tInt32, "y")))
      )
    , ( ".f2", Function (fromList [(tInt32, ".v0")]) (tInt32, eApp tInt32 (eVar (tInt32 ~> tInt32 ~> tInt32, ".f1")) [eLit (LInt32 2), eVar (tInt32, ".v0")])
      )
    , ( ".f3", Function (fromList [(tInt32, ".v0")]) (tInt32, eApp tInt32 (eVar ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32, ".g4")) [eVar (tInt32 ~> tInt32, ".f2"), eVar (tInt32, ".v0")])
      )
    , ( ".g4", Function (fromList [(tInt32 ~> tInt32, "x"), (tInt32, ".v0")]) (tInt32, eApp tInt32 (eVar (tInt32 ~> tInt32, "x")) [eVar (tInt32, ".v0")])
      )
    , ( ".g5", Function (fromList [(tInt32, "x")]) (tInt32, eVar (tInt32, "x"))
      )
    ]
  )


--
-- .f0(x) = x
-- .f1(x, y) = x + y
-- .f2(.v0) = .f1(2, .v0)
-- .f3(.v0) = .h6(.v0)
-- .g4(x, .v0) = x(.v0)
-- .g5(x) = x
-- .h6(.v0) = .f2(.v0)
--
-- .f3(.g5(3)) + .f1(4, 5)
--
fragment17_7 :: (Expr Type Type () Void, [(Name, Definition (Label Type) (Expr Type Type () Void))])
fragment17_7 = 
  ( eOp2 OAddInt32 
      (eApp tInt32 (eVar (tInt32 ~> tInt32, ".f3")) [eApp tInt32 (eVar (tInt32 ~> tInt32, ".g5")) [eLit (LInt32 3)]]) 
      (eApp tInt32 (eVar (tInt32 ~> tInt32 ~> tInt32, ".f1")) [eLit (LInt32 4), eLit (LInt32 5)])
  , [ ( ".f0", Function (fromList [(tVar 2, "x")]) (tVar 2, eVar (tVar 2, "x"))
      )
    , ( ".f1", Function (fromList [(tInt32, "x"), (tInt32, "y")]) (tInt32, eOp2 OAddInt32 (eVar (tInt32, "x")) (eVar (tInt32, "y")))
      )
    , ( ".f2", Function (fromList [(tInt32, ".v0")]) (tInt32, eApp tInt32 (eVar (tInt32 ~> tInt32 ~> tInt32, ".f1")) [eLit (LInt32 2), eVar (tInt32, ".v0")])
      )
    , ( ".f3", Function (fromList [(tInt32, ".v0")]) (tInt32, eApp tInt32 (eVar (tInt32 ~> tInt32, ".h6")) [eVar (tInt32, ".v0")])
      )
    , ( ".g4", Function (fromList [(tInt32 ~> tInt32, "x"), (tInt32, ".v0")]) (tInt32, eApp tInt32 (eVar (tInt32 ~> tInt32, "x")) [eVar (tInt32, ".v0")])
      )
    , ( ".g5", Function (fromList [(tInt32, "x")]) (tInt32, eVar (tInt32, "x"))
      )
    , ( ".h6", Function (fromList [(tInt32, ".v0")]) (tInt32, eApp tInt32 (eVar (tInt32 ~> tInt32, ".f2")) [eVar (tInt32, ".v0")])
      )
    ]
  )


fragment17_8 :: (Expr Type Void () (), [(Name, Definition (Label Type) (Expr Type Void () ()))])
fragment17_8 = 
  ( eOp2 OAddInt32 
      (eCall (tInt32 ~> tInt32, ".f3") [eCall (tInt32 ~> tInt32, ".g5") [eLit (LInt32 3)]]) 
      (eCall (tInt32 ~> tInt32 ~> tInt32, ".f1") [eLit (LInt32 4), eLit (LInt32 5)])
  , [ 
  ( ".f0", Function (fromList [(tVar 2, "x")]) (tVar 2, eVar (tVar 2, "x"))
      )
    , ( ".f1", Function (fromList [(tInt32, "x"), (tInt32, "y")]) (tInt32, eOp2 OAddInt32 (eVar (tInt32, "x")) (eVar (tInt32, "y")))
      )
    , ( ".f2", Function (fromList [(tInt32, ".v0")]) (tInt32, eCall (tInt32 ~> tInt32 ~> tInt32, ".f1") [eLit (LInt32 2), eVar (tInt32, ".v0")])
      )
    , ( ".f3", Function (fromList [(tInt32, ".v0")]) (tInt32, eCall (tInt32 ~> tInt32, ".h6") [eVar (tInt32, ".v0")])
      )
    , ( ".g4", Function (fromList [(tInt32 ~> tInt32, "x"), (tInt32, ".v0")]) (tInt32, eCall (tInt32 ~> tInt32, "x") [eVar (tInt32, ".v0")])
      )
    , ( ".g5", Function (fromList [(tInt32, "x")]) (tInt32, eVar (tInt32, "x"))
      )
    , ( ".h6", Function (fromList [(tInt32, ".v0")]) (tInt32, eCall (tInt32 ~> tInt32, ".f2") [eVar (tInt32, ".v0")])
      )
    ]
  )



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
--        [eLam [(15, "x")] (eVar (16, "x")), eLam [(17, "x")] (eVar (18, "x")), eLit (LInt32 1)])
--i32 :: Type
--i32 = tInt32
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
--input2 = op2 OAddInt32 (var ((), "x")) (var ((), "y"))
--
--input2Typed :: Expr Type () () () Void
--input2Typed = op2 OAddInt32 (var (i32, "x")) (var (i32, "y"))
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
--          (lit (LInt32 3))
--          (lam
--             [(i32, "n")]
--             (op2
--                OAddInt32
--                (op2 OAddInt32 (var (i32, "m")) (var (i32, "n")))
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
--          OAddInt32
--          (op2 OAddInt32 (var (i32, "m")) (var (i32, "n")))
--          (lit (LInt32 3))))
--
--input7 :: Expr Type () () () a3
--input7 =
--  let_
--    (i32, "x")
--    (app (var (i32 ~> i32, "foo")) [var (i32, "x")])
--    (op2 OAddInt32 (var (i32, "x")) (lit (LInt32 3)))
--
--input7NoLetBindings :: Expr Type a0 () () a3
--input7NoLetBindings =
--  app
--    (lam [(i32, "x")] (op2 OAddInt32 (var (i32, "x")) (lit (LInt32 3))))
--    [app (var (i32 ~> i32, "foo")) [var (i32, "x")]]
--
--input8 :: Expr Type a0 () () a3
--input8 =
--  lam
--    [(i32, "m")]
--    (lam
--       [(i32, "n")]
--       (op2
--          OAddInt32
--          (op2 OAddInt32 (var (i32, "m")) (var (i32, "n")))
--          (lit (LInt32 3))))
--
--input8Converted :: Expr Type a0 () () a3
--input8Converted =
--  lam
--    [(i32, "m"), (i32, "n")]
--    (op2
--       OAddInt32
--       (op2 OAddInt32 (var (i32, "m")) (var (i32, "n")))
--       (lit (LInt32 3)))
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
--          [ ("foo", Function (Signature [(i32, "x")] (i32, lit (LInt32 123))))
--          , ("baz", Function (Signature [(i32, "x")] (i32, lit (LInt32 123))))
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
----                   (i32, bOp2 OAddInt32 (bVar "x") (bVar "y"))))
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
--    (lam [(i32, "y")] (op2 OAddInt32 (var (i32, "x")) (var (i32, "y"))))
--
--input15ClosuresConverted :: Expr Type a0 () () a3
--input15ClosuresConverted =
--  lam
--    [(i32, "x")]
--    (app
--       (lam
--          [(i32, "x"), (i32, "y")]
--          (op2 OAddInt32 (var (i32, "x")) (var (i32, "y"))))
--       [var (i32, "x")])
--
---- (\xs : List -> foo(xs))(Cons(5, Nil))
--input16 :: Expr Type () a1 () a3
--input16 =
--  let_
--    (tData "List", "xs")
--    (app
--       (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
--       [lit (LInt32 5), var (tData "List", "Nil")])
--    (app (var (tData "List" ~> tInt32, "foo")) [var (tData "List", "xs")])
--
---- (\xs : List -> foo(xs))(Cons(5, Nil))
--input16NoLetBindings :: Expr Type a0 () () a3
--input16NoLetBindings =
--  app
--    (lam
--       [(tData "List", "xs")]
--       (app (var (tData "List" ~> tInt32, "foo")) [var (tData "List", "xs")]))
--    [ app
--        (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
--        [lit (LInt32 5), var (tData "List", "Nil")]
--    ]
--
--input160 :: [(Name, Definition TypedExpr)]
--input160 =
--  [ ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
--  , ("foo", Function (Signature [] (tInt32, fooAst)))
--  , ("main", Function (Signature [] (tInt32, mainAst)))
--  ]
--  where
--    fooAst = let_ undefined undefined undefined
--    mainAst = undefined -- app (var ((), "foo")) []
--
----        ((), "xs")
----        (app () (var ((), "Cons")) [lit (LInt32 5), app () (var ((), "Nil")) []])
----        (let_
----           ((), "ys")
----           (app () (var ((), "Cons")) [lit (LInt32 5), var ((), "xs")])
----           (case_
----              (var ((), "ys"))
----              [ ([((), "Nil")], lit (LInt32 1))
----              , ( [((), "Cons"), ((), "_"), ((), "zs")]
----                , case_
----                    (var ((), "zs"))
----                    [ ([((), "Nil")], lit (LInt32 2))
----                    , ([((), "Cons"), ((), "_"), ((), "_")], lit (LInt32 3))
----                    ])
----              ]))
--input160Compiled :: [(Name, Definition Ast)]
--input160Compiled =
--  [ ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
--  , ( "def_0"
--    , Function
--        (Signature
--           [ (tInt32 ~> tData "List" ~> tData "List", "Cons")
--           , (tData "List", "xs")
--           ]
--           ( tInt32
--           , call_
--               (undefined, "def_1")
--               [ call_
--                   (undefined, "Cons")
--                   [lit (LInt32 5), var (undefined, "xs")]
--               ])))
--  , ( "def_1"
--    , Function
--        (Signature
--           [(tData "List", "ys")]
--           ( tInt32
--           , case_
--               (var (undefined, "ys"))
--               [ ([(undefined, "Nil")], lit (LInt32 1))
--               , ( [(undefined, "Cons"), (undefined, "_"), (undefined, "zs")]
--                 , case_
--                     (var (undefined, "zs"))
--                     [ ([(undefined, "Nil")], lit (LInt32 2))
--                     , ( [ (undefined, "Cons")
--                         , (undefined, "_")
--                         , (undefined, "_")
--                         ]
--                       , lit (LInt32 3))
--                     ])
--               ])))
--  , ( "foo"
--    , Function
--        (Signature
--           []
--           ( tInt32
--           , call_
--               (undefined, "def_0")
--               [ var (undefined, "Cons")
--               , call_
--                   (undefined, "Cons")
--                   [lit (LInt32 5), call_ (undefined, "Nil") []]
--               ])))
--  , ("main", Function (Signature [] (tInt32, call_ (undefined, "foo") [])))
--  ]
--
--program1 :: [(Name, Definition (SourceExpr ()))]
--program1 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt32] tInt32))
--  , ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tVar 0, tData "List"]])
--  , ( "foo"
--    , Function
--        (Signature
--           [(tUnit, "_")]
--           ( tInt32
--           , let_
--               ((), "foo")
--               (app
--                  (var ((), "Cons"))
--                  [lit (LInt32 5), app (var ((), "Nil")) []])
--               (case_
--                  (var ((), "foo"))
--                  [ ([((), "Cons"), ((), "x"), ((), "xs")], var ((), "x"))
--                  , ([((), "Nil")], lit (LInt32 9))
--                  ]))))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt32
--           , app (var ((), "print_int32")) [app (var ((), "foo")) [lit LUnit]])))
--  ]
--
--program2 :: [(Name, Definition (SourceExpr ()))]
--program2 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt32] tInt32))
--  , ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tVar 0, tData "List"]])
--  , ( "foo"
--    , Function
--        (Signature
--           [(tUnit, "_")]
--           ( tInt32
--           , let_
--               ((), "foo")
--               (app (var ((), "Nil")) [])
--               (case_
--                  (var ((), "foo"))
--                  [ ([((), "Cons"), ((), "x"), ((), "xs")], var ((), "x"))
--                  , ([((), "Nil")], lit (LInt32 9))
--                  ]))))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt32
--           , app (var ((), "print_int32")) [app (var ((), "foo")) [lit LUnit]])))
--  ]
--
--program3 :: [(Name, Definition (SourceExpr ()))]
--program3 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt32] tInt32))
--  , ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tVar 0, tData "List"]])
--  , ( "foo"
--    , Function
--        (Signature
--           [(tUnit, "_")]
--           ( tInt32
--           , let_
--               ((), "foo")
--               (app
--                  (var ((), "Cons"))
--                  [lit (LInt32 5), app (var ((), "Nil")) []])
--               (case_
--                  (var ((), "foo"))
--                  [ ( [((), "Cons"), ((), "x"), ((), "xs")]
--                    , op2 OAddInt32 (var ((), "x")) (lit (LInt32 1)))
--                  , ([((), "Nil")], lit (LInt32 9))
--                  ]))))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt32
--           , app (var ((), "print_int32")) [app (var ((), "foo")) [lit LUnit]])))
--  ]
--
--program4 :: [(Name, Definition (SourceExpr ()))]
--program4 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt32] tInt32))
--  , ( "add"
--    , Function
--        (Signature
--           [(tInt32, "x"), (tInt32, "y")]
--           (tInt32, op2 OAddInt32 (var ((), "x")) (var ((), "y")))))
--  , ( "add5"
--    , Function
--        (Signature [] (tInt32 ~> tInt32, app (var ((), "add")) [lit (LInt32 5)])))
--  , ( "add5_2"
--    , Function
--        (Signature
--           [(tUnit, "_")]
--           (tInt32, app (var ((), "add5")) [lit (LInt32 2)])))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt32
--           , app
--               (var ((), "print_int32"))
--               [app (var ((), "add5_2")) [lit LUnit]])))
--  ]
--
--program5 :: [(Name, Definition (SourceExpr ()))]
--program5 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt32] tInt32))
--  , ( "add"
--    , Function
--        (Signature
--           [(tInt32, "x"), (tInt32, "y")]
--           (tInt32, op2 OAddInt32 (var ((), "x")) (var ((), "y")))))
--  , ( "add55"
--    , Function
--        (Signature
--           []
--           (tInt32 ~> tInt32, app (var ((), "add")) [lit (LInt32 55)])))
--  , ( "foo"
--    , Function
--        (Signature
--           [(tInt32 ~> tInt32, "f")]
--           (tInt32, app (var ((), "f")) [lit (LInt32 2)])))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt32
--           , app
--               (var ((), "print_int32"))
--               [app (var ((), "foo")) [var ((), "add55")]])))
--  ]
--
--program6 :: [(Name, Definition (SourceExpr ()))]
--program6 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt32] tInt32))
--  , ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tVar 0, tData "List"]])
--  , ( "foo"
--    , Function
--        (Signature
--           [(tUnit, "_")]
--           ( tInt32
--           , let_
--               ((), "abc")
--               (app (var ((), "Cons")) [lit (LInt32 5)])
--               (case_
--                  (app (var ((), "abc")) [app (var ((), "Nil")) []])
--                  [ ([((), "Cons"), ((), "x"), ((), "xs")], var ((), "x"))
--                  , ([((), "Nil")], lit (LInt32 9))
--                  ]))))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt32
--           , app (var ((), "print_int32")) [app (var ((), "foo")) [lit LUnit]])))
--  ]
--
--program7 :: [(Name, Definition (SourceExpr ()))]
--program7 =
--  [ ("gc_malloc", External (Signature [tInt64] (tVar 0)))
--  , ("print_int32", External (Signature [tInt32] tInt32))
--  , ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tVar 0, tData "List"]])
--  , ( "foo"
--    , Function
--        (Signature
--
----           [(tUnit, "_")]
--           ( tInt32
--           , let_
--               ((), "abc")
--               (app (var ((), "Cons")) [lit (LInt32 5)])
--               (case_
--                  (app (var ((), "abc")) [var ((), "Nil")])
--                  [ ([((), "Cons"), ((), "x"), ((), "xs")], var ((), "x"))
--                  , ([((), "Nil")], lit (LInt32 9))
--                  ]))))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt32
--           , app (var ((), "print_int32")) [app (var ((), "foo")) [lit LUnit]])))
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
--      (lam [((), "y")] (op2 OAddInt32 (var ((), "y")) (lit (LInt32 1))))
--      (app (app (var ((), "id")) [var ((), "f")]) [app (var ((), "id")) [lit (LInt32 5)]]))
--
--input171 :: TypedExpr
--input171 =
--  let_
--    (tVar 0 ~> tVar 0, "id")
--    (lam [(tVar 0, "x")] (var (tVar 0, "x")))
--    (let_
--      (i32 ~> i32, "f")
--      (lam [(i32, "y")] (op2 OAddInt32 (var (i32, "y")) (lit (LInt32 1))))
--      (app (app (var ((i32 ~> i32) ~> i32 ~> i32, "id")) [var (i32 ~> i32, "f")]) [app (var (i32 ~> i32, "id")) [lit (LInt32 5)]]))
--
