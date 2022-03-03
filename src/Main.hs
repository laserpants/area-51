{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Either
import Data.Function (on)
import Data.List.NonEmpty hiding ((!!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy.IO as Text
import Data.Tuple (swap)
import Data.Tuple.Extra (snd3)
import Data.Void
import Pong.Eval
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Type as LLVM
import LLVM.Context (withContext)
import LLVM.IRBuilder
import LLVM.IRBuilder.Module
import LLVM.Linking (loadLibraryPermanently)
import LLVM.Module (File(..), withModuleFromAST, writeObjectToFile)
import LLVM.Pretty
import LLVM.Target (withHostTargetMachineDefault, withTargetOptions)
import Pong.Compiler
import Pong.Data
import Pong.LLVM.Emit
import Pong.Lang
import Pong.TypeChecker
import Pong.Util
import qualified Pong.Util.Env as Env
import System.IO.Temp
import System.Process

main :: IO ()
main = do
  putStrLn "hello world"

-- ==========================
-- ==========================
-- boo =
--   lam(a) => lam(b) => b
--
-- plus(x, y) =
--   x + y
--
-- baz(x)
--   = plus(x)
--
-- foo(z) =
--   let
--     h =
--       z + 1
--     in
--       let
--         g =
--           lam(x) => x
--         in
--           let
--             f =
--               lam(y) => y + h
--             in
--               g(f)(g(5)) + f(1)
--
prog1_0 =
  [ ( "boo"
    , Constant
        ( tGen 0 ~> tGen 1 ~> tGen 1
        , eLam [(tGen 0, "a")] (eLam [(tGen 1, "b")] (eVar (tGen 1, "b")))))
  , ( "plus"
    , Function
        (fromList [(tInt32, "x"), (tInt32, "y")])
        (tInt32, eOp2 OAddInt32 (eVar (tInt32, "x")) (eVar (tInt32, "y"))))
  , ( "baz"
    , Function
        (fromList [(tInt32, "x")])
        ( tInt32 ~> tInt32
        , eApp (eVar (tInt32 ~> tInt32 ~> tInt32, "plus")) [eVar (tInt32, "x")]))
  , ( "foo"
    , Function
        (fromList [(tInt32, "z")])
        ( tInt32
        , eLet
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
                        (eApp
                           (eVar ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32, "g"))
                           [eVar (tInt32 ~> tInt32, "f")])
                        [eApp (eVar (tInt32 ~> tInt32, "g")) [eLit (LInt32 5)]])
                     (eApp (eVar (tInt32 ~> tInt32, "f")) [eLit (LInt32 1)]))))))
  ]

-- Step #2.
--
-- * combine lambdas 
-- * convert closures
-- * fill arguments (no functions returning functions)
-- ==========================
-- boo =
--   lam[a, b] => b
--
-- plus(x, y) =
--   x + y
--
-- baz(x, v_0)
--   = plus(x, v_0)
--
-- foo(z) =
--   let
--     h =
--       z + 1
--     in
--       let
--         g =
--           lam(x) => x
--         in
--           let
--             f =
--               (lam(a_0, y) => y + a_0)(h)
--             in
--               g(f)(g(5)) + f(1)
--
prog1_1 =
  [ ( "boo"
    , Constant
        ( tGen 0 ~> tGen 1 ~> tGen 1
        , eLam [(tGen 0, "a"), (tGen 1, "b")] (eVar (tGen 1, "b"))))
  , ("plus", fromJust (lookup "plus" prog1_0))
  , ( "baz"
    , Function
        (fromList [(tInt32, "x"), (tInt32, "_v0")])
        ( tInt32
        , eApp
            (eVar (tInt32 ~> tInt32 ~> tInt32, "plus"))
            [eVar (tInt32, "x"), eVar (tInt32, "_v0")]))
  , ( "foo"
    , Function
        (fromList [(tInt32, "z")])
        ( tInt32
        , eLet
            (tInt32, "h")
            (eOp2 OAddInt32 (eVar (tInt32, "z")) (eLit (LInt32 1)))
            (eLet
               (tVar 0 ~> tVar 0, "g")
               (eLam [(tVar 0, "x")] (eVar (tVar 0, "x")))
               (eLet
                  (tInt32 ~> tInt32, "f")
                  (eApp
                     (eLam
                        [(tInt32, "a_0"), (tInt32, "y")]
                        (eOp2
                           OAddInt32
                           (eVar (tInt32, "y"))
                           (eVar (tInt32, "a_0"))))
                     [eVar (tInt32, "h")])
                  (eOp2
                     OAddInt32
                     (eApp
                        (eApp
                           (eVar ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32, "g"))
                           [eVar (tInt32 ~> tInt32, "f")])
                        [eApp (eVar (tInt32 ~> tInt32, "g")) [eLit (LInt32 5)]])
                     (eApp (eVar (tInt32 ~> tInt32, "f")) [eLit (LInt32 1)]))))))
  ]

-- Step #3.
--
-- * lift lambdas
-- ==========================
-- boo(a, b) =
--   b
--
-- plus(x, y) = x + y
--
-- baz(x, v_0) = plus(x, v_0)
--
-- g(x) = x
--
-- f(a_0, y) = y + a_0
--
-- foo(z) =
--   let
--     h =
--       z + 1
--     in
--       let 
--         f' =
--           ^f(h)                    { f, apply_f, h }
--         in
--           g(f')(g(5)) + f'(1)
prog1_2 =
  [ ( "boo"
    , Function
        (fromList [(tGen 0, "a"), (tGen 1, "b")])
        (tGen 1, eVar (tGen 1, "b")))
  , ("plus", fromJust (lookup "plus" prog1_1))
  , ("baz", fromJust (lookup "baz" prog1_1))
  , ("g", Function (fromList [(tGen 0, "x")]) (tGen 0, eVar (tGen 0, "x")))
  , ( "f"
    , Function
        (fromList [(tInt32, "a_0"), (tInt32, "y")])
        (tInt32, eOp2 OAddInt32 (eVar (tInt32, "y")) (eVar (tInt32, "a_0"))))
  , ( "foo"
    , Function
        (fromList [(tInt32, "z")])
        ( tInt32
        , eLet
            (tInt32, "h")
            (eOp2 OAddInt32 (eVar (tInt32, "z")) (eLit (LInt32 1)))
            (eLet
               (tInt32 ~> tInt32, "f'")
               (eApp
                  (eVar (tInt32 ~> tInt32 ~> tInt32, "f"))
                  [eVar (tInt32, "h")])
               (eOp2
                  OAddInt32
                  (eApp
                     (eApp
                        (eVar ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32, "g"))
                        [eVar (tInt32 ~> tInt32, "f'")])
                     [eApp (eVar (tInt32 ~> tInt32, "g")) [eLit (LInt32 5)]])
                  (eApp (eVar (tInt32 ~> tInt32, "f'")) [eLit (LInt32 1)])))))
  ]

-- Step #4.
--
-- * elim. partial function applications
-- ==========================
-- plus(x, y) = x + y
--
-- baz(x, v_0) = plus(x, v_0)
--
-- g(x) = x
--
-- f(a_0, y) = y + a_0
--
-- foo(z) =
--   let
--     h =
--       z + 1
--     in
--       f(h)(g(5)) + f(h, 1)  ===>   f(h, g(5)) + f(h, 1)
prog1_3 =
  [ ("boo", fromJust (lookup "boo" prog1_2))
  , ("plus", fromJust (lookup "plus" prog1_2))
  , ("baz", fromJust (lookup "baz" prog1_2))
  , ("g", fromJust (lookup "g" prog1_2))
  , ("f", fromJust (lookup "f" prog1_2))
  , ( "foo"
    , Function
        (fromList [(tInt32, "z")])
        ( tInt32
        , eLet
            (tInt32, "h")
            (eOp2 OAddInt32 (eVar (tInt32, "z")) (eLit (LInt32 1)))
            (eOp2
               OAddInt32
               (eApp
                  (eVar (tInt32 ~> tInt32 ~> tInt32, "f"))
                  [ eVar (tInt32, "h")
                  , eApp (eVar (tInt32 ~> tInt32, "g")) [eLit (LInt32 5)]
                  ])
               (eApp
                  (eVar (tInt32 ~> tInt32 ~> tInt32, "f"))
                  [eVar (tInt32, "h"), eLit (LInt32 1)]))))
  ]

--
-- type List a = Nil | Cons a (List a)
--
-- 
--
prog2_0 = [("List", undefined)]

-- Step #1.
-- 
-- * Lay out constructors 
-- ==========================
--
-- type List a = Nil | Cons a (List a)
--
--
-- Nil = Nil
-- Cons(x, xs) = Cons(x, xs)
-- 
--
prog2_1 = undefined

-- Rows!
--
-- foo(_) =
--   let
--     r =
--       { a = 1, b = "woo", a = True }
--     in
--       match r {
--         | { a = x | r1 } =>
--           match r1 with {
--             | { a = x1 | r2 } =>
--               match r2 with {
--                 | { a = x2 | r3 } => x2
--               }
--           }
--       }
prog3_0 = undefined
