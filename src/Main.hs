{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Either
import Data.Function (on)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy.IO as Text
import Data.Tuple (swap)
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Type as LLVM
import LLVM.IRBuilder
import LLVM.IRBuilder.Module
import LLVM.Pretty
import Pong.Compiler
import Pong.LLVM.Emit
import Pong.Lang
import Pong.TypeChecker
import Pong.Util
import qualified Pong.Util.Env as Env

main :: IO ()
main = do
  putStrLn "hello world"

--foo1 = 
--  app tInt32 (app (tInt32 .-> tInt32) (var (tInt32 .-> tInt32 .-> tInt32, "f")) [lit (LInt32 5)]) [lit (LInt32 6)]
--
--foo2 =
--  runCompiler (compileAst foo1) mempty
--
--foo3 = 
--  lam [(tInt32, "x"), (tInt32, "y")] (op2 OAddInt32 (var (tInt32, "x")) (var (tInt32, "y")))
--
--foo4 =
--  runCompiler (compileAst foo3) mempty
--
--foo5 = 
--  app tInt32 (var (tInt32 .-> tInt32, "f")) [lit (LInt32 5)]
--
--foo6 =
--  runCompiler (compileAst foo5) mempty
--
--
--
----runTestModule :: IO ()
----runTestModule = Text.putStrLn (ppll testModule)
----
----testModule :: LLVM.Module
----testModule =
----  buildProgram "MyModule" $
----  Program 0 $
----  Map.fromList
----    [ ( "List"
----      , Data
----          "List"
----          [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
----    , ( "foo"
----      , Function
----          (Signature
----             []
----             (tData "List", bCall "Cons" [bLit (LInt32 5), bCall "Nil" []])))
----    , ( "foo2"
----      , Function
----          (Signature
----             []
----             ( tInt32
----             , bCase
----                 (bCall "foo" [])
----                 [(["Nil"], bLit (LInt32 0)), (["Cons", "x", "xs"], bVar "x")])))
----    , ("main", Function (Signature [] (tData "List", bCall "foo" [])))
----    , ( "main2"
----      , Function
----          (Signature
----             []
----             ( tInt32
----             , bCall "fun2" [bCall "fun1" [bLit (LInt32 1), bLit (LInt32 2)]])))
----    , ( "fun1"
----      , Function
----          (Signature
----             [(tInt32, "a"), (tInt32, "b"), (tInt32, "c")]
----             (tInt32, bLit (LInt32 123))))
----    , ( "fun2"
----      , Function
----          (Signature
----             [(tInt32 .-> tInt32, "f")]
----             (tInt32, bCall "f" [bLit (LInt32 5)])))
----    ]
----
----testProgram :: [(Name, Definition (Expr ()))]
----testProgram =
----  [ ("fact", Function (Signature [(tInt32, "n")] (tInt32, factExpr)))
----  , ("main", Function (Signature [] (tInt32, mainExpr)))
----  ]
----  where
----    factExpr =
----      if_
----        (op2 OEqInt32 (var ((), "n")) (lit (LInt32 0)))
----        (lit (LInt32 1))
----        (op2
----           OMulInt32
----           (var ((), "n"))
----           (app
----              ()
----              (var ((), "fact"))
----              [op2 OSubInt32 (var ((), "n")) (lit (LInt32 1))]))
----    mainExpr = app () (var ((), "fact")) [lit (LInt32 5)]
----
----testModule3 :: LLVM.Module
----testModule3 = buildProgram "Main" (toProgram testProgram)
----
----runTestModule3 :: IO ()
----runTestModule3 = Text.putStrLn (ppll testModule3)
----
------
----testProgram2 :: [(Name, Definition (Expr ()))]
----testProgram2 =
----  [ ( "List"
----    , Data
----        "List"
----        [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
----  , ("foo", Function (Signature [] (tInt32, fooExpr)))
----  , ("main", Function (Signature [] (tInt32, mainExpr)))
----  ]
----  where
----    fooExpr =
----      let_
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
----    mainExpr = app () (var ((), "foo")) []
----
----testModule4 :: LLVM.Module
----testModule4 = buildProgram "Main" (toProgram testProgram2)
----
----runTestModule4 :: IO ()
----runTestModule4 = Text.putStrLn (ppll testModule4)
----
----z123 :: [(Name, Definition Body)]
----z123 =
----  [ ( "List"
----    , Data
----        "List"
----        [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
----  , ( "def_0"
----    , Function
----        (Signature
----           [ (tInt32 `tArr` tData "List" `tArr` tData "List", "Cons")
----           , (tData "List", "xs")
----           ]
----           (tInt32, bCall "def_1" [bCall "Cons" [bLit (LInt32 5), bVar "xs"]])))
----  , ( "def_1"
----    , Function
----        (Signature
----           [(tData "List", "ys")]
----           ( tInt32
----           , bCase
----               (bVar "ys")
----               [ (["Nil"], bLit (LInt32 1))
----               , ( ["Cons", "_", "zs"]
----                 , bCase
----                     (bVar "zs")
----                     [ (["Nil"], bLit (LInt32 2))
----                     , (["Cons", "_", "_"], bLit (LInt32 3))
----                     ])
----               ])))
----  , ( "foo"
----    , Function
----        (Signature
----           []
----           ( tInt32
----           , bCall
----               "def_0"
----               [bVar "Cons", bCall "Cons" [bLit (LInt32 5), bCall "Nil" []]])))
----  , ("main", Function (Signature [] (tInt32, bCall "foo" [])))
----  ]
----
----z125 :: [(Name, Definition (Expr Type))]
----z125 =
----  [ ( "List"
----    , Data
----        "List"
----        [Constructor "Nil" [], Constructor "Cons" [tOpaque, tData "List"]])
----  , ( "foo"
----    , Function
----        (Signature
----           []
----           ( tData "List"
----           , app
----               (tData "List")
----               (var (tInt32 .-> tData "List" .-> tData "List", "Cons"))
----               [ lit (LInt32 5)
----               , app (tData "List") (var (tData "List", "Nil")) []
----               ])))
----  , ( "zoo"
----    , Function
----        (Signature
----           []
----           ( tInt32
----           , case_ (var (tData "List", "foo"))
----                [ ([(tInt32 .-> tData "List" .-> tData "List", "Cons"), (tInt32, "x"), (tData "List", "xs")], var (tInt32, "x"))
----                , ([(tData "List", "Nil")], lit (LInt32 0))
----                ])))
----  ]
----
------  , ( "foo"
------    , Function
------        (Signature
------           []
------           ( tInt32
------           , let_
------               (tData "List", "fs")
------               (app
------                  (tData "List")
------                  (var
------                     ( (tInt32 .-> tInt32) .-> tData "List" .-> tData "List"
------                     , "Cons"))
------                  [ lam
------                      [(tInt32, "x")]
------                      (op2 OAddInt32 (var (tInt32, "x")) (lit (LInt32 1)))
------                  , app (tData "List") (var (tData "List", "Nil")) []
------                  ])
------               (let_
------                  (tData "List", "xs")
------                  (app
------                     (tData "List")
------                     (var (tInt32 .-> tData "List" .-> tData "List", "Cons"))
------                     [ lit (LInt32 5)
------                     , app
------                         (tData "List")
------                         (var (tInt32 .-> tData "List" .-> tData "List", "Cons"))
------                         [ lit (LInt32 7)
------                         , app (tData "List") (var (tData "List", "Nil")) []
------                         ]
------                     ])
------                  (let_
------                     (tData "List" .-> tOpaque, "head")
------                     (lam
------                        [(tData "List", "xs")]
------                        (case_
------                           (var (tData "List", "xs"))
------                           [ ([(tData "List", "Nil")], lit (LInt32 0))
------                           , ( [ ( tOpaque .-> tData "List" .-> tData "List"
------                                 , "Cons")
------                               , (tOpaque, "y")
------                               , (tData "List", "ys")
------                               ]
------                             , var (tOpaque, "y"))
------                           ]))
------                     (app
------                        tInt32
------                        (app
------                           (tInt32 .-> tInt32)
------                           (var (tData "List" .-> tInt32 .-> tInt32, "head"))
------                           [var (tData "List", "fs")])
------                        [ app
------                            tInt32
------                            (var (tData "List" .-> tInt32, "head"))
------                            [var (tData "List", "xs")]
------                        ]))))))
----z126 :: Program
----z126 = toProgram z125
----
----testModule7 :: LLVM.Module
----testModule7 = buildProgram "Main" (toProgram z125)
----
----runTestModule7 :: IO ()
----runTestModule7 = Text.putStrLn (ppll testModule7)
