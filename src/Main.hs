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

runTestModule :: IO ()
runTestModule = Text.putStrLn (ppll testModule)

testModule :: LLVM.Module
testModule =
  buildProgram "MyModule" $
  Program 0 $
  Map.fromList
    [ ( "List"
      , Data
          "List"
          [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
    , ( "foo"
      , Function
          (Signature
             []
             (tData "List", bCall "Cons" [bLit (LInt32 5), bCall "Nil" []])))
    , ( "foo2"
      , Function
          (Signature
             []
             ( tInt32
             , bCase
                 (bCall "foo" [])
                 [(["Nil"], bLit (LInt32 0)), (["Cons", "x", "xs"], bVar "x")])))
    , ("main", Function (Signature [] (tData "List", bCall "foo" [])))
    , ( "main2"
      , Function
          (Signature
             []
             ( tInt32
             , bCall "fun2" [bCall "fun1" [bLit (LInt32 1), bLit (LInt32 2)]])))
    , ( "fun1"
      , Function
          (Signature
             [(tInt32, "a"), (tInt32, "b"), (tInt32, "c")]
             (tInt32, bLit (LInt32 123))))
    , ( "fun2"
      , Function
          (Signature
             [(tInt32 .-> tInt32, "f")]
             (tInt32, bCall "f" [bLit (LInt32 5)])))
    ]

testProgram :: [(Name, Definition (Ast ()))]
testProgram =
  [ ("fact", Function (Signature [(tInt32, "n")] (tInt32, factExpr)))
  , ("main", Function (Signature [] (tInt32, mainExpr)))
  ]
  where
    factExpr =
      if_
        (op2 OEqInt32 (var ((), "n")) (lit (LInt32 0)))
        (lit (LInt32 1))
        (op2
           OMulInt32
           (var ((), "n"))
           (app () (var ((), "fact")) [op2 OSubInt32 (var ((), "n")) (lit (LInt32 1))]))
    mainExpr = app () (var ((), "fact")) [lit (LInt32 5)]

testModule3 :: LLVM.Module
testModule3 = buildProgram "Main" (compileProgram testProgram)

runTestModule3 :: IO ()
runTestModule3 = Text.putStrLn (ppll testModule3)

--
testProgram2 :: [(Name, Definition (Ast ()))]
testProgram2 =
  [ ( "List"
    , Data
        "List"
        [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
  , ("foo", Function (Signature [] (tInt32, fooExpr)))
  , ("main", Function (Signature [] (tInt32, mainExpr)))
  ]
  where
    fooExpr =
      let_
        ((), "xs")
        (app () (var ((), "Cons")) [lit (LInt32 5), app () (var ((), "Nil")) []])
        (let_
           ((), "ys")
           (app () (var ((), "Cons")) [lit (LInt32 5), var ((), "xs")])
           (case_
              (var ((), "ys"))
              [ ([((), "Nil")], lit (LInt32 1))
              , ( [((), "Cons"), ((), "_"), ((), "zs")]
                , case_
                    (var ((), "zs"))
                    [ ([((), "Nil")], lit (LInt32 2))
                    , ([((), "Cons"), ((), "_"), ((), "_")], lit (LInt32 3))
                    ])
              ]))
    mainExpr = app () (var ((), "foo")) []

testModule4 :: LLVM.Module
testModule4 = buildProgram "Main" (compileProgram testProgram2)

runTestModule4 :: IO ()
runTestModule4 = Text.putStrLn (ppll testModule4)

z123 :: [(Name, Definition Body)]
z123 =
  [ ( "List"
    , Data
        "List"
        [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
  , ( "def_0"
    , Function
        (Signature
           [ (tInt32 `tArr` tData "List" `tArr` tData "List", "Cons")
           , (tData "List", "xs")
           ]
           (tInt32, bCall "def_1" [bCall "Cons" [bLit (LInt32 5), bVar "xs"]])))
  , ( "def_1"
    , Function
        (Signature
           [(tData "List", "ys")]
           ( tInt32
           , bCase
               (bVar "ys")
               [ (["Nil"], bLit (LInt32 1))
               , ( ["Cons", "_", "zs"]
                 , bCase
                     (bVar "zs")
                     [ (["Nil"], bLit (LInt32 2))
                     , (["Cons", "_", "_"], bLit (LInt32 3))
                     ])
               ])))
  , ( "foo"
    , Function
        (Signature
           []
           ( tInt32
           , bCall
               "def_0"
               [bVar "Cons", bCall "Cons" [bLit (LInt32 5), bCall "Nil" []]])))
  , ("main", Function (Signature [] (tInt32, bCall "foo" [])))
  ]
