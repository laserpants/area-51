module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy.IO as Text
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Type as LLVM
import LLVM.IRBuilder
import LLVM.IRBuilder.Module
import LLVM.Pretty
import Pong.Compiler
import Pong.LLVM.Emit
import Pong.Lang

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
      , Data [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
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
