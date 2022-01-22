{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy.IO as Text
import Debug.Trace
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Type as LLVM
import LLVM.IRBuilder
import LLVM.IRBuilder.Module
import LLVM.Pretty
import Pong.Compiler
import Pong.Data
import Pong.LLVM.Emit
import Pong.Lang
import Pong.Test.Data
import Pong.Test.Drivers
import Pong.TypeChecker
import Pong.Util
import qualified Pong.Util.Env as Env
import Test.Hspec

main :: IO ()
main =
  hspec $
    ---------------------------------------------------------------------------
    -- Module Pong ------------------------------------------------------------
    ---------------------------------------------------------------------------
   do
    describe "free" $ do
      describe "Ast" $ do
        runFreeTest "x ==> [x]" (var (i32, "x")) ["x"]
        runFreeTest "5 ==> []" (lit (LInt32 5) :: Ast ()) []
        runFreeTest "\\x : Int -> x ==> []" (lam [((), "x")] (var ((), "x"))) []
        runFreeTest
          "\\x : Int -> y ==> [y]"
          (lam [((), "x")] (var ((), "y")))
          ["y"]
        runFreeTest
          "\\x : Int -> f y ==> [f, y]"
          (lam [((), "x")] (app () (var ((), "f")) [var ((), "y")]))
          ["f", "y"]
        runFreeTest
          "\\x : Int -> f x ==> [f]"
          (lam [((), "x")] (app () (var ((), "f")) [var ((), "x")]))
          ["f"]
        runFreeTest "let f = foo in \\x : Int -> f x ==> [foo]" input1 ["foo"]
        runFreeTest "x + y ==> [x, y]" input2 ["x", "y"]
        runFreeTest "match xs with | Cons x ys => x ==> [xs]" input3 ["xs"]
        runFreeTest
          "match xs with | Cons x ys => y ==> [xs, ys]"
          input4
          ["xs", "y"]
        runFreeTest
          "match xs with | Cons x ys => x | Nil => y ==> [xs, ys]"
          input5
          ["xs", "y"]
      describe "Body" $ do
        runFreeTest "x ==> [x]" (bVar "x") ["x"]
        runFreeTest "5 ==> []" (bLit (LInt32 5)) []
        runFreeTest
          "if x then y else z ==> [x, y, z]"
          (bIf (bVar "x") (bVar "y") (bVar "z"))
          ["x", "y", "z"]
        runFreeTest "f(5) ==> f" (bCall "f" [bLit (LInt32 5)]) ["f"]
        runFreeTest "match xs with | Cons x ys => x ==> [xs]" input14 ["xs"]
      describe "Signature" $ do runIO $ print "TODO"
    ---------------------------------------------------------------------------
    describe "typeOf" $ do
      describe "Literal" $ do runTypeOfTest "True" (LBool True) tBool
      describe "Op2" $ do
        runTypeOfTest "OEqInt32" OEqInt32 (i32 .-> i32 .-> tBool)
      describe "Expr" $ do runIO $ print "TODO"
      describe "Definition" $ do
        runTypeOfTest
          "#1"
          (Function
             (Signature [(i32, "x"), (tUnit, "y")] (tBool, bLit (LBool True))))
          (i32 .-> tUnit .-> tBool)
        runTypeOfTest "#2" (Constant (LBool True)) tBool
    ---------------------------------------------------------------------------
    describe "arity" $ do
      describe "Definition" $ do runIO $ print "TODO"
      describe "Type" $ do
        runArityTest "Int32 -> Int32 -> Int32" (i32 .-> i32 .-> i32) 2
        runArityTest "Int32" i32 0
    ---------------------------------------------------------------------------
    describe "isTCon" $ do
      runIsTConTest "i32 -> i32 ~ ArrT" (ArrT, i32 .-> i32) True
      runIsTConTest "t /~ ArrT" (ArrT, tVar 0) False
      runIsTConTest "i32 /~ VarT" (VarT, i32) False
      runIsTConTest "t ~ VarT" (VarT, tVar 0) True
      runIsTConTest "t /~ ArrT" (ArrT, tVar 0) False
    ---------------------------------------------------------------------------
    describe "isCon" $ do
      runIsConTest "x ~ Var" (VarE, var ((), "x")) True
      runIsConTest "x /~ Lit" (LitE, var ((), "x")) False
      runIsConTest "() ~ Lit" (LitE, lit LUnit) True
      runIsConTest "() /~ Var" (VarE, lit LUnit) False
    ---------------------------------------------------------------------------
    describe "unwindType" $ do
      runUnwindTypeTest "i32" i32 [i32]
      runUnwindTypeTest "i32 -> i32" (i32 .-> i32) [i32, i32]
      runUnwindTypeTest
        "(i32 -> i32) -> i32"
        ((i32 .-> i32) .-> i32)
        [i32 .-> i32, i32]
      runUnwindTypeTest
        "(i32 -> i32) -> (i32 -> i32)"
        ((i32 .-> i32) .-> (i32 .-> i32))
        [i32 .-> i32, i32, i32]
      runUnwindTypeTest
        "(i32 -> i32) -> i32 -> i32"
        ((i32 .-> i32) .-> i32 .-> i32)
        [i32 .-> i32, i32, i32]
    ---------------------------------------------------------------------------
    describe "returnTypeOf" $ do
      runReturnTypeOfTest "Int32 -> Int32 -> Bool" (i32 .-> i32 .-> tBool) tBool
      runReturnTypeOfTest "OEqInt32" OEqInt32 tBool
      runReturnTypeOfTest "Bool" tBool tBool
    ---------------------------------------------------------------------------
    describe "foldType" $ do
      runFoldTypeTest
        "(tBool, [i32, tUnit])"
        (tBool, [i32, tUnit])
        (i32 .-> tUnit .-> tBool)
      runFoldTypeTest "(i32, [])" (i32, []) i32
    ---------------------------------------------------------------------------
    describe "insertArgs" $ do runIO $ print "TODO"
    ---------------------------------------------------------------------------
    describe "emptyProgram" $ do runIO $ print "TODO"
    ---------------------------------------------------------------------------
    describe "insertDefinition" $ do runIO $ print "TODO"
    ---------------------------------------------------------------------------
    -- Module Pong.TypeChecker ------------------------------------------------
    ---------------------------------------------------------------------------
    describe "runCheck" $ do
      runTypeCheckerTest
        "#1"
        (input1, Env.fromList [("foo", i32 .-> i32)])
        (Right input1Typed)
      runTypeCheckerTest
        "#2"
        (input2, Env.fromList [("x", i32), ("y", i32)])
        (Right input2Typed)
    ---------------------------------------------------------------------------
    -- Module Pong.Compiler ---------------------------------------------------
    ---------------------------------------------------------------------------
    describe "convertLetBindings" $ do
      runConvertLetBindingsTest "#1" input6 input6Converted
      runConvertLetBindingsTest "#2" input7 input7Converted
    describe "combineLambdas" $ do
      runCombineLambdasTest "#1" input8 input8Converted
    describe "convertClosures" $ do
      runConvertClosuresTest "#1" input9 input9Converted
      runConvertClosuresTest "#2" input15 input15Converted
    describe "preprocess" $ do runIO $ print "TODO"
    describe "modifyFunDefs" $ do
      runModifyFunDefsTest1 "#1" input10 ["foo", "baz", "new"]
      runModifyFunDefsTest2 "#2" input10 (bLit (LInt32 1))
    describe "uniqueName" $ do runUniqueNameTest "#1"
    describe "compileFunction" $ do runIO $ print "TODO"
    describe "compileExpr" $ do
      runCompileExpressionTest1 "#1" (input12, input13) (i32 .-> i32 .-> i32)
    describe "lookupFunType" $ do runIO $ print "TODO"
    describe "fillParams" $ do runFillParamsTest "#1" (input12, input13) i32
    describe "compileProgram" $ do
      runCompileProgramTest "#1" input16 input16Compiled
    ---------------------------------------------------------------------------
    -- Module Pong.LLVM.Emit --------------------------------------------------
    ---------------------------------------------------------------------------
    describe "llvmType" $ do
      runLlvmTypeTest "() ==> i1" tUnit LLVM.i1
      runLlvmTypeTest "Bool ==> i1" tBool LLVM.i1
      runLlvmTypeTest "Int32 ==> i32" i32 LLVM.i32
      runLlvmTypeTest "Int64 ==> i64" tInt64 LLVM.i64
      runLlvmTypeTest "Float ==> float" tFloat LLVM.float
      runLlvmTypeTest "Double ==> double" tDouble LLVM.double
    describe "emitLit" $ do runIO $ print "TODO"
    describe "emitOp2Instr" $ do runIO $ print "TODO"
    describe "emitBody" $ do runIO $ print "TODO"
