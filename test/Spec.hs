{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict ((!))
import Debug.Trace
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
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy.IO as Text
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified Pong.Util.Env as Env

main :: IO ()
main =
  hspec $

    ---------------------------------------------------------------------------
    -- Module Pong ------------------------------------------------------------
    ---------------------------------------------------------------------------
   do
    describe "free" $ do
      describe "Expr" $ do
        runFreeTest "x                                       >>  [x]"     (var (i32, "x")) [(tInt32, "x")]
        runFreeTest "5                                       >>  []"      (lit (LInt32 5) :: Ast) []
        runFreeTest "lam(x) => x                             >>  []"      (lam [((), "x")] (var ((), "x"))) []
        runFreeTest "lam(x) => y                             >>  [y]"     (lam [((), "x")] (var ((), "y"))) [((), "y")]
        runFreeTest "lam(x) => f y                           >>  [f, y]"  (lam [((), "x")] (app (var ((), "f")) [var ((), "y")])) [((), "f"), ((), "y")]
        runFreeTest "lam(x) => f x                           >>  [f]"     (lam [((), "x")] (app (var ((), "f")) [var ((), "x")])) [((), "f")]
        runFreeTest "let f = foo in lam(x) => f x            >>  [foo]"   input1 [((), "foo")]
        runFreeTest "x + y                                   >>  [x, y]"  input2 [((), "x"), ((), "y")]
        runFreeTest "match xs { Cons x ys => x }             >>  [xs]"    input3 [((), "xs")]
        runFreeTest "match xs { Cons x ys => y }             >>  [xs, y]" input4 [((), "xs"), ((), "y")]
        runFreeTest "match xs { Cons x ys => x | Nil => y }  >>  [xs, y]" input5 [((), "xs"), ((), "y")]
        runFreeTest "if x then y else z                      >>  [x, y, z]" (if_ (var ((), "x")) (var ((), "y")) (var ((), "z"))) [((), "x"), ((), "y"), ((), "z")]
        runFreeTest "f(5)                                    >>  f"         (call_ ((), "f") [lit (LInt32 5)]) [((), "f")]
      describe "Signature" $ do runIO $ print "TODO"
    ---------------------------------------------------------------------------
    describe "typeOf" $ do
      describe "Literal" $ do runTypeOfTest "True" (LBool True) tBool
      describe "Op2" $ do
        runTypeOfTest "OEqInt32" OEqInt32 (i32 ~> i32 ~> tBool)
      describe "Ast" $ do runIO $ print "TODO"
      describe "Definition" $ do
        runTypeOfTest
          "#1"
          (Function
             (Signature [(i32, "x"), (tUnit, "y")] (tBool, lit (LBool True))))
          (i32 ~> tUnit ~> tBool)
        runTypeOfTest "#2" (Constant (LBool True)) tBool
    ---------------------------------------------------------------------------
    describe "arity" $ do
      describe "Definition" $ do runIO $ print "TODO"
      describe "Type" $ do
        runArityTest "Int32 ~> Int32 ~> Int32" (i32 ~> i32 ~> i32) 2
        runArityTest "Int32" i32 0
    ---------------------------------------------------------------------------
    describe "isTCon" $ do
      runIsTConTest "i32 ~> i32 == ArrT" (ArrT, i32 ~> i32) True
      runIsTConTest "t          /= ArrT" (ArrT, tVar 0) False
      runIsTConTest "i32        /= VarT" (VarT, i32) False
      runIsTConTest "t          == VarT" (VarT, tVar 0) True
      runIsTConTest "t          /= ArrT" (ArrT, tVar 0) False
    ---------------------------------------------------------------------------
    describe "isCon" $ do
      runIsConTest "x          == Var" (VarE, var ((), "x")) True
      runIsConTest "x          /= Lit" (LitE, var ((), "x")) False
      runIsConTest "()         == Lit" (LitE, lit LUnit) True
      runIsConTest "()         /= Var" (VarE, lit LUnit) False
    ---------------------------------------------------------------------------
    describe "unwindType" $ do
      runUnwindTypeTest "i32" i32 [i32]
      runUnwindTypeTest "i32 ~> i32" (i32 ~> i32) [i32, i32]
      runUnwindTypeTest
        "(i32 ~> i32 ~> i32"
        ((i32 ~> i32) ~> i32)
        [i32 ~> i32, i32]
      runUnwindTypeTest
        "(i32 ~> i32 ~> (i32 ~> i32)"
        ((i32 ~> i32) ~> (i32 ~> i32))
        [i32 ~> i32, i32, i32]
      runUnwindTypeTest
        "(i32 ~> i32 ~> i32 ~> i32"
        ((i32 ~> i32) ~> i32 ~> i32)
        [i32 ~> i32, i32, i32]
    ---------------------------------------------------------------------------
    describe "returnTypeOf" $ do
      runReturnTypeOfTest "Int32 ~> Int32 ~> Bool" (i32 ~> i32 ~> tBool) tBool
      runReturnTypeOfTest "OEqInt32" OEqInt32 tBool
      runReturnTypeOfTest "Bool" tBool tBool
    ---------------------------------------------------------------------------
    describe "foldType" $ do
      runFoldTypeTest
        "(tBool, [i32, tUnit])"
        (tBool, [i32, tUnit])
        (i32 ~> tUnit ~> tBool)
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
        (input1, Env.fromList [("foo", i32 ~> i32)])
        (Right input1Typed)
      runTypeCheckerTest
        "#2"
        (input2, Env.fromList [("x", i32), ("y", i32)])
        (Right input2Typed)
    ---------------------------------------------------------------------------
    -- Module Pong.Compiler ---------------------------------------------------
    ---------------------------------------------------------------------------
    describe "convertLetBindings" $ do
      runConvertLetBindingsTest "#1" input6 input6NoLetBindings
      runConvertLetBindingsTest "#2" input7 input7NoLetBindings
      runConvertLetBindingsTest "#3" input16 input16NoLetBindings
    describe "combineLambdas" $ do
      runCombineLambdasTest "#1" input8 input8Converted
    describe "convertClosures" $ do
      runConvertClosuresTest "#1" input9 input9ClosuresConverted
      runConvertClosuresTest "#2" input15 input15ClosuresConverted
    describe "preprocess" $ do runIO $ print "TODO"
    describe "modifyFunDefs" $ do
      runModifyFunDefsTest1 "#1" input10 ["foo", "baz", "new"]
      runModifyFunDefsTest2 "#2" input10 (lit (LInt32 1))
    describe "uniqueName" $ do runUniqueNameTest "#1"
    describe "compileFunction" $ do runIO $ print "TODO"
--    describe "compileAst" $ do
--      runCompileExpressionTest1 "#1" (input12, input13) (i32 ~> i32 ~> i32)
    describe "lookupFunType" $ do runIO $ print "TODO"
    describe "fillParams" $ do runFillParamsTest "#1" (input12, input13) i32
--    describe "compileProgram" $ runCompileProgramTest "#1" input160 input160Compiled
    ---------------------------------------------------------------------------
    -- Module Pong.LLVM.Emit --------------------------------------------------
    ---------------------------------------------------------------------------
    describe "llvmType" $ do
      runLlvmTypeTest "()         >>      {}" tUnit (LLVM.StructureType False [])
      runLlvmTypeTest "Bool       >>      i1" tBool LLVM.i1
      runLlvmTypeTest "Int32      >>      i32" i32 LLVM.i32
      runLlvmTypeTest "Int64      >>      i64" tInt64 LLVM.i64
      runLlvmTypeTest "Float      >>      float" tFloat LLVM.float
      runLlvmTypeTest "Double     >>      double" tDouble LLVM.double
    describe "emitLit" $ do runIO $ print "TODO"
    describe "emitOp2Instr" $ do runIO $ print "TODO"
    describe "emitBody" $ do runIO $ print "TODO"
    ---------------------------------------------------------------------------
    -- End to end -------------------------------------------------------------
    ---------------------------------------------------------------------------
    describe "End to end" $ do
      runEndToEndCompilerTest "#1" program1 "5\n"
      runEndToEndCompilerTest "#2" program2 "9\n"
