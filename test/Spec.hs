{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Data.List.NonEmpty (toList, fromList)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Text.Megaparsec (runParser)
import Control.Newtype.Generics
import Data.Map.Strict (Map, (!))
import Data.Tuple.Extra (swap, second)
import Data.Void
import Debug.Trace
import LLVM.IRBuilder
import LLVM.IRBuilder.Module
import LLVM.Pretty
import Pong.Compiler
import Pong.Data
import Pong.Eval
import Pong.Parser
import Pong.LLVM.Emit
import Pong.Lang
import Pong.Test.Data
import Pong.Test.Drivers
import Pong.TypeChecker
import Pong.Util (Name, (<$$>), cata)
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy.IO as Text
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified Pong.Util.Env as Env

--foo = runWriter (evalStateT (liftLambdas (fillExprParams fragment16_2)) 0)

foo :: Value
foo = ConValue "Cons" [LitValue (PInt 5), ConValue "Nil" []]

fromProgram :: State (Int, Program a1) a2 -> (a2, [(Name, Definition (Label Type) a1)])
fromProgram prog = Map.toList . unpack . snd <$> runState prog (0, emptyProgram)

--fromProgram2 :: State (Program (Expr Type Type () Void)) PreAst -> (PreAst, [(Name, Definition (Label Type) PreAst)])
--fromProgram2 prog = undefined -- Map.toList . unpack <$> runState prog emptyProgram
--  where
--    xx :: (PreAst, Map Name (Definition (Label Type) (Expr Type Type () Void)))
--    xx = unpack <$> runState prog emptyProgram

toProgram :: [(Name, Definition (Label Type) a)] -> Program a
toProgram = Program . Map.fromList

main :: IO ()
main =
  hspec $ do
--    describe "fillParams" $ do
--      it "#1" (fillParams fragment1_0 == fragment1_1)
--      it "#2" (fillParams fragment10_0 == fragment10_1)
    describe "fillExprParams" $ do
      it "#1" (fillExprParams fragment11_1 == fragment11_2)
      it "#2" (fillExprParams fragment12_0 == fragment12_1)
      it "#3" (fillExprParams fragment17_3 == fragment17_4)
--    describe "hoistTopLambdas" $ do
--      it "#1" (hoistTopLambdas fragment2_0 == fragment2_1)
    describe "combineApps" $ do
      it "#1" (combineApps fragment3_0 == fragment3_1)
      it "#2" (marshall fragment3_0 == fragment3_1)
    describe "combineLambdas" $ do
      --it "#1" (combineLambdas fragment4_0 == fragment4_1)
      --it "#2" (combineLambdas fragment17_2 == fragment17_3)
      it "#3" (combineLambdas fragment4_0 == (fragment4_1 :: TypedExpr))
      it "#4" (combineLambdas fragment17_2 == (fragment17_3 :: TypedExpr))
--    describe "convertClosures" $ do
--      it "#1" (convertClosures fragment5_0 == fragment5_1)
--      it "#2" (convertClosures fragment7_0 == fragment7_1)
--      it "#3" (convertClosuresT fragment5_2 == fragment5_3)
--    describe "convertFunApps" $ do
--      it "#1" (convertFunApps fragment6_0 == fragment6_1)
--    describe "liftLambdas" $ do
--      it "#1" (fromProgram (liftLambdas fragment8_0) == fragment8_1)
--      it "#2" (fromProgram (liftLambdas fragment17_4) == fragment17_5)
--      it "#3" (fromProgram (liftLambdas fragment18_2) == fragment18_3)
--    describe "replaceVarLets" $ do
--      it "#1" (fst (replaceVarLets fragment9_0) == fragment9_1)
--      it "#2" (fst (replaceVarLets fragment11_0) == fragment15_1)
--    describe "typeCheck" $ do
--      it "#1" (Right fragment13_1 == runTypeChecker mempty (tagExpr fragment13_0))
--      it "#2" (Right fragment16_2 == runTypeChecker' 8 mempty (applySubstitution =<< check fragment16_1))
--      it "#3" (Right fragment17_2 == runTypeChecker mempty (applySubstitution =<< check =<< tagExpr fragment17_1))
--      it "#4" (Right fragment18_2 == runTypeChecker mempty (applySubstitution =<< check =<< tagExpr fragment18_1))
--      it "#5" (Right fragment21_1 == runTypeChecker mempty (applySubstitution =<< check =<< tagExpr fragment21_0))
--    describe "unify" $ do
--      it "#1" (let Right sub = runUnify fragment14_0 fragment14_1 in apply sub fragment14_0 == fragment14_1)
--      it "#2" (let Right sub = runUnifyRows row_0  row_1 in canonRow (apply sub row_0 :: Row Type Int) == row_1)
--      it "#3" (let Right sub = runUnify type_0 type_1 in canonTypeRows (apply sub type_0 :: Type) == type_1)
--      it "#4" (let Right sub = runUnify type_2 type_3; u = apply sub type_2 :: Type in canonTypeRows u == type_3)
--      it "#5" (let Left e = runUnifyRows row_2 row_3 in UnificationError == e)
--      it "#6" (let Right sub = runUnifyRows row_4 row_5; q = apply sub row_4 :: Row Type Int in canonRow q == row_5)
--      it "#7" (let Right sub = runUnifyRows row_6 row_7; q = apply sub row_6 :: Row Type Int in canonRow q == row_7)
--      it "#8" (let Left e = runUnifyRows row_8 row_9 in UnificationError == e)
--      it "#9" (let Right sub = runUnifyRows row_10 row_11; q = apply sub row_10 :: Row Type Int; s = apply sub row_11 :: Row Type Int in canonRow q == canonRow s)
--      it "#10" (let Right sub = runUnifyRows row_12 row_13; q = apply sub row_13 :: Row Type Int in canonRow q == canonRow row_12)
--      it "#11" (let Left e = runUnifyRows row_14 row_15 in UnificationError == e)
--      it "#12" (let Left e = runUnifyRows row_16 row_17 in UnificationError == e)
--      it "#13" (let Right sub = runUnifyRows row_20 row_21 in canonRow (apply sub row_20 :: Row Type Int) == canonRow (apply sub row_21))
--      it "#14" (let Right sub = runUnifyRows row_22 row_23 in canonRow (apply sub row_22 :: Row Type Int) == canonRow (apply sub row_23))
--      it "#15" (let Right sub = runUnifyRows row_24 row_25 in canonRow (apply sub row_24 :: Row Type Int) == canonRow (apply sub row_25))
--      it "#16" (let Right sub = runUnifyRows row_26 row_27 in canonRow (apply sub row_26 :: Row Type Int) == canonRow (apply sub row_27))
--      it "#17" (let Right sub = runUnifyRows row_28 row_29 in canonRow (apply sub row_28 :: Row Type Int) == canonRow (apply sub row_29))
--      it "#18" (let Left e = runUnifyRows row_30 row_31 in UnificationError == e)
--      it "#19" (let Right sub = runUnifyRows row_32 row_33 in canonRow (apply sub row_32 :: Row Type Int) == canonRow (apply sub row_33))
--      it "#20" (let Right sub = runUnifyRows row_34 row_35 in canonRow (apply sub row_34 :: Row Type Int) == canonRow (apply sub row_35))
--      it "#21" (let Right sub = runUnifyRows row_36 row_37 in canonRow (apply sub row_36 :: Row Type Int) == canonRow (apply sub row_37))
--      it "#22" (let Right sub = runUnifyRows row_38 row_39 in canonRow (apply sub row_38 :: Row Type Int) == canonRow (apply sub row_39))
--      it "#23" (let Right sub = runUnifyRows row_40 row_41 in canonRow (apply sub row_40 :: Row Type Int) == canonRow (apply sub row_41))
--    describe "alignCallSigns" $ do
--      it "#1" (alignCallSigns_ fragment17_5 == fragment17_6)
--    describe "replaceFunArgs" $ do
--      it "#1" (replaceFunArgs_ fragment17_6 == fragment17_7)
--      it "#2" (replaceFunArgs_ fragment19_2 == fragment19_3)
--    describe "convertFunApps" $ do
--      it "#1" (convertFunApps_ fragment17_7 == fragment17_8)
--      it "#2" (convertFunApps_ fragment18_3 == fragment18_4)
--    describe "evalProgram_" $ do
--      it "#1" (evalProgram_ fragment17_8 == LitValue (PInt 14))
--      it "#2" (evalProgram_ fragment18_4 == LitValue (PInt 120))
--      it "#3" (runReader fragment20_2 mempty == LitValue (PInt 100))
--      it "#4" (evalProgram_ fragment20_3 == LitValue (PInt 5))
--      it "#5" (evalProgram_ fragment20_4 == LitValue (PInt 5))
--      it "#6" (evalProgram_ fragment20_5 == LitValue (PInt 5))
--      it "#7" (evalProgram_ fragment20_6 == LitValue (PInt 0))
--      it "#8" (evalProgram_ (fragment21_2, []) == LitValue (PInt 2))
--    describe "parseCompileEval" $ do
--      it "#1" (LitValue (PInt 5) == parseCompileEval "def foo(n : int) : int = 5 def main(a : int) : int = foo(1)")
--      it "#2" (LitValue (PInt 120) == parseCompileEval "def fact(n : int) : int = if n == 0 then 1 else n * fact(n - 1) \ndef main(a : int) : int = fact(5)")
--      it "#3" (LitValue (PInt 120) == parseCompileEval "def fact(n : int) : int = if n == 0 then 1 else n * fact(n - 1) -- This is a comment \ndef main(a : int) : int = fact(5)")
--      it "#4" (LitValue (PInt 2) == parseCompileEval "def main(n : int) : int = let xs = Nil() in match xs { Cons(y, ys) => match ys { Cons(z, zs) => 1 } | Nil => 2 }")
--      it "#5" (LitValue (PInt 2) == parseCompileEval "def main(n : int) : int = let xs = Nil in match xs { Cons(y, ys) => match ys { Cons(z, zs) => 1 } | Nil => 2 }")
--      it "#6" (LitValue (PInt 100) == parseCompileEval "def main(n : int) : int = let xs = Cons(100, Nil()) in match xs { Cons(y, ys) => match ys { Cons(z, zs) => 1 | Nil() => y } | Nil() => 2 }")
--      it "#7" (ConValue "Nil" [] == parseCompileEval "def main(n : int) : List int = let xs = Cons(100, Cons(101, Nil())) in match xs { Cons(y, ys) => match ys { Cons(z, zs) => zs | Nil() => Nil() } | Nil() => Nil() }")
--      it "#8" (ConValue "Nil" [] == parseCompileEval "def main(n : int) : List int = let xs = Cons(100, Cons(101, Nil())) in match xs { | Cons(y, ys) => match ys { | Cons(z, zs) => zs | Nil() => Nil() } | Nil() => Nil() }")
--      it "#9" (ConValue "Cons" [LitValue (PInt 101), ConValue "Nil" []] == parseCompileEval "def main(n : int) : List int = let xs = Cons(100, Cons(101, Nil())) in match xs { Cons(y, ys) => ys | Nil() => Nil() }")
--      it "#10" (LitValue (PInt 101) == parseCompileEval "def main(z : int) : int = let h = z + 1 in let g = lam(x) => x in let f = lam(y) => y + h in g(101)")
--      it "#11" (LitValue (PInt 1) == parseCompileEval "def main(z : int) : int = let f = lam(y) => z in f(1)")
--      it "#12" (LitValue (PInt 10) == parseCompileEval "def main(z : int) : int = let h = z + 1 in let g = lam(x) => x in let f = lam(y) => y + h in f(5) + f(1)")
--      it "#13" (LitValue (PInt 10) == parseCompileEval "def main(z : int) : int = let h = z + 1 in let g = lam(x) => x in let f = lam(y) => y + h in (g(f))(g(5)) + f(1)")
--      it "#14" (LitValue (PInt 12) == parseCompileEval "def main(z : int) : int = let f = lam(x) => lam(y) => lam(z) => x + y + z in let g = f(1) in let h = g(2) in let i = h(3) in i + g(2, 3)")
--
--
--      it "#15" (LitValue (PInt 6) == parseCompileEval "def main(a : int) : int = let r = { price = 5, quantity = 3 } in field { price = p | r } = r in p + 1 }")
--      it "#16" (LitValue (PInt 3) == parseCompileEval "def main(a : int) : int = let r = { price = 5, quantity = 3 } in field { quantity = q | r } = r in q")
--      it "#17" (LitValue (PInt 3) == parseCompileEval "def main(a : int) : int = let r = { price = 5, quantity = 3 } in field { price = p | q } = r in field { quantity = s | o } = q in s")
--      it "#18" (LitValue (PInt 5) == parseCompileEval "def main(a : int) : int = let r = { price = 5, quantity = 3 } in field { quantity = s | q } = r in field { price = p | o } = q in p")
--
--
--      it "#19" (LitValue (PInt 1010) == parseCompileEval "def main(a : int) : int = let r = { price = 5, quantity = 3 } in field { quantity = s | q } = r in field { price = p | o } = q in if o == {} then 1010 else 1011")
--      it "#20" (LitValue (PInt 1) == parseCompileEval "def main(a : int) : int = let q = { quantity = 1 } in let r = { price = 5 | q } in field { quantity = q | a } = r in q")
--      it "#21" (LitValue (PInt 5) == parseCompileEval "def main(a : int) : int = let q = { quantity = 1 } in let r = { price = 5 | q } in field { price = p | a } = r in p }")
--      it "#22" (RowValue rNil == parseCompileEval "def main(a : int) : int = let q = { quantity = 1 } in let r = { price = 5 | q } in field { quantity = q | a } = r in field { price = p | b } = a in b")
--      it "#23" (LitValue (PInt 2) == parseCompileEval "def main(a : int) : int = let q = Cons(1, Cons(2, Nil())) in match q { Nil => 0 | Cons(x, xs) => match xs { Nil => 0 | Cons(y, ys) => y } }")
--      it "#24" (LitValue (PInt 2) == parseCompileEval "def main(a : int) : int = (if 5 == 0 then lam(x) => x else lam(y) => y + 1)(1)")
--      it "#25" (LitValue (PInt 2) == parseCompileEval "def main(a : int) : int = (let f = lam(x) => x + 1 in f)(1)")
--      it "#26" (LitValue (PInt 2) == parseCompileEval "def main(a : int) : int = let r = { a = lam(x) => x + 1 } in (field { a = f | q } = r in f)(1)")
--      it "#27" (LitValue (PInt 2) == parseCompileEval "def main(a : int) : int = let r = { a = lam(x) => x + 1 } in field { a = f | q } = r in f")
--      it "#28" (LitValue (PInt 2) == parseCompileEval "def main(a : int) : int = let xs = Nil() in let f = lam(x) => x + 1 in (match xs { Cons(y, ys) => f | Nil => f })(1)")
--      it "#29" (LitValue (PInt 2) == parseCompileEval "def main(a : int) : int = let r = Cons(lam(x) => x + 1, Nil()) in match r { Cons(f, ys) => f(1)}")


applyToFuns 
  :: (MonadState (Int, Program (Expr Type Type a1 a2)) m) 
  => (Expr Type Type a1 a2 -> m (Expr Type Type a1 a2)) 
  -> m ()
applyToFuns f =
  forEachDef $ \case
    Function as (t, expr) -> do
      e <- f expr
      pure (Function as (t, e))
    def -> pure def

runProgramState :: State (Int, Program a) s -> [(Name, Definition (Label Type) a)] -> (s, [(Name, Definition (Label Type) a)])
runProgramState a p = Map.toList . unpack . snd <$> runState a (0, toProgram p)

alignCallSigns_ :: (PreAst, [(Name, Definition (Label Type) PreAst)]) -> (PreAst, [(Name, Definition (Label Type) PreAst)])
alignCallSigns_ (e, ds) = runProgramState (applyToFuns alignCallSigns >> alignCallSigns e) ds

replaceFunArgs_ :: (PreAst, [(Name, Definition (Label Type) PreAst)]) -> (PreAst, [(Name, Definition (Label Type) PreAst)])
replaceFunArgs_ (e, ds) = runProgramState (applyToFuns replaceFunArgs >> replaceFunArgs e) ds

convertFunApps_ :: (PreAst, [(Name, Definition (Label Type) PreAst)]) -> (Ast, [(Name, Definition (Label Type) Ast)])
convertFunApps_ (e, ds) = (convertFunApps e, fmap convertFunApps <$$> ds)


--preprocess_ :: (TypedExpr, [(Name, Definition (Label Type) TypedExpr)]) -> (PreAst, [(Name, Definition (Label Type) PreAst)])
preprocess_ (e, ds) = (preprocess e, fmap preprocess <$$> ds)


runUnify t1 t2 =  runTypeChecker' (leastFree [t1, t2]) mempty (unify t1 t2)

runUnifyRows r1 r2 =  runTypeChecker' (leastFree [tRow r1, tRow r2]) mempty (unifyRows r1 r2)





--testx444 =
--  runTypeChecker mempty (applySubstitution =<< check =<< tagExpr fragment21_0)











--        succeedUnifyTypes
--            (tRow "name" tString (tRow "id" tInt (tRow "shoeSize" tFloat tRowNil)))
--            (tRow "shoeSize" tFloat (tRow "id" tInt (tRow "name" tString tRowNil)))





--            (tRow "id" tInt (tRow "password" tString (tRow "name" tString tRowNil)))
--            (tRow "id" tInt (tVar kRow "r"))





-- foo(g) =
--   let 
--     f' =
--       f(2)            
--     in
--       g(f')(g(5)) + f'(1)
--

-- foo(g) =
--   let 
--     f' =
--       (lam(v0) => f(2, v0))
--     in
--       g(f')(g(5)) + f'(1)
--

-- f'(v0) = 
--   f(2, v0)
--
-- foo : (a -> a) -> i32
-- foo(g) =
--   g(f')(g(5)) + f'(1)
--

--zzz :: ((t -> t) -> t -> t) -> (t -> t) -> t -> t
--zzz f g b = f (\x -> x) (g b)
--
--test1 = zzz id id 1

--
-- let
--   zzz : ((t -> t) -> t -> t) -> (t -> t) -> t -> t = 
--     lam(f : (t -> t) -> t -> t) =>
--       lam(g : t -> t) =>
--         lam(b : t) =>
--           (f(lam(x : t) => x : t))(g(b : t))
--   in
--     zzz(lam(x : t) => x : t, lam(x : t) => x : t, 1 : i32)


--
-- let
--   zzz : ((t -> t) -> t -> t) -> (t -> t) -> t -> t = 
--     lam[f : (t -> t) -> t -> t, g : t -> t, b : t] =>
--       (f(lam(x : t) => x : t))(g(b : t))
--   in
--     zzz(lam(x : t) => x : t, lam(x : t) => x : t, 1)


--
-- f0(x : t) = x : t
--
-- zzz(f : (t -> t) -> t -> t, g : t -> t, b : t) = f(f0 : t -> t)(g(b : t))
--
-- f1(x) = x
-- f2(x) = x
--
-- zzz(f1, f2, 1)




-- fez : (a -> b -> c) -> b -> c
-- fez(f) = f(5)
--
-- fez : (a -> b -> c) -> b -> c
-- fez(f, x) = f(5, x)
--

--  hspec $
--
--    ---------------------------------------------------------------------------
--    -- Module Pong ------------------------------------------------------------
--    ---------------------------------------------------------------------------
--   do
--    describe "free" $ do
--      describe "Expr" $ do
--        runFreeTest "x                                       >>  [x]"     (var (i32, "x")) [(tInt, "x")]
--        runFreeTest "5                                       >>  []"      (lit (PInt 5) :: Ast) []
--        runFreeTest "lam(x) => x                             >>  []"      (lam [((), "x")] (var ((), "x"))) []
--        runFreeTest "lam(x) => y                             >>  [y]"     (lam [((), "x")] (var ((), "y"))) [((), "y")]
--        runFreeTest "lam(x) => f y                           >>  [f, y]"  (lam [((), "x")] (app (var ((), "f")) [var ((), "y")])) [((), "f"), ((), "y")]
--        runFreeTest "lam(x) => f x                           >>  [f]"     (lam [((), "x")] (app (var ((), "f")) [var ((), "x")])) [((), "f")]
--        runFreeTest "let f = foo in lam(x) => f x            >>  [foo]"   input1 [((), "foo")]
--        runFreeTest "x + y                                   >>  [x, y]"  input2 [((), "x"), ((), "y")]
--        runFreeTest "match xs { Cons x ys => x }             >>  [xs]"    input3 [((), "xs")]
--        runFreeTest "match xs { Cons x ys => y }             >>  [xs, y]" input4 [((), "xs"), ((), "y")]
--        runFreeTest "match xs { Cons x ys => x | Nil => y }  >>  [xs, y]" input5 [((), "xs"), ((), "y")]
--        runFreeTest "if x then y else z                      >>  [x, y, z]" (if_ (var ((), "x")) (var ((), "y")) (var ((), "z"))) [((), "x"), ((), "y"), ((), "z")]
--        runFreeTest "f(5)                                    >>  f"         (call_ ((), "f") [lit (PInt 5)]) [((), "f")]
--      describe "Signature" $ do runIO $ print "TODO"
--    ---------------------------------------------------------------------------
--    describe "typeOf" $ do
--      describe "Prim" $ do runTypeOfTest "True" (PBool True) tBool
--      describe "Op2" $ do
--        runTypeOfTest "OEqInt" OEqInt (i32 ~> i32 ~> tBool)
--      describe "Ast" $ do runIO $ print "TODO"
--      describe "Definition" $ do
--        runTypeOfTest
--          "#1"
--          (Function
--             (Signature [(i32, "x"), (tUnit, "y")] (tBool, lit (PBool True))))
--          (i32 ~> tUnit ~> tBool)
--        runTypeOfTest "#2" (Constant (PBool True)) tBool
--    ---------------------------------------------------------------------------
--    describe "arity" $ do
--      describe "Definition" $ do runIO $ print "TODO"
--      describe "Type" $ do
--        runArityTest "Int ~> Int ~> Int" (i32 ~> i32 ~> i32) 2
--        runArityTest "Int" i32 0
--    ---------------------------------------------------------------------------
--    describe "isTCon" $ do
--      runIsTConTest "i32 ~> i32 == ArrT" (ArrT, i32 ~> i32) True
--      runIsTConTest "t          /= ArrT" (ArrT, tVar 0) False
--      runIsTConTest "i32        /= VarT" (VarT, i32) False
--      runIsTConTest "t          == VarT" (VarT, tVar 0) True
--      runIsTConTest "t          /= ArrT" (ArrT, tVar 0) False
--    ---------------------------------------------------------------------------
--    describe "isCon" $ do
--      runIsConTest "x          == Var" (VarE, var ((), "x")) True
--      runIsConTest "x          /= Lit" (LitE, var ((), "x")) False
--      runIsConTest "()         == Lit" (LitE, lit PUnit) True
--      runIsConTest "()         /= Var" (VarE, lit PUnit) False
--    ---------------------------------------------------------------------------
--    describe "unwindType" $ do
--      runUnwindTypeTest "i32" i32 [i32]
--      runUnwindTypeTest "i32 ~> i32" (i32 ~> i32) [i32, i32]
--      runUnwindTypeTest
--        "(i32 ~> i32) ~> i32"
--        ((i32 ~> i32) ~> i32)
--        [i32 ~> i32, i32]
--      runUnwindTypeTest
--        "(i32 ~> i32) ~> (i32 ~> i32)"
--        ((i32 ~> i32) ~> (i32 ~> i32))
--        [i32 ~> i32, i32, i32]
--      runUnwindTypeTest
--        "(i32 ~> i32) ~> i32 ~> i32"
--        ((i32 ~> i32) ~> i32 ~> i32)
--        [i32 ~> i32, i32, i32]
--    ---------------------------------------------------------------------------
--    describe "returnTypeOf" $ do
--      runReturnTypeOfTest "Int ~> Int ~> Bool" (i32 ~> i32 ~> tBool) tBool
--      runReturnTypeOfTest "OEqInt" OEqInt tBool
--      runReturnTypeOfTest "Bool" tBool tBool
--    ---------------------------------------------------------------------------
--    describe "foldType" $ do
--      runFoldTypeTest
--        "(tBool, [i32, tUnit])"
--        (tBool, [i32, tUnit])
--        (i32 ~> tUnit ~> tBool)
--      runFoldTypeTest "(i32, [])" (i32, []) i32
--    ---------------------------------------------------------------------------
--    describe "insertArgs" $ do runIO $ print "TODO"
--    ---------------------------------------------------------------------------
--    describe "emptyProgram" $ do runIO $ print "TODO"
--    ---------------------------------------------------------------------------
--    describe "insertDefinition" $ do runIO $ print "TODO"
--    ---------------------------------------------------------------------------
--    -- Module Pong.TypeChecker ------------------------------------------------
--    ---------------------------------------------------------------------------
--    describe "runCheck" $ do
--      runTypeCheckerTest
--        "#1"
--        (input1, Env.fromList [("foo", i32 ~> i32)])
--        (Right input1Typed)
--      runTypeCheckerTest
--        "#2"
--        (input2, Env.fromList [("x", i32), ("y", i32)])
--        (Right input2Typed)
--    ---------------------------------------------------------------------------
--    -- Module Pong.Compiler ---------------------------------------------------
--    ---------------------------------------------------------------------------
--    describe "convertLetBindings" $ do
--      runConvertLetBindingsTest "#1" input6 input6NoLetBindings
--      runConvertLetBindingsTest "#2" input7 input7NoLetBindings
--      runConvertLetBindingsTest "#3" input16 input16NoLetBindings
--    describe "combineLambdas" $ do
--      runCombineLambdasTest "#1" input8 input8Converted
--    describe "convertClosures" $ do
--      runConvertClosuresTest "#1" input9 input9ClosuresConverted
--      runConvertClosuresTest "#2" input15 input15ClosuresConverted
--    describe "preprocess" $ do runIO $ print "TODO"
--    describe "modifyFunDefs" $ do
--      runModifyFunDefsTest1 "#1" input10 ["foo", "baz", "new"]
--      runModifyFunDefsTest2 "#2" input10 (lit (PInt 1))
--    describe "uniqueName" $ do runUniqueNameTest "#1"
--    describe "compileFunction" $ do runIO $ print "TODO"
----    describe "compileAst" $ do
----      runCompileExpressionTest1 "#1" (input12, input13) (i32 ~> i32 ~> i32)
--    describe "lookupFunType" $ do runIO $ print "TODO"
--    describe "fillParams" $ do runFillParamsTest "#1" (input12, input13) i32
----    describe "compileProgram" $ runCompileProgramTest "#1" input160 input160Compiled
--    ---------------------------------------------------------------------------
--    -- Module Pong.LLVM.Emit --------------------------------------------------
--    ---------------------------------------------------------------------------
--    describe "llvmType" $ do
--      runLlvmTypeTest "()         >>      {}" tUnit (LLVM.StructureType False [])
--      runLlvmTypeTest "Bool       >>      i1" tBool LLVM.i1
--      runLlvmTypeTest "Int      >>      i32" i32 LLVM.i32
--      runLlvmTypeTest "Int64      >>      i64" tInt64 LLVM.i64
--      runLlvmTypeTest "Float      >>      float" tFloat LLVM.float
--      runLlvmTypeTest "Double     >>      double" tDouble LLVM.double
--    describe "emitLit" $ do runIO $ print "TODO"
--    describe "emitOp2Instr" $ do runIO $ print "TODO"
--    describe "emitBody" $ do runIO $ print "TODO"
--    ---------------------------------------------------------------------------
--    -- End to end -------------------------------------------------------------
--    ---------------------------------------------------------------------------
--    describe "End to end" $ do
--      runEndToEndCompilerTest "#1" program1 "5\n"
--      runEndToEndCompilerTest "#2" program2 "9\n"
--      runEndToEndCompilerTest "#3" program3 "6\n"
--      runEndToEndCompilerTest "#4" program4 "7\n"
--      runEndToEndCompilerTest "#5" program5 "57\n"
--      runEndToEndCompilerTest "#6" program6 "5\n"
--      runEndToEndCompilerTest "#7" program7 "5\n"
--


--compileDef0
--  :: (MonadState (Program PreAst) m) 
--  => Definition (Label Type) TypedExpr 
--  -> m (Definition (Label Type) PreAst)
compileDef0 def = convertClosuresT <$> def

compileDef11
  :: (MonadState (Int, Program PreAst) m) 
  => Definition (Label Type) PreAst
  -> m (Definition (Label Type) PreAst)
compileDef11 d = pure (combineApps <$> d)

compileDef1
  :: (MonadState (Int, Program PreAst) m) 
  => Definition (Label Type) TypedExpr 
  -> m (Definition (Label Type) PreAst)
--compileDef1 def = preprocess (convertClosuresT . combineLambdas <$> def)
compileDef1 def = do
  --traceShowM (convertClosuresT . combineLambdas <$> def)
  preprocess (convertClosuresT . combineLambdas <$> def)

compileDef2 
  :: (MonadState (Int, Program PreAst) m) 
  => Definition (Label Type) PreAst
  -> m (Definition (Label Type) PreAst)
--compileDef2 = traverse (replaceFunArgs <=< alignCallSigns) 
compileDef2 abc = do
  zz <- traverse alignCallSigns abc
  pure zz
--  traceShowM "1 =================="
--  traceShowM zz
--  yy <- traverse replaceFunArgs zz
--  traceShowM "2 =================="
--  traceShowM yy
--  traceShowM "3 =================="
--  pure yy

compileDef 
  :: (MonadState (Int, Program PreAst) m) 
  => Definition (Label Type) TypedExpr 
  -> m (Definition (Label Type) Ast)
compileDef def = do
  d2 <- preprocess (convertClosuresT . combineLambdas <$> def)
  --d3 <- runReaderT (traverse (replaceFunArgs <=< alignCallSigns) d2) 1
  d3 <- traverse (alignCallSigns) d2
  pure (convertFunApps <$> d3)

--overDefs :: (MonadState (Program p) m) => (Definition (Label Type) a -> m (Definition (Label Type) b)) -> Program a -> m (Program b)
overDefs f (Program p) = Program <$> traverse f p

--abcx456 :: (MonadState (Program PreAst) m) => Program TypedExpr -> m (Program Ast)
--abcx456 = overDefs compileDef 

--abcx555 :: Program Ast
--abcx555 = flip evalState emptyProgram $ do
--  overDefs compileDef pirog1

--abcx888 :: [(Name, Definition (Label Type) Ast)]
--abcx888 = let Program p = abcx555 in Map.toList p

--abcx999 :: (Ast, [(Name, Definition (Label Type) Ast)])
--abcx999 = (eCall (tInt ~> tInt, "main") [eLit (PInt 1)], abcx888)

parseCompileEval s =
    evalProgram_ (eCall (tInt ~> tInt, "main") [eLit (PInt 1)], Map.toList r)
  where
    xx1 = snd $ execState (forEachDefX q compileDef1) (0, emptyProgram)
    xx2 = snd $ execState (forEachDefX xx1 compileDef2) (0, xx1)
    Program r = over Program (convertFunApps <$$>) xx2
    q = oiouo p
    Right p = runParser program "" s

-- let
--   f =
--     lam(y) => 
--       z
--   in
--     f(1)

-- let
--   f =
--     (lam[z, y] => z)(z)
--   in
--     f(1)

-- let
--   f =
--     (lam(v0) => (lam[z, y] => z)(z, v0))
--   in
--     f(1)

-- f0(z, y) = z
--
-- let
--   f =
--     (lam(v0) => f0(z, v0))
--   in
--     f(1)

-- f0(z, y) = z
-- f1(v0) = f0(z, v0)
--
-- let
--   in
--     f1(1)


parseCompileEval2 s =
    xx2
    --evalProgram_ (eCall (tInt ~> tInt, "main") [eLit (PInt 1)], Map.toList r)
  where
    gork = zz2 <$> xx0
    Program xx0 = let Program z = q in Program (compileDef0 <$> z) 
--    xx0 = execState (forEachDefX q zz2) emptyProgram
    --xx0 = over Program (zz2 <$$>) r
    xx1 = snd $ execState (forEachDefX q compileDef1) (0, emptyProgram)
    xx11 = snd $ execState (forEachDefX xx1 compileDef11) (0, xx1)
    xx2 = snd $ execState (forEachDefX xx11 compileDef2) (0, xx11)
    Program r = over Program (convertFunApps <$$>) xx2
    q = oiouo p
    Right p = runParser program "" s

    zz2 def = (fillParams (convert <$> def))
  
    convert =
      cata $ \case
        EVar v -> eVar v
        ECon c -> eCon c
        ELit l -> eLit l
        EIf a1 a2 a3 -> eIf a1 a2 a3
        ELam _ a1 a2 -> eLam () a1 a2
        ELet a1 a2 a3 -> eLet a1 a2 a3
        EApp a1 a2 a3 -> eApp a1 a2 a3
        EOp2 op a1 a2 -> eOp2 op a1 a2
        ECase a1 a2 -> eCase a1 a2
        ERow row -> eRow (mapRow convert row)




--pirog0 :: Program Ast
--pirog0 = xx3 -- flip evalState emptyProgram $ do
--  where
----    xx1 :: Program PreAst
--    xx1 = execState (forEachDefX q compileDef1) (0, emptyProgram)
--    xx2 = execState (forEachDefX xx1 compileDef2) (0, xx1)
--    xx3 :: Program Ast
--    xx3 = over Program (convertFunApps <$$>) xx2
--    q = oiouo p
--    --te = programToTypeEnv p
--    --Right p = runParser program "" "def foo(n : int) : int = 5 def main(a : int) : int = foo(1)"
--    --Right p = runParser program "" "def fact(n : int) : int = if n == 0 then 1 else n * fact(n - 1) \ndef main(a : int) : int = fact(5)"
--    --Right p = runParser program "" "def fact(n : int) : int = if n == 0 then 1 else n * fact(n - 1) -- This is a comment \ndef main(a : int) : int = fact(5)"
--    --Right p = runParser program "" "def main(n : int) : int = let xs = Nil() in match xs { Cons(y, ys) => match ys { Cons(z, zs) => 1 } | Nil => 2 }"
--    --Right p = runParser program "" "def main(n : int) : int = let xs = Nil in match xs { Cons(y, ys) => match ys { Cons(z, zs) => 1 } | Nil => 2 }"
--    --Right p = runParser program "" "def main(n : int) : int = let xs = Cons(100, Nil()) in match xs { Cons(y, ys) => match ys { Cons(z, zs) => 1 | Nil() => y } | Nil() => 2 }"
--    --Right p = runParser program "" "def main(n : int) : List int = let xs = Cons(100, Cons(101, Nil())) in match xs { Cons(y, ys) => match ys { Cons(z, zs) => zs | Nil() => Nil() } | Nil() => Nil() }"
--    --Right p = runParser program "" "def main(n : int) : List int = let xs = Cons(100, Cons(101, Nil())) in match xs { | Cons(y, ys) => match ys { | Cons(z, zs) => zs | Nil() => Nil() } | Nil() => Nil() }"
--    --Right p = runParser program "" "def main(n : int) : List int = let xs = Cons(100, Cons(101, Nil())) in match xs { Cons(y, ys) => ys | Nil() => Nil() }"
--    Right p = runParser program "" "def main(z : int) : int = let h = z + 1 in let g = lam(x) => x in let f = lam(y) => y + h in g(101)"


oiouo :: Program SourceExpr -> Program TypedExpr
oiouo p = over Program (rtcx2 <$>) p
  where
    te = Env.inserts [("None", tCon "Option" [tGen 0]), ("Some", tGen 0 ~> tCon "Option" [tGen 0]), ("Nil", tCon "List" [tGen 0]), ("Cons", tGen 0 ~> tCon "List" [tGen 0] ~> tCon "List" [tGen 0])] (programToTypeEnv p)
    rtcx2 :: Definition (Label Type) SourceExpr -> Definition (Label Type) TypedExpr
    rtcx2 (Function args (t, e)) =
              case runTypeChecker (insertArgs (toList args) te) (applySubstitution =<< check =<< tagExpr e) of
                Left err -> error (show err)
                Right r -> Function args (t, r)
    rtcx2 _ = error "TODO"


baz123 e = 
    runTypeChecker te (applySubstitution =<< check =<< tagExpr e)
  where
    te = Env.inserts [("None", tCon "Option" [tGen 0]), ("Some", tGen 0 ~> tCon "Option" [tGen 0]), ("Nil", tCon "List" [tGen 0]), ("Cons", tGen 0 ~> tCon "List" [tGen 0] ~> tCon "List" [tGen 0])] mempty


--runq t = evalProgram_ (eCall (tInt ~> tInt, "main") [eLit (PInt 1)], Map.toList p)
--  where
--    Program p = t

--runp = evalProgram_ (eCall (tInt ~> tInt, "main") [eLit (PInt 1)], Map.toList p)
--  where
--    Program p = pirog0

--[(Name, Definition (Label Type) a)]
--pirog1 :: Program TypedExpr
--pirog1 =
--  toProgram 
--    [ ( "fact", Function (fromList [(tInt, "n")]) 
--            (tInt, 
--              eIf 
--                (eOp2 oEqInt (eVar (tInt, "n")) (eLit (PInt 0)))
--                (eLit (PInt 1))
--                (eOp2 oMulInt
--                    (eVar (tInt, "n"))
--                    (eApp 
--                        tInt
--                        (eVar (tInt ~> tInt, "fact"))
--                        [ eOp2 oSubInt (eVar (tInt, "n")) (eLit (PInt 1)) ]
--                    ))
--            ))
--    , ( "main", Function (fromList [(tInt, "a")]) 
--            (tInt, 
--              eApp
--                tInt
--                (eVar (tInt ~> tInt, "fact"))
--                [eLit (PInt 5)]
--            ))
--    ]


foox123 = runParser expr "" 
    --"(let z = Nil() in (let x = 3 in x)(4))"
    --"let z = Nil() in (let x = 3 in x)(4)"
    "let xs = Nil() in match xs { Cons(y, ys) => match ys { Cons(z, zs) => 1 } | Nil => 2 }"
    --"let xs = Cons(5 , Cons(3, Nil())) in match xs { | Cons(x, ys) => x | Nil => 4 }"
    --"let xs = Cons(5 , Cons(3, Nil)) in 3" -- match xs { | Cons(x, ys) => x | Nil => 4 }"
    --"let xs = \"foo\" in 3" -- match xs { | Cons(x, ys) => x | Nil => 4 }"

foox124 = do
  undefined
  where
    --q = flip runState p $ preprocess (Function (fromList []) (undefined, undefined))
    --p = Program (Map.fromList [ ("foo1", Function (fromList [(undefined, "a")]) (undefined, a2)) ])
    --a3 = preprocess (Function (fromList []) (typeOf a2, a2))
    Right a2 = runTypeChecker (Env.fromList [("Nil", tCon "List" [tGen 0]), ("Cons", tGen 0 ~> tCon "List" [tGen 0] ~> tCon "List" [tGen 0])]) (applySubstitution =<< check =<< tagExpr a1)
    Right a1 = runParser expr "" 
         "let xs = Nil() in match xs { Cons(y, ys) => match ys { Cons(z, zs) => 1 } | Nil => 2 }"

--foox125 
--  :: (MonadState (Program PreAst) m) 
--  => m ()
--foox125 p =
--  undefined

--p444 :: (MonadState (Program PreAst) m) => Program TypedExpr -> m () -- (Program PreAst)
--p444 p = undefined
--  where
--    xx = flip runState p (forEachDef preprocess)

xxx0 :: (a -> b) -> Program a -> Program b
xxx0 f = xxx1 (fmap f)

xxx1 :: (Definition (Label Type) a -> Definition (Label Type) b) -> Program a -> Program b
xxx1 = over Program . fmap 

xxx2 :: (Monad m) => (Definition (Label Type) a -> m (Definition (Label Type) b)) -> Program a -> m (Program b)
xxx2 f (Program p) = Program <$> traverse f p

p123 :: Program TypedExpr
p123 = Program (Map.fromList [("main", Function (fromList [(tInt, "x")]) (typeOf a2, a2))])
    where
    Right a2 = runTypeChecker (Env.fromList [("Nil", tCon "List" [tGen 0]), ("Cons", tGen 0 ~> tCon "List" [tGen 0] ~> tCon "List" [tGen 0])]) (applySubstitution =<< check =<< tagExpr a1)
    Right a1 = runParser expr "" 
         "let xs = Nil() in match xs { Cons(y, ys) => match ys { Cons(z, zs) => 1 } | Nil => 2 }"

--p124 :: Program Ast
--p124 = (`evalState` emptyProgram) abc
--  where
----    xyz = xxx0 convertClosures p123
--
--    abc :: (MonadState (Int, Program PreAst) m) => m (Program Ast)
--    abc = xxx2 (def <=< preprocess) p123
--
--    def :: (MonadState (Int, Program PreAst) m) => Definition (Label Type) PreAst -> m (Definition (Label Type) Ast)
--    def = pure . (convertFunApps <$>)
--
----    ghi :: (MonadState (Program PreAst) m) => Definition (Label Type) PreAst -> m (Definition (Label Type) Ast)
----    ghi = pure . (convertClosures <$>)


--  foox125 p123

--foox124 = do
--  case a2 of
--    Left e -> error (show e)
--    Right te -> do
--      --let a3 = Function (fromList [(tInt, "x")]) (returnType te, te)
--      undefined
----      let a4 = flip runState emptyProgram $ 
----                  preprocess a3
----      --let a5 = fromProgram <$$> a4
----      a4
--  where
--    Right a1 = foox123
--    a2 = runTypeChecker (Env.fromList [("Nil", tCon "List" [tGen 0]), ("Cons", tGen 0 ~> tCon "List" [tGen 0] ~> tCon "List" [tGen 0])]) (applySubstitution =<< check =<< tagExpr a1)



--
-- let
--   g = 
--     lam(x) => 
--       x
--   in
--     let 
--       f =
--         lam(y) => 
--           y + 1
--       in
--         (g(f))(g(5))
--
--
-- let
--   g = 
--     lam(x) => 
--       x
--   in
--     let 
--       f =
--         lam(y) => 
--           y + 1
--       in
--         g(f, g(5))
--
