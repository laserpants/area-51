module Pong.TestHelpers where

import Pong.Data
import Pong.Lang
import Pong.Tree
import Pong.Type
import Pong.Util
import System.Directory
import System.IO.Unsafe

typeCheck :: TypeChecker a -> Either TypeError a
typeCheck = evalTypeChecker 1 mempty

runUnify :: MonoType -> MonoType -> Either TypeError Substitution
runUnify t1 t2 = evalTypeChecker (freeIndex [t1, t2]) mempty (unifyTypes t1 t2)

runUnifyRows :: Row MonoType Int -> Row MonoType Int -> Either TypeError Substitution
runUnifyRows r1 r2 = evalTypeChecker (freeIndex [tRec r1, tRec r2]) mempty (unifyRows r1 r2)

testHoistProgram :: Program MonoType TypedExpr -> Program MonoType TypedExpr
testHoistProgram p = programFor p (const hoistTopLambdas)

testMonomorphizeProgram :: Program MonoType TypedExpr -> Program MonoType TypedExpr
testMonomorphizeProgram p = runTransform (programForM p (const (traverse monomorphizeLets)))

{-# NOINLINE projectDir #-}
projectDir :: String
projectDir = unsafePerformIO getCurrentDirectory

unsafeReadFile :: String -> Text
unsafeReadFile = pack . unsafePerformIO . readFile

readProjectFile :: String -> Text
readProjectFile path = unsafeReadFile (projectDir <> "/" <> path)

mainSig :: Label Scheme 
mainSig = (Scheme (tUnit ~> tInt), "main")
