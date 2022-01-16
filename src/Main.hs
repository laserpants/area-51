{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Control.Monad.Reader
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

--
testProgram :: [(Name, Definition (Ast ()))]
testProgram =
  [ ("fact", Function (Signature [(tInt32, "n")] (tInt32, factExpr)))
  , ("main", Function (Signature [] (tInt32, mainExpr)))
  ]
  where
    factExpr =
      if_
        (op2 OEqInt32 (var () "n") (lit (LInt32 0)))
        (lit (LInt32 1))
        (op2
           OMulInt32
           (var () "n")
           (app () (var () "fact") [op2 OSubInt32 (var () "n") (lit (LInt32 1))]))
    mainExpr = app () (var () "fact") [lit (LInt32 5)]

compileTestProgram :: [(Name, Definition (Ast ()))] -> Program
compileTestProgram ds = execCompiler comp env
  where
    env = Env.fromList (typeOf <$$> ds)
    --
    comp :: Compiler ()
    comp
      | null ls =
        forM_ rs $ \(name, def) ->
          case def of
            Function sig -> compileFunction name sig
            _ -> pure ()
      | otherwise -- /
       = error (show ls)
    -- /
    (ls, rs) =
      let typecheckDef ::
               Definition (Ast ()) -> Definition (Either TypeError Expr)
          typecheckDef def = runCheck (insertArgs (funArgs def) env) <$> def
          partitionDefs =
            partitionEithers . (uncurry (\a -> bimap (a, ) (a, )) <$>)
       in partitionDefs (sequence <$$> second typecheckDef <$> ds)

testModule3 :: LLVM.Module
testModule3 = buildProgram "Main" (compileTestProgram testProgram)

runTestModule3 :: IO ()
runTestModule3 = Text.putStrLn (ppll testModule3)
