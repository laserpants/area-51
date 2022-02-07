{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Either
import Data.Function (on)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy.IO as Text
import Data.Tuple (swap)
import Data.Void
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
import Pong.LLVM.Emit
import Pong.Lang
import Pong.TypeChecker
import Pong.Util
import qualified Pong.Util.Env as Env

class Iso a b where
  mu :: a -> b
  um :: b -> a

instance Iso (a, (b, c)) (a, b, c) where
  mu (a, (b, c)) = (a, b, c)
  um (a, b, c) = (a, (b, c))

instance Iso ((a, b), c) (a, b, c) where
  mu ((a, b), c) = (a, b, c)
  um (a, b, c) = ((a, b), c)

--instance (Iso a b, Iso b c) => Iso a c where
--  mu = undefined
--  um = undefined
--fmapx :: (Functor f) => (a -> b) -> f a -> f b
--fmapx = 
fooz :: Identity Int
fooz = fmap (+ 1) 5

main :: IO ()
main = do
  putStrLn "hello world"

--foo123 =
--  [ ("abc", External (Signature [(tInt32, "x")] (tInt32, ())))
--  , ("def", Function (Signature [(tInt32, "x")] (tInt32, var (tInt32, "x"))))
--  , ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tOpaque, tData "List"]])
--  ]
--
--foo456 = Program 0 (Map.fromList foo123)
--
--foo789 = buildProgram "Main" foo456
--
--foo999 = Text.putStrLn (ppll foo789)
--
--foo1 =
--  app
--    (app (var (tInt32 ~> tInt32 ~> tInt32, "f")) [lit (LInt32 5)])
--    [lit (LInt32 6)]
--
--foo2 = runCompiler (compileAst foo1) mempty
--
--foo3 =
--  lam
--    [(tInt32, "x"), (tInt32, "y")]
--    (op2 OAddInt32 (var (tInt32, "x")) (var (tInt32, "y")))
--
--foo4 = runCompiler (compileAst foo3) mempty
--
--foo5 = app (var (tInt32 ~> tInt32, "f")) [lit (LInt32 5)]
--
--foo6 = runCompiler (compileAst foo5) mempty
--
--foo9 = runCheck (Env.fromList [("foo", tInt32 ~> tInt32)]) input1
--
--input1 :: Expr () () () () a3
--input1 =
--  let_
--    ((), "f")
--    (var ((), "foo"))
--    (lam [((), "x")] (app (var ((), "f")) [var ((), "x")]))
--
--testProgram :: [(Name, Definition Ast)]
--testProgram =
--  [ ( "fact"
--    , Function
--        (Signature
--           [(tInt32, "n")]
--           ( tInt32
--           , if_
--               (op2 OEqInt32 (var (tInt32, "n")) (lit (LInt32 0)))
--               (lit (LInt32 1))
--               (op2
--                  OMulInt32
--                  (var (tInt32, "n"))
--                  (call_
--                     (tInt32 ~> tInt32, "fact")
--                     [op2 OSubInt32 (var (tInt32, "n")) (lit (LInt32 1))])))))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           (tInt32, call_ (tInt32 ~> tInt32, "fact") [lit (LInt32 5)])))
--  ]
--
----  , ( "f2"
----    , Function
----        ( Signature
----            []
----            ( tInt32 ~> tInt32
----            , var (tInt32 ~> tInt32, "fact")
----            )
----        )
----    )
----  ,
----    ( "f3"
----    , Function
----        ( Signature
----            []
----            ( tInt32
----            , call_ (tInt32 ~> tInt32, "f2") [lit (LInt32 5)]
----            )
----        )
----    )
--foo4562 = Program 0 (Map.fromList testProgram)
--
--foo7892 = buildProgram "Main" foo4562
--
--foo9992 = Text.putStrLn (ppll foo7892)
--
--testProgram2 :: [(Name, Definition TypedExpr)]
--testProgram2 =
--  [ ( "fact"
--    , Function
--        (Signature
--           [(tInt32, "n")]
--           ( tInt32
--           , if_
--               (op2 OEqInt32 (var (tInt32, "n")) (lit (LInt32 0)))
--               (lit (LInt32 1))
--               (op2
--                  OMulInt32
--                  (var (tInt32, "n"))
--                  (app
--                     (var (tInt32 ~> tInt32, "fact"))
--                     [op2 OSubInt32 (var (tInt32, "n")) (lit (LInt32 1))])))))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           (tInt32, app (var (tInt32 ~> tInt32, "fact")) [lit (LInt32 5)])))
--  , ( "f2"
--    , Function
--        (Signature [] (tInt32 ~> tInt32, var (tInt32 ~> tInt32, "fact"))))
--  ]
--
--abc123 = Text.putStrLn (ppll foo)
--  where
--    foo = buildProgram "Main" prog
--    prog = execCompiler (compileDefinitions testProgram2) mempty
--
--testProgram3 :: [(Name, Definition TypedExpr)]
--testProgram3 =
--  [ ( "sum"
--    , Function
--        (Signature
--           [(tInt32, "m"), (tInt32, "n")]
--           (tInt32, op2 OAddInt32 (var (tInt32, "m")) (var (tInt32, "n")))))
--  , ( "plus5"
--    , Function
--        (Signature
--           []
--           ( tInt32 ~> tInt32
--           , app (var (tInt32 ~> tInt32 ~> tInt32, "sum")) [lit (LInt32 5)])))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           (tInt32, app (var (tInt32 ~> tInt32, "plus5")) [lit (LInt32 5)])))
--  ]
--
--abc456 = Text.putStrLn (ppll foo)
--  where
--    foo = buildProgram "Main" prog
--    prog = execCompiler (compileDefinitions testProgram3) mempty
--
--testProgram4 :: [(Name, Definition TypedExpr)]
--testProgram4 =
--  [ ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tOpaque, tData "List"]])
--  , ( "foo"
--    , Function (Signature [(tData "List", "xs")] (tInt32, lit (LInt32 4))))
--        --case_ (var (tData "List", "xs"))
--        --    [ ([(tInt32 ~> tData "List" ~> tData "List", "Cons"), (tInt32, "y"), (tData "List", "ys")], var (tInt32, "y"))
--        --    , ([(tData "List", "Nil")], lit (LInt32 0))
--        --    ])))
--  , ( "xyz"
--    , Function
--        (Signature
--           [(tData "List", "xs")]
--           ( tData "List"
--           , app
--               (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
--               [lit (LInt32 5), var (tData "List", "Nil")])))
--  , ( "main"
--    , Function
--        (Signature
--           []
--           ( tInt32
--           , let_
--               (tData "List", "xs")
--               (app
--                  (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
--                  [lit (LInt32 5), var (tData "List", "Nil")])
--               (app
--                  (var (tData "List" ~> tInt32, "foo"))
--                  [var (tData "List", "xs")]))))
--  ]
--          --(app (var (tInt32 ~> tData "List" ~> tData "List", "Cons")) [lit (LInt32 5), app (var (tData "List", "Nil")) []])
--          --(var (tData "List", "Nil"))
--          --(lit (LInt32 771))
--
----  , ("xs", Function (Signature [] (tData "List", var (tData "List", "Nil"))))
----
----          (var (tData "List", "xyz"))
---- let xs = Cons(5, Nil) in foo(xs)
--testAbc =
--  let_
--    (tData "List", "xs")
--    (app
--       (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
--       [lit (LInt32 5), var (tData "List", "Nil")])
--    (app (var (tData "List" ~> tInt32, "foo")) [var (tData "List", "xs")])
--
---- (\xs : List -> foo(xs))(Cons(5, Nil))
--testAbc2 =
--  app
--    (lam
--       [(tData "List", "xs")]
--       (app (var (tData "List" ~> tInt32, "foo")) [var (tData "List", "xs")]))
--    [ app
--        (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
--        [lit (LInt32 5), var (tData "List", "Nil")]
--    ]
--
----testAbc3 :: TypedExpr
--testAbc3 = runReader (preprocess testAbc2) mempty
--
--testAbc4 :: TypedExpr
--testAbc4 =
--  app
--    (app
--       (lam
--          [(tData "List" ~> tInt32, "foo"), (tData "List", "xs")]
--          (app (var (tData "List" ~> tInt32, "foo")) [var (tData "List", "xs")]))
--       [var (tData "List" ~> tInt32, "foo")])
--    [ app
--        (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
--        [lit (LInt32 5), var (tData "List", "Nil")]
--    ]
--
--testAbc6 :: PreAst
--testAbc6 =
--  app
--    (lam
--       [(tData "List" ~> tInt32, "foo"), (tData "List", "xs")]
--       (app (var (tData "List" ~> tInt32, "foo")) [var (tData "List", "xs")]))
--    [ var (tData "List" ~> tInt32, "foo")
--    , app
--        (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
--        [lit (LInt32 5), var (tData "List", "Nil")]
--    ]
--
--testAbc7 = runCompiler (compileAst testAbc6) mempty
--
--testAbc8 =
--  call_
--    ((tData "List" ~> tInt32) ~> tData "List" ~> tInt32, "def_0")
--    [ var (tData "List" ~> tInt32, "foo")
--    , call_
--        (tInt32 ~> tData "List" ~> tData "List", "Cons")
--        [lit (LInt32 5), var (tData "List", "Nil")]
--    ]
--
----    [("def_0",Function (Signature {arguments = [(Fix (TArr (Fix (TData "List")) (Fix TInt32)),"foo"),(Fix (TData "List"),"xs")], body = (Fix TInt32,Fix (ECall () (Fix (TArr (Fix (TData "List")) (Fix TInt32)),"foo") [Fix (EVar (Fix (TData "List"),"xs"))]))}))]})
----testAbc5 = compileFunction "main" (Signature [] (undefined, testAbc4))
--testAbc5 = Text.putStrLn (ppll foo)
--  where
--    foo = buildProgram "Main" prog
--    prog =
--      toProgram
--        [ ( "List"
--          , Data
--              "List"
--              [Constructor "Nil" [], Constructor "Cons" [tOpaque, tData "List"]])
--        , ( "main"
--          , Function
--              (Signature
--                 []
--                 ( tInt32
--                 , app
--                     (app
--                        (lam  -- \foo xs -> foo(xs)
--                           [ (tData "List" ~> tOpaque, "foo")
--                           , (tData "List", "xs")
--                           ]
--                           (app
--                              (var (tData "List" ~> tInt32, "foo"))
--                              [var (tData "List", "xs")]))
--                        [var (tData "List" ~> tInt32, "foo")])
--                     [ app
--                         (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
--                         [lit (LInt32 5), app (var (tData "List", "Nil")) []] -- var (tData "List", "Nil")]
--                     ] :: TypedExpr)))
--        , ("foo", Function (Signature [(tData "List", "xs")] (tOpaque, 
--                    case_ (var (tData "List", "xs"))
--                    [ ([(tOpaque ~> tData "List" ~> tData "List", "Cons"), (tOpaque, "y"), (tData "List", "ys")], var (tOpaque, "y"))
--                    , ([(tData "List", "Nil")], lit (LInt32 0))
--                    ]
--                                                             )))
--        ]
--
--testProgram5 :: [(Name, Definition Ast)]
--testProgram5 =
--  [ ( "List"
--    , Data
--        "List"
--        [Constructor "Nil" [], Constructor "Cons" [tOpaque, tData "List"]])
--        --case_ (var (tData "List", "xs"))
--        --    [ ([(tInt32 ~> tData "List" ~> tData "List", "Cons"), (tInt32, "y"), (tData "List", "ys")], var (tInt32, "y"))
--        --    , ([(tData "List", "Nil")], lit (LInt32 0))
--        --    ])))
--  , ( "xs"
--    , Function
--        (Signature
--           []
--           ( tData "List"
--           , call_
--               (tInt32 ~> tData "List" ~> tData "List", "Cons")
--               [lit (LInt32 5), var (tData "List", "Nil")])))
--  ]
--
----  , ("foo", Function (Signature [(tData "List", "xs")] (tInt32, 
----        lit (LInt32 4))))
----  , ("main", Function (Signature [] (tInt32, 
----          call_ (tData "List" ~> tInt32, "foo") [var (tData "List", "xs")]
----        )))
--abc789 = Text.putStrLn (ppll foo)
--  where
--    foo = buildProgram "Main" prog
--    prog = toProgram testProgram4 --
------runTestModule :: IO ()
------runTestModule = Text.putStrLn (ppll testModule)
------
------testModule :: LLVM.Module
------testModule =
------  buildProgram "MyModule" $
------  Program 0 $
------  Map.fromList
------    [ ( "List"
------      , Data
------          "List"
------          [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
------    , ( "foo"
------      , Function
------          (Signature
------             []
------             (tData "List", bCall "Cons" [bLit (LInt32 5), bCall "Nil" []])))
------    , ( "foo2"
------      , Function
------          (Signature
------             []
------             ( tInt32
------             , bCase
------                 (bCall "foo" [])
------                 [(["Nil"], bLit (LInt32 0)), (["Cons", "x", "xs"], bVar "x")])))
------    , ("main", Function (Signature [] (tData "List", bCall "foo" [])))
------    , ( "main2"
------      , Function
------          (Signature
------             []
------             ( tInt32
------             , bCall "fun2" [bCall "fun1" [bLit (LInt32 1), bLit (LInt32 2)]])))
------    , ( "fun1"
------      , Function
------          (Signature
------             [(tInt32, "a"), (tInt32, "b"), (tInt32, "c")]
------             (tInt32, bLit (LInt32 123))))
------    , ( "fun2"
------      , Function
------          (Signature
------             [(tInt32 ~> tInt32, "f")]
------             (tInt32, bCall "f" [bLit (LInt32 5)])))
------    ]
------
------testProgram :: [(Name, Definition (Expr ()))]
------testProgram =
------  [ ("fact", Function (Signature [(tInt32, "n")] (tInt32, factExpr)))
------  , ("main", Function (Signature [] (tInt32, mainExpr)))
------  ]
------  where
------    factExpr =
------      if_
------        (op2 OEqInt32 (var ((), "n")) (lit (LInt32 0)))
------        (lit (LInt32 1))
------        (op2
------           OMulInt32
------           (var ((), "n"))
------           (app
------              ()
------              (var ((), "fact"))
------              [op2 OSubInt32 (var ((), "n")) (lit (LInt32 1))]))
------    mainExpr = app () (var ((), "fact")) [lit (LInt32 5)]
------
------testModule3 :: LLVM.Module
------testModule3 = buildProgram "Main" (toProgram testProgram)
------
------runTestModule3 :: IO ()
------runTestModule3 = Text.putStrLn (ppll testModule3)
------
--------
------testProgram2 :: [(Name, Definition (Expr ()))]
------testProgram2 =
------  [ ( "List"
------    , Data
------        "List"
------        [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
------  , ("foo", Function (Signature [] (tInt32, fooExpr)))
------  , ("main", Function (Signature [] (tInt32, mainExpr)))
------  ]
------  where
------    fooExpr =
------      let_
------        ((), "xs")
------        (app () (var ((), "Cons")) [lit (LInt32 5), app () (var ((), "Nil")) []])
------        (let_
------           ((), "ys")
------           (app () (var ((), "Cons")) [lit (LInt32 5), var ((), "xs")])
------           (case_
------              (var ((), "ys"))
------              [ ([((), "Nil")], lit (LInt32 1))
------              , ( [((), "Cons"), ((), "_"), ((), "zs")]
------                , case_
------                    (var ((), "zs"))
------                    [ ([((), "Nil")], lit (LInt32 2))
------                    , ([((), "Cons"), ((), "_"), ((), "_")], lit (LInt32 3))
------                    ])
------              ]))
------    mainExpr = app () (var ((), "foo")) []
------
------testModule4 :: LLVM.Module
------testModule4 = buildProgram "Main" (toProgram testProgram2)
------
------runTestModule4 :: IO ()
------runTestModule4 = Text.putStrLn (ppll testModule4)
------
------z123 :: [(Name, Definition Body)]
------z123 =
------  [ ( "List"
------    , Data
------        "List"
------        [Constructor "Nil" [], Constructor "Cons" [tInt32, tData "List"]])
------  , ( "def_0"
------    , Function
------        (Signature
------           [ (tInt32 `tArr` tData "List" `tArr` tData "List", "Cons")
------           , (tData "List", "xs")
------           ]
------           (tInt32, bCall "def_1" [bCall "Cons" [bLit (LInt32 5), bVar "xs"]])))
------  , ( "def_1"
------    , Function
------        (Signature
------           [(tData "List", "ys")]
------           ( tInt32
------           , bCase
------               (bVar "ys")
------               [ (["Nil"], bLit (LInt32 1))
------               , ( ["Cons", "_", "zs"]
------                 , bCase
------                     (bVar "zs")
------                     [ (["Nil"], bLit (LInt32 2))
------                     , (["Cons", "_", "_"], bLit (LInt32 3))
------                     ])
------               ])))
------  , ( "foo"
------    , Function
------        (Signature
------           []
------           ( tInt32
------           , bCall
------               "def_0"
------               [bVar "Cons", bCall "Cons" [bLit (LInt32 5), bCall "Nil" []]])))
------  , ("main", Function (Signature [] (tInt32, bCall "foo" [])))
------  ]
------
------z125 :: [(Name, Definition (Expr Type))]
------z125 =
------  [ ( "List"
------    , Data
------        "List"
------        [Constructor "Nil" [], Constructor "Cons" [tOpaque, tData "List"]])
------  , ( "foo"
------    , Function
------        (Signature
------           []
------           ( tData "List"
------           , app
------               (tData "List")
------               (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
------               [ lit (LInt32 5)
------               , app (tData "List") (var (tData "List", "Nil")) []
------               ])))
------  , ( "zoo"
------    , Function
------        (Signature
------           []
------           ( tInt32
------           , case_ (var (tData "List", "foo"))
------                [ ([(tInt32 ~> tData "List" ~> tData "List", "Cons"), (tInt32, "x"), (tData "List", "xs")], var (tInt32, "x"))
------                , ([(tData "List", "Nil")], lit (LInt32 0))
------                ])))
------  ]
------
--------  , ( "foo"
--------    , Function
--------        (Signature
--------           []
--------           ( tInt32
--------           , let_
--------               (tData "List", "fs")
--------               (app
--------                  (tData "List")
--------                  (var
--------                     ( (tInt32 ~> tInt32) ~> tData "List" ~> tData "List"
--------                     , "Cons"))
--------                  [ lam
--------                      [(tInt32, "x")]
--------                      (op2 OAddInt32 (var (tInt32, "x")) (lit (LInt32 1)))
--------                  , app (tData "List") (var (tData "List", "Nil")) []
--------                  ])
--------               (let_
--------                  (tData "List", "xs")
--------                  (app
--------                     (tData "List")
--------                     (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
--------                     [ lit (LInt32 5)
--------                     , app
--------                         (tData "List")
--------                         (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
--------                         [ lit (LInt32 7)
--------                         , app (tData "List") (var (tData "List", "Nil")) []
--------                         ]
--------                     ])
--------                  (let_
--------                     (tData "List" ~> tOpaque, "head")
--------                     (lam
--------                        [(tData "List", "xs")]
--------                        (case_
--------                           (var (tData "List", "xs"))
--------                           [ ([(tData "List", "Nil")], lit (LInt32 0))
--------                           , ( [ ( tOpaque ~> tData "List" ~> tData "List"
--------                                 , "Cons")
--------                               , (tOpaque, "y")
--------                               , (tData "List", "ys")
--------                               ]
--------                             , var (tOpaque, "y"))
--------                           ]))
--------                     (app
--------                        tInt32
--------                        (app
--------                           (tInt32 ~> tInt32)
--------                           (var (tData "List" ~> tInt32 ~> tInt32, "head"))
--------                           [var (tData "List", "fs")])
--------                        [ app
--------                            tInt32
--------                            (var (tData "List" ~> tInt32, "head"))
--------                            [var (tData "List", "xs")]
--------                        ]))))))
------z126 :: Program
------z126 = toProgram z125
------
------testModule7 :: LLVM.Module
------testModule7 = buildProgram "Main" (toProgram z125)
------
------runTestModule7 :: IO ()
------runTestModule7 = Text.putStrLn (ppll testModule7)
--
--
--testAbc56 = Text.putStrLn (ppll foo)
--  where
--    foo = buildProgram "Main" prog
--    prog :: Program
--    prog =
--      toProgram
--        (
--        [ ( "List"
--          , Data
--              "List"
--              [Constructor "Nil" [], Constructor "Cons" [tOpaque, tData "List"]])
----        , ( "main"
----          , Function undefined
----          )
----
----        , ( "main"
----          , Function
----              (Signature
----                 []
----                 ( tInt32
----                 , app
----                     (app
----                        (lam  -- \foo xs -> foo(xs)
----                           [ (tData "List" ~> tOpaque, "foo")
----                           , (tData "List", "xs")
----                           ]
----                           (app
----                              (var (tData "List" ~> tInt32, "foo"))
----                              [var (tData "List", "xs")]))
----                        [var (tData "List" ~> tInt32, "foo")])
----                     [ app
----                         (var (tInt32 ~> tData "List" ~> tData "List", "Cons"))
----                         [lit (LInt32 5), app (var (tData "List", "Nil")) []] -- var (tData "List", "Nil")]
----                     ] :: TypedExpr)))
--        , ("foo", Function (Signature [(tData "List", "xs")] (tOpaque, 
--                    case_ (var (tData "List", "xs"))
--                    [ ([(tOpaque ~> tData "List" ~> tData "List", "Cons"), (tOpaque, "y"), (tData "List", "ys")], var (tOpaque, "y"))
--                    , ([(tData "List", "Nil")], lit (LInt32 0))
--                    ]
--                                                             )))
--        ] :: [(Name, Definition Ast)] )
--
--
testAbc5 = Text.putStrLn (ppll foo)
  where
    foo = buildProgram "Main" prog
    prog =
      toProgram
        ([ ( "List"
           , Data
               "List"
               [ Constructor "Nil" []
               , Constructor "Cons" [undefined, tData "List"]
               ])
         , ( "bar"
           , Function (Signature [(tInt32, "n")] (tInt32, var (tInt32, "n"))))
         , ("foo", External (Signature [(tInt32, "x")] (tInt32, ())))
         , ( "boo"
           , Function
               (Signature
                  [(tInt32, "x")]
                  ( tData "List"
                  , app
                      (var (undefined ~> tData "List" ~> tData "List", "Cons"))
                      [lit (LInt32 123), app (var (tData "List", "Nil")) []])))
         , ("baz", Constant (LInt32 1223))
         ] :: [(Name, Definition TypedExpr)])

testAbc77 = do
  withContext (\ctx -> withModuleFromAST ctx module_ (\m -> 
      withHostTargetMachineDefault
        (\machine -> do writeObjectToFile machine (File "./outtmp.o") m)))
  where
    module_ :: LLVM.Module
    module_ = buildProgram "Main" prog
    prog = execCompiler (compileSource ds) (getEnv ds)
    ds =
      [ ("gc_malloc", External (Signature [(tInt64, "0")] (tVar 0, ())))
      , ("print_int32", External (Signature [(tInt32, "0")] (tUnit, ())))
      , ( "List"
        , Data
            "List"
            [Constructor "Nil" [], Constructor "Cons" [tVar 0, tData "List"]])
      , ( "foo"
        , Function
            (Signature
               []
               ( tInt32
               , let_
                   ((), "foo")
                   (app
                      (var ((), "Cons"))
                      [lit (LInt32 5), app (var ((), "Nil")) []])
                   (case_
                      (var ((), "foo"))
                      [ ([((), "Cons"), ((), "x"), ((), "xs")], var ((), "x"))
                      , ([((), "Nil")], lit (LInt32 9))
                      ]))))
      , ( "main"
        , Function
            (Signature
               []
               ( tInt32
               , app (var ((), "print_int32")) [app (var ((), "foo")) []])))
      ] :: [(Name, Definition (SourceExpr ()))]

testAbc7 = Text.putStrLn (ppll foo)
  where
    foo = buildProgram "Main" prog
    prog = execCompiler (compileSource ds) (getEnv ds)
    ds =
      [ ( "List"
        , Data
            "List"
--             [Constructor "Nil" [], Constructor "Cons" [tOpaque, tData "List"]])
            [Constructor "Nil" [], Constructor "Cons" [tVar 0, tData "List"]])
      , ( "zoo"
        , Function
            (Signature
               []
               ( tInt32
               , let_
                   ((), "foo")
                   (app
                      (var ((), "Cons"))
                      [lit (LInt32 5), app (var ((), "Nil")) []])
                   (case_
                      (var ((), "foo"))
                      [ ([((), "Cons"), ((), "x"), ((), "xs")], var ((), "x"))
                      , ([((), "Nil")], lit (LInt32 9))
                      ]))))
--       , ( "zoo"
--         , Function (Signature [] (tInt32, 
--            case_ (var ((), "foo")) 
--              [ ([((), "Cons"), ((), "x"), ((), "xs")], var ((), "x"))
--              , ([((), "Nil")], lit (LInt32 9))
--              ]))
--         )
--       , ( "foo"
--         , Function
--             (Signature
--                []
--                ( tData "List"
--                , app
--                    (var ((), "Cons"))
--                    [lit (LInt32 5), app (var ((), "Nil")) []])))
      ] :: [(Name, Definition (SourceExpr ()))]
--         , ( "bar"
--           , Function (Signature [(tInt32, "n")] (tInt32, var (tInt32, "n"))))
--         , ("foo", External (Signature [(tInt32, "x")] (tInt32, ())))
--         , ("boo", Function (Signature [(tInt32, "x")] (tData "List", 
--              app (var (tOpaque ~> tData "List" ~> tData "List", "Cons")) [lit (LInt32 123), app (var (tData "List", "Nil")) []] )))
--         , ("baz", Constant (LInt32 1223))
--         ] :: [(Name, Definition TypedExpr)])
