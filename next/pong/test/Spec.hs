{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Arrow ((>>>))
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Char (isUpper)
import Data.Fix
import Data.Foldable
import Data.Function (on)
import Data.Functor
import Data.Int (Int32, Int64)
import Data.List (sortBy, sortOn, groupBy, intersperse)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Map.Strict ((!))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Tuple.Extra
import Debug.Trace
import Pong
import System.Process
import Test.Hspec
import Text.Megaparsec (runParser)
import TextShow
import qualified Control.Monad.Free as Free
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

testTypeEnv :: TypeEnv
testTypeEnv = envFromList
  [ ("Cons", scheme [0] (TVar 0 ~> TCon "List" [TVar 0] ~> TCon "List" [TVar 0]))
  , ("Nil", scheme [0] (TCon "List" [TVar 0]))

  , ("Record#", scheme [0] (TVar 0 ~> TCon "Record#" [TVar 0]))

--  , ("&Cons", scheme [0] (TVar 0))
  , ("!print_int32", scheme [0] (tInt32 ~> TCon "Unit" []))
  , ("!print_int64", scheme [0] (tInt64 ~> TCon "Unit" []))
  , ("!print_string", scheme [0] (tString ~> TCon "Unit" []))
  , ("!increment", scheme [0] (tInt32 ~> tInt32))
  ]

maxStrLen :: Int
maxStrLen = 80

ePat :: Expr t -> [Clause t] -> Expr t
ePat e =
  \case
    c:cs -> EPat e (c:|cs)
    [] -> error "Empty list"

eLet :: [(Label t, Expr t)] -> Expr t -> Expr t
eLet vs e =
  case vs of
    a:as -> ELet (a:|as) e
    [] -> error "Empty list"

eApp :: t -> Expr t -> [Expr t] -> Expr t
eApp t e =
  \case
    a:as -> EApp t e (a:|as)
    [] -> e

eLam :: [Label t] -> Expr t -> Expr t
eLam lls e =
  case lls of
    a:as -> ELam (a:|as) e
    [] -> error "Empty list"

clause :: [Label t] -> Expr t -> Clause t
clause lls e =
  case lls of
    a:as -> Clause (a:|as) e
    [] -> error "Empty list"

trimStr :: String -> String
trimStr str
  | length str < maxStrLen = str
  | otherwise = take maxStrLen str <> " ..."

tvarsShouldEqual :: (Show a, Substitutable a) => a -> Set Int -> SpecWith ()
tvarsShouldEqual input result = it (trimStr is) (tvars input == result)
  where
    is = "tvars (" <> show input <> ") == " <> show result

testTvars :: SpecWith ()
testTvars = do
  describe "tvars" $ do
    describe "Type" $ do
      tvarsShouldEqual tUnit mempty
      tvarsShouldEqual tBool mempty
      tvarsShouldEqual tInt32 mempty
      tvarsShouldEqual tFloat mempty
      tvarsShouldEqual tDouble mempty
      tvarsShouldEqual tChar mempty
      tvarsShouldEqual tString mempty
      tvarsShouldEqual (TVar 0) (Set.singleton 0)
      tvarsShouldEqual (TCon "List" [tUnit]) mempty
      tvarsShouldEqual (TCon "List" [TVar 0]) (Set.singleton 0)
      tvarsShouldEqual (TCon "List" [TCon "List" [TVar 0]]) (Set.singleton 0)
      tvarsShouldEqual (tArr tInt32 tInt32) mempty
      tvarsShouldEqual (tArr (TCon "List" [TVar 0]) tInt32) (Set.singleton 0)
      tvarsShouldEqual (tArr (TVar 0) (TVar 0)) (Set.singleton 0)
      tvarsShouldEqual (tArr (TVar 0) (TVar 1)) (Set.fromList [0, 1])
      tvarsShouldEqual (tArr tInt32 (TVar 0)) (Set.singleton 0)
    describe "Expr Type" $ do
      tvarsShouldEqual (EVar (Label tChar "x")) mempty
      tvarsShouldEqual (EVar (Label (TVar 0) "x")) (Set.singleton 0)
      tvarsShouldEqual (ELit (PInt32 42) :: Expr Type) mempty
      tvarsShouldEqual (ePat (EVar (Label (TVar 0) "x")) [clause [Label (TVar 1) "Cons", Label (TVar 2) "x"] (ELit (PInt32 1)), clause [Label (TVar 3) "Nil"] (ELit (PInt32 2))]) (Set.fromList [0, 1, 2, 3])

--

freeShouldEqual :: Expr () -> Set ((), Name) -> SpecWith ()
freeShouldEqual input result =
  it (trimStr is) (free input == result)
  where
    is = "free (" <> show input <> ") == " <> show result

testFreeVars :: SpecWith ()
testFreeVars =
  describe "free" $ do
    mapM_ (uncurry freeShouldEqual)
      [
        ( EVar (Label () "v")
        , Set.fromList [((), "v")]
        )
      , ( eLet [(Label () "x", ELit (PInt32 1))] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y")))
        , Set.fromList [((), "y")]
        )
      , ( eLet [(Label () "z", ELit (PInt32 1))] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y")))
        , Set.fromList [((), "x"), ((), "y")]
        )
      , ( eLam [Label () "x"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y")))
        , Set.fromList [((), "y")]
        )
      , ( eLam [Label () "x", Label () "y"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y")))
        , mempty
        )
      , ( -- let f = lam(x) => x + y in 1
          eLet [(Label () "f", eLam [Label () "x"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y"))))] (ELit (PInt32 1))
        , Set.fromList [((), "y")]
        )
      , ( -- let f = lam(x) => x + y in p
          eLet [(Label () "f", eLam [Label () "x"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y"))))] (EVar (Label () "p"))
        , Set.fromList [((), "y"), ((), "p")]
        )
      ]

--

evalTagExpr :: Expr () -> Expr Int
evalTagExpr input =
  case runInfer testTypeEnv (tagExpr input) of
    Left{}          -> error "Implementation error"
    Right (expr, _) -> expr

tagExprShouldEqual :: Expr () -> Expr Int -> SpecWith ()
tagExprShouldEqual input result =
  it (trimStr is) (evalTagExpr input == result)
  where
    is = "tagExpr (" <> show input <> ") == " <> show result

testTagExpr :: SpecWith ()
testTagExpr =
  describe "tagExpr" $ do
    mapM_ (uncurry tagExprShouldEqual)
      [ -- x
        ( EVar (Label () "x")
        , EVar (Label 0 "x")
        )
      , -- let x = 5 in x + 1
        ( eLet [(Label () "x", ELit (PInt32 5))] (EOp2 ((), OAdd) (EVar (Label () "x")) (ELit (PInt32 1)))
        , eLet [(Label 0 "x", ELit (PInt32 5))] (EOp2 (1, OAdd) (EVar (Label 2 "x")) (ELit (PInt32 1)))
        )
      , -- lam(x) => not(x)
        ( eLam [Label () "x"] (EOp1 ((), ONot) (EVar (Label () "x")))
        , eLam [Label 0 "x"] (EOp1 (1, ONot) (EVar (Label 2 "x")))
        )
      , -- (lam(x) => not(x))(true)
        ( eApp () (eLam [Label () "x"] (EOp1 ((), ONot) (EVar (Label () "x")))) [ELit (PBool True)]
        , eApp 0 (eLam [Label 1 "x"] (EOp1 (2, ONot) (EVar (Label 3 "x")))) [ELit (PBool True)]
        )
      , -- if true == false then 1 else 100
        ( EIf (EOp2 ((), OEq) (ELit (PBool True)) (ELit (PBool False))) (ELit (PInt32 1)) (ELit (PInt32 100))
        , EIf (EOp2 (0, OEq) (ELit (PBool True)) (ELit (PBool False))) (ELit (PInt32 1)) (ELit (PInt32 100))
        )
      , -- match x { | Cons(x) => 1 | Nil => 2 }
        ( ePat (EVar (Label () "x")) [clause [Label () "Cons", Label () "x"] (ELit (PInt32 1)), clause [Label () "Nil"] (ELit (PInt32 2))]
        , ePat (EVar (Label 0 "x")) [clause [Label 1 "Cons", Label 2 "x"] (ELit (PInt32 1)), clause [Label 3 "Nil"] (ELit (PInt32 2))]
        )
      , -- not(false)
        ( EOp1 ((), ONot) (ELit (PBool False))
        , EOp1 (0, ONot) (ELit (PBool False))
        )
      ]

--

typeOfShouldEqual :: Expr Type -> Type -> SpecWith ()
typeOfShouldEqual input result = do
  it (trimStr is) (typeOf input == result)
  where
    is = "typeOf (" <> show input <> ") == " <> show result

testTypeOf :: SpecWith ()
testTypeOf = do
  describe "typeOf" $ do
    typeOfShouldEqual
      (eLam [Label tInt32 "x"] (ELit (PInt32 0)))
      (tInt32 ~> tInt32)
    typeOfShouldEqual
      (eLet [(Label (TVar 2 ~> TVar 2) "id", eLam [Label (TVar 2) "x"] (EVar (Label (TVar 2) "x")))] (EIf (eApp tBool (EVar (Label (tBool ~> tBool) "id")) [ELit (PBool True)]) (eApp tInt32 (EVar (Label (tInt32 ~> tInt32) "id")) [ELit (PInt32 1)]) (ELit (PInt32 2))))
      tInt32
    typeOfShouldEqual
      (EOp2 (tInt32, OAdd) (ELit (PInt32 1)) (ELit (PInt32 1)))
      tInt32

--

inferExprShouldEqual :: Expr () -> Either String (Expr Type) -> SpecWith ()
inferExprShouldEqual input result =
  it (trimStr is) ((zeroIndexed <$> typeExpr testTypeEnv input) == result)
  where
    is = "inferExpr (" <> show input <> ") == " <> show result

testInferExpr :: SpecWith ()
testInferExpr = do
  describe "inferExpr" $ do
    mapM_ (uncurry inferExprShouldEqual)
      [ -- field { name = n | r } = { id = 1 } in n
        ( EFocus
            (Focus (Label () "name") (Label () "n") (Label () "r"))
            (EExt "id" (ELit (PInt32 1)) ENil)
            (EVar (Label () "n"))
        , Left "Cannot unify"
        )
      , -- field { name = n | r } = { name = "Bob", id = 1 } in n
        ( EFocus
            (Focus (Label () "name") (Label () "n") (Label () "r"))
            (EExt "name" (ELit (PString "Bob")) (EExt "id" (ELit (PInt32 1)) ENil))
            (EVar (Label () "n"))
        , Right $ EFocus
            (Focus (Label tString "name") (Label tString "n") (Label (RExt "id" [tInt32] RNil) "r"))
            (EExt "name" (ELit (PString "Bob")) (EExt "id" (ELit (PInt32 1)) ENil))
            (EVar (Label tString "n"))
        )
      , -- let id = lam(x) => x in if id(true) then id(1) else 2
        ( eLet
            [
              ( Label () "id"
              , eLam
                  [Label () "x"]
                  (EVar (Label () "x"))
              )
            ]
            (EIf
              (eApp ()
                (EVar (Label () "id"))
                [ELit (PBool True)])
              (eApp ()
                (EVar (Label () "id"))
                [ELit (PInt32 1)])
              (ELit (PInt32 2)))
        , Right $ eLet
            [
              ( Label (TVar 0 ~> TVar 0) "id"
              , eLam
                [Label (TVar 0) "x"]
                (EVar (Label (TVar 0) "x"))
              )
            ]
            (EIf
              (eApp tBool
                (EVar (Label (tBool ~> tBool) "id"))
                [ELit (PBool True)])
              (eApp tInt32
                (EVar (Label (tInt32 ~> tInt32) "id"))
                [ELit (PInt32 1)])
              (ELit (PInt32 2)))
        )
      , -- let f = lam(n) => if n == 0 then 1 else n * f(n - 1) in f(5)
        ( eLet
            [
              ( Label () "f"
              , eLam
                  [Label () "n"]
                  (EIf
                    (EOp2
                      ((), OEq)
                      (EVar (Label () "n"))
                      (ELit (PInt32 0)))
                    (ELit (PInt32 1))
                    (EOp2
                      ((), OMul)
                      (EVar (Label () "n"))
                      (eApp
                        ()
                        (EVar (Label () "f"))
                        [EOp2
                          ((), OSub)
                          (EVar (Label () "n"))
                          (ELit (PInt32 1))
                        ])
                    ))
              )
            ]
            (eApp ()
              (EVar (Label () "f"))
              [ELit (PInt32 5)])
        , Right $ eLet
            [
              ( Label (tInt32 ~> tInt32) "f"
              ,  eLam
                  [Label tInt32 "n"]
                  (EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt32 "n"))
                      (ELit (PInt32 0)))
                    (ELit (PInt32 1))
                    (EOp2
                      (tInt32, OMul)
                      (EVar (Label tInt32 "n"))
                      (eApp
                        tInt32
                        (EVar (Label (tInt32 ~> tInt32) "f"))
                        [EOp2
                          (tInt32, OSub)
                          (EVar (Label tInt32 "n"))
                          (ELit (PInt32 1))
                        ])
                    ))
              )
            ]
            (eApp tInt32
              (EVar (Label (tInt32 ~> tInt32) "f"))
              [ELit (PInt32 5)])
        )
      , -- let f = lam(x, y) => x + y in let g = f(1) in g(2)
        ( eLet
            [
              ( Label () "f"
              , eLam
                  [Label () "x", Label () "y"]
                  (EOp2
                    ((), OAdd)
                    (EVar (Label () "x"))
                    (EVar (Label () "y")))
              )
            ]
            (eLet
              [
                ( Label () "g"
                , eApp
                    ()
                    (EVar (Label () "f"))
                    [ELit (PInt32 1)]
                )
              ]
              (eApp
                ()
                (EVar (Label () "g"))
                [ELit (PInt32 2)]))
        , Right $ eLet
            [
              ( Label (TVar 0 ~> TVar 0 ~> TVar 0) "f"
              , eLam
                  [Label (TVar 0) "x", Label (TVar 0) "y"]
                  (EOp2
                    (TVar 0, OAdd)
                    (EVar (Label (TVar 0) "x"))
                    (EVar (Label (TVar 0) "y")))
              )
            ]
            (eLet
              [
                ( Label (tInt32 ~> tInt32) "g"
                , eApp
                  (tInt32 ~> tInt32)
                  (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "f"))
                  [ELit (PInt32 1)]
                )
              ]
              (eApp
                tInt32
                (EVar (Label (tInt32 ~> tInt32) "g"))
                [ELit (PInt32 2)]))
        )
      , -- let f = lam(g) => g(1) in let h = lam(x) => x + 1 in f(h)
        ( eLet
            [
              ( Label () "f"
              , eLam
                  [Label () "g"]
                  (eApp
                    ()
                    (EVar (Label () "g"))
                    [ELit (PInt32 1)])
              )
            ]
            (eLet
              [
                ( Label () "h"
                , eLam
                    [Label () "x"]
                    (EOp2
                      ((), OAdd)
                      (EVar (Label () "x"))
                      (ELit (PInt32 1)))
                )
              ]
              (eApp
                ()
                (EVar (Label () "f"))
                [EVar (Label () "h")])
            )
        , Right $ eLet
            [
              ( Label ((tInt32 ~> TVar 0) ~> TVar 0) "f"
              , eLam
                  [Label (tInt32 ~> TVar 0) "g"]
                  (eApp
                    (TVar 0)
                    (EVar (Label (tInt32 ~> TVar 0) "g"))
                    [ELit (PInt32 1)])
              )
            ]
            (eLet
              [
                ( Label (tInt32 ~> tInt32) "h"
                , eLam
                    [Label tInt32 "x"]
                    (EOp2
                      (tInt32, OAdd)
                      (EVar (Label tInt32 "x"))
                      (ELit (PInt32 1)))
                )
              ]
              (eApp
                tInt32
                (EVar (Label ((tInt32 ~> tInt32) ~> tInt32) "f"))
                [EVar (Label (tInt32 ~> tInt32) "h")])
            )
        )
      , -- let f = lam(x, y) => x + y in f(3, 5)
        ( eLet
            [
              ( Label () "f"
              , eLam
                  [Label () "x", Label () "y"]
                  (EOp2
                    ((), OAdd)
                    (EVar (Label () "x"))
                    (EVar (Label () "y")))
              )
            ]
            (eApp
              ()
              (EVar (Label () "f"))
              [ELit (PInt32 3), ELit (PInt32 5)])
        , Right $ eLet
            [
              ( Label (TVar 0 ~> TVar 0 ~> TVar 0) "f"
              , eLam
                  [Label (TVar 0) "x", Label (TVar 0) "y"]
                  (EOp2
                    (TVar 0, OAdd)
                    (EVar (Label (TVar 0) "x"))
                    (EVar (Label (TVar 0) "y")))
              )
            ]
            (eApp
              tInt32
              (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "f"))
              [ELit (PInt32 3), ELit (PInt32 5)])
        )
      , -- let f = lam(x) => match x { | Cons(x, xs) => x | Nil() => 0 } in f(Cons(5, Nil()))
        ( eLet
            [
              ( Label () "f"
              , eLam
                  [Label () "x"]
                  (ePat
                    (EVar (Label () "x"))
                    [ clause [Label () "Cons", Label () "y", Label () "ys"] (EVar (Label () "y"))
                    , clause [Label () "Nil"] (ELit (PInt32 0))
                    ])
              )
            ]
            (eApp
              ()
              (EVar (Label () "f"))
              [ eApp
                  ()
                  (EVar (Label () "Cons"))
                  [ ELit (PInt32 5)
                  , EVar (Label () "Nil")
                  ]
              ])
        , Right $ eLet
            [
              ( Label (TCon "List" [tInt32] ~> tInt32) "f"
              , eLam
                  [Label (TCon "List" [tInt32]) "x"]
                  (ePat
                    (EVar (Label (TCon "List" [tInt32]) "x"))
                    [ clause [Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons", Label tInt32 "y", Label (TCon "List" [tInt32]) "ys"] (EVar (Label tInt32 "y"))
                    , clause [Label (TCon "List" [tInt32]) "Nil"] (ELit (PInt32 0))
                    ])
              )
            ]
            (eApp
              tInt32
              (EVar (Label (TCon "List" [tInt32] ~> tInt32) "f"))
              [ eApp
                  (TCon "List" [tInt32])
                  (EVar (Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"))
                  [ ELit (PInt32 5)
                  , EVar (Label (TCon "List" [tInt32]) "Nil")
                  ]
              ])
        )
      , -- let id = if id(true) then lam(x) => x else lam(x) => x
        ( eLet
            [
              ( Label () "id"
              , EIf
                  (eApp ()
                    (EVar (Label () "id"))
                    [ELit (PBool True)])
                  (eLam
                    [Label () "x"]
                    (EVar (Label () "x")))
                  (eLam
                    [Label () "x"]
                    (EVar (Label () "x")))
              )
            ]
            (eApp ()
              (EVar (Label () "id"))
              [ELit (PInt32 5)])
        , Left "Cannot unify"
        )
      , -- let x = 1 in let f = lam(y) => x + y in f(1)
        ( eLet
            [
              ( Label () "x"
              , ELit (PInt32 1)
              )
            ]
            (eLet
              [
                ( Label () "f"
                , eLam
                    [Label () "y"]
                    (EOp2
                      ((), OAdd)
                      (EVar (Label () "x"))
                      (EVar (Label () "y")))
                )
              ]
              (eApp
                ()
                (EVar (Label () "f"))
                [ELit (PInt32 1)]))
        , Right $ eLet
            [
              ( Label tInt32 "x"
              , ELit (PInt32 1)
              )
            ]
            (eLet
              [
                ( Label (tInt32 ~> tInt32) "f"
                , eLam
                    [Label tInt32 "y"]
                    (EOp2
                      (tInt32, OAdd)
                      (EVar (Label tInt32 "x"))
                      (EVar (Label tInt32 "y")))
                )
              ]
              (eApp
                tInt32
                (EVar (Label (tInt32 ~> tInt32) "f"))
                [ELit (PInt32 1)]))
        )
      , --  let f = lam(g) => g(100) in let h = lam(x, y) => x + y in (f(h))(1)
        ( eLet
            [
              ( Label () "f"
              , eLam
                  [Label () "g"]
                  (eApp
                    ()
                    (EVar (Label () "g"))
                    [ELit (PInt32 100)])
              )
            ]
            (eLet
              [
                ( Label () "h"
                , eLam
                    [Label () "x", Label () "y"]
                    (EOp2
                      ((), OAdd)
                      (EVar (Label () "x"))
                      (EVar (Label () "y")))
                )
              ]
              (eApp
                ()
                (eApp
                  ()
                  (EVar (Label () "f"))
                  [EVar (Label () "h")])
                [ELit (PInt32 1)]))
        , Right $ eLet
            [
              ( Label ((tInt32 ~> TVar 0) ~> TVar 0) "f"
              , eLam
                  [Label (tInt32 ~> TVar 0) "g"]
                  (eApp
                    (TVar 0)
                    (EVar (Label (tInt32 ~> TVar 0) "g"))
                    [ELit (PInt32 100)])
              )
            ]
            (eLet
              [
                ( Label (TVar 1 ~> TVar 1 ~> TVar 1) "h"
                , eLam
                    [Label (TVar 1) "x", Label (TVar 1) "y"]
                    (EOp2
                      (TVar 1, OAdd)
                      (EVar (Label (TVar 1) "x"))
                      (EVar (Label (TVar 1) "y")))
                )
              ]
              (eApp
                tInt32
                (eApp
                  (tInt32 ~> tInt32)
                  (EVar (Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "f"))
                  [EVar (Label (tInt32 ~> tInt32 ~> tInt32) "h")])
                [ELit (PInt32 1)]))
        )
        ,
          ( eLet
              [
                ( Label () "f"
                , eLam
                    [Label () "x", Label () "y"]
                    (EOp2
                      ((), OAdd)
                      (EVar (Label () "x"))
                      (EVar (Label () "y")))
                )
              ]
              (eLet
                [
                  ( Label () "g"
                  , eLam
                      [Label () "$f"]
                      (eApp
                        ()
                        (EVar (Label () "$f"))
                        [ELit (PInt32 1)])
                  )
                ]
                (eApp
                  ()
                  (EVar (Label () "g"))
                  [EVar (Label () "f"), ELit (PInt32 2)]))
          , Right $ eLet
              [
                ( Label (TVar 0 ~> TVar 0 ~> TVar 0) "f"
                , eLam
                    [Label (TVar 0) "x", Label (TVar 0) "y"]
                    (EOp2
                      (TVar 0, OAdd)
                      (EVar (Label (TVar 0) "x"))
                      (EVar (Label (TVar 0) "y")))
                )
              ]
              (eLet
                [
                  ( Label ((tInt32 ~> TVar 1) ~> TVar 1) "g"
                  , eLam
                      [Label (tInt32 ~> TVar 1) "$f"]
                      (eApp
                        (TVar 1)
                        (EVar (Label (tInt32 ~> TVar 1) "$f"))
                        [ELit (PInt32 1)])
                  )
                ]
                (eApp
                  tInt32
                  (EVar (Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "g"))
                  [EVar (Label (tInt32 ~> tInt32 ~> tInt32) "f"), ELit (PInt32 2)]))
          )
        , -- let f = lam(x) => g(x + 1); g = lam(y) => if y == 100 then 100 else f(y) in f(1)
          ( eLet
            [
              ( Label () "f"
              , eLam [Label () "x"]
                  (eApp ()
                    (EVar (Label () "g"))
                    [ EOp2
                        ((), OAdd)
                        (EVar (Label () "x"))
                        (ELit (PInt32 1))
                    ])
              )
            ,
              ( Label () "g"
              , eLam [Label () "y"]
                  (EIf
                    (EOp2
                      ((), OEq)
                      (EVar (Label () "y"))
                      (ELit (PInt32 100)))
                    (ELit (PInt32 100))
                    (eApp ()
                      (EVar (Label () "f"))
                      [ EVar (Label () "y") ]))
              )
            ]
            (eApp ()
              (EVar (Label () "f"))
              [ ELit (PInt32 1) ])
          , Right $ eLet
            [
              ( Label (tInt32 ~> tInt32) "f"
              , eLam [Label tInt32 "x"]
                  (eApp tInt32
                    (EVar (Label (tInt32 ~> tInt32) "g"))
                    [ EOp2
                        (tInt32, OAdd)
                        (EVar (Label tInt32 "x"))
                        (ELit (PInt32 1))
                    ])
              )
            ,
              ( Label (tInt32 ~> tInt32) "g"
              , eLam [Label tInt32 "y"]
                  (EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt32 "y"))
                      (ELit (PInt32 100)))
                    (ELit (PInt32 100))
                    (eApp tInt32
                      (EVar (Label (tInt32 ~> tInt32) "f"))
                      [ EVar (Label tInt32 "y") ]))
              )
            ]
            (eApp tInt32
              (EVar (Label (tInt32 ~> tInt32) "f"))
              [ ELit (PInt32 1) ])
          )
        , --
          ( eLet
              [
                ( Label () "f.0"
                , eLam
                    [Label () "n.2"]
                    (EIf
                      (EOp2
                        ((), OEq)
                        (EVar (Label () "n.2"))
                        (ELit (PInt32 0))
                      )
                      (ELit (PInt32 1))
                      (eApp
                        ()
                        (EVar (Label () "g.1"))
                        [ EVar (Label () "n.2") ]
                      ))
                )
              , ( Label () "g.1"
                , eLam
                    [Label () "n.3"]
                    (eLet
                      [ ( Label () "m.4"
                        , EOp2
                            ((), OSub)
                            (EVar (Label () "n.3"))
                            (ELit (PInt32 1))
                        )
                      ]
                      (eApp
                        ()
                        (EVar (Label () "f.0"))
                        [ EVar (Label () "m.4") ]
                      ))
                )
              ]
              (eLet
                [
                  ( Label () "id.5"
                  , eLam [Label () "x.6"] (EVar (Label () "x.6"))
                  )
                ]
                (eApp
                  ()
                  (eApp
                    ()
                    (EVar (Label () "id.5"))
                    [ EVar (Label () "f.0") ]
                  )
                  [ eApp
                      ()
                      (EVar (Label () "id.5"))
                      [ ELit (PInt32 5) ]
                  ])
                )
          , Right $ eLet
              [
                ( Label (tInt32 ~> tInt32) "f.0"
                , eLam
                    [Label tInt32 "n.2"]
                    (EIf
                      (EOp2
                        (tBool, OEq)
                        (EVar (Label tInt32 "n.2"))
                        (ELit (PInt32 0))
                      )
                      (ELit (PInt32 1))
                      (eApp
                        tInt32
                        (EVar (Label (tInt32 ~> tInt32) "g.1"))
                        [ EVar (Label tInt32 "n.2") ]
                      ))
                )
              , ( Label (tInt32 ~> tInt32) "g.1"
                , eLam
                    [Label tInt32 "n.3"]
                    (eLet
                      [ ( Label tInt32 "m.4"
                        , EOp2
                            (tInt32, OSub)
                            (EVar (Label tInt32 "n.3"))
                            (ELit (PInt32 1))
                        )
                      ]
                      (eApp
                        tInt32
                        (EVar (Label (tInt32 ~> tInt32) "f.0"))
                        [ EVar (Label tInt32 "m.4") ]
                      ))
                )
              ]
              (eLet
                [
                  ( Label (TVar 0 ~> TVar 0) "id.5"
                  , eLam [Label (TVar 0) "x.6"] (EVar (Label (TVar 0) "x.6"))
                  )
                ]
                (eApp
                  tInt32
                  (eApp
                    (tInt32 ~> tInt32)
                    (EVar (Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id.5"))
                    [ EVar (Label (tInt32 ~> tInt32) "f.0") ]
                  )
                  [ eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "id.5"))
                      [ ELit (PInt32 5) ]
                  ])
                )
          )
        ,
          ( --
            -- let
            --   f =
            --     lam(x) =>
            --       match x {
            --         | Nil => 0
            --         | Cons(a, as) => 1 + f(as)
            --       }
            --   in
            --     let
            --       xs =
            --         Cons(5, Nil)
            --       in
            --         let
            --           ys =
            --             Cons(4, xs)
            --           in
            --             f(ys)
            --
            eLet
              [
                ( Label () "f"
                , eLam
                    [ Label () "x" ]
                    ( ePat
                        (EVar (Label () "x"))
                        [ clause
                            [ Label () "Nil" ]
                            (ELit (PInt32 0))
                        , clause
                            [ Label () "Cons"
                            , Label () "a"
                            , Label () "as" ]
                            (EOp2
                              ((), OAdd)
                              (ELit (PInt32 1))
                              (eApp
                                ()
                                (EVar (Label () "f"))
                                [ EVar (Label () "as")
                                ]
                              )
                            )
                        ]
                    )
                )
              ]
              (eLet
                [
                  ( Label () "xs"
                  , eApp
                      ()
                      (EVar (Label () "Cons"))
                      [ ELit (PInt32 5)
                      , EVar (Label () "Nil")
                      ]
                  )
                ]
                (eLet
                  [
                    ( Label () "ys"
                    , eApp
                        ()
                        (EVar (Label () "Cons"))
                        [ ELit (PInt32 4)
                        , EVar (Label () "xs")
                        ]
                    )
                  ]
                  (eApp
                    ()
                    (EVar (Label () "f"))
                    [ EVar (Label () "ys") ]
                  )
                )
              )
          , Right $ eLet
              [
                ( Label (TCon "List" [TVar 0] ~> tInt32) "f"
                , eLam
                    [ Label (TCon "List" [TVar 0]) "x" ]
                    ( ePat
                        (EVar (Label (TCon "List" [TVar 0]) "x"))
                        [ clause [ Label (TCon "List" [TVar 0]) "Nil" ] (ELit (PInt32 0))
                        , clause
                            [ Label (TVar 0 ~> TCon "List" [TVar 0] ~> TCon "List" [TVar 0]) "Cons"
                            , Label (TVar 0) "a"
                            , Label (TCon "List" [TVar 0]) "as" ]
                            (EOp2
                              (tInt32, OAdd)
                              (ELit (PInt32 1))
                              (eApp
                                tInt32
                                (EVar (Label (TCon "List" [TVar 0] ~> tInt32) "f"))
                                [ EVar (Label (TCon "List" [TVar 0]) "as")
                                ]
                              )
                            )
                        ]
                    )
                )
              ]
              (eLet
                [
                  ( Label (TCon "List" [tInt32]) "xs"
                  , eApp
                      (TCon "List" [tInt32])
                      (EVar (Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"))
                      [ ELit (PInt32 5)
                      , EVar (Label (TCon "List" [tInt32]) "Nil")
                      ]
                  )
                ]
                (eLet
                  [
                    ( Label (TCon "List" [tInt32]) "ys"
                    , eApp
                        (TCon "List" [tInt32])
                        (EVar (Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"))
                        [ ELit (PInt32 4)
                        , EVar (Label (TCon "List" [tInt32]) "xs")
                        ]
                    )
                  ]
                  (eApp
                    tInt32
                    (EVar (Label (TCon "List" [tInt32] ~> tInt32) "f"))
                    [ EVar (Label (TCon "List" [tInt32]) "ys") ]
                  )
                )
              )
          )
--      , --
--        ( undefined
--        , undefined
--        )
      ]

--

qualifyNamesShouldEqual :: (Show t, Eq t) => Expr t -> Expr t -> SpecWith ()
qualifyNamesShouldEqual input result =
  it (trimStr is) (qualifyNames input == result)
  where
    is = "qualifyNames (" <> show input <> ") == " <> show result

testQualifyNames :: SpecWith ()
testQualifyNames = do
  describe "qualifyNames" $ do
    mapM_ (uncurry qualifyNamesShouldEqual)
      [ --  field { name = n | r } = { name = "Bob", id = 1 } in n
        ( EFocus
            (Focus (Label () "name") (Label () "n") (Label () "r"))
            (EExt "name" (ELit (PString "Bob")) (EExt "id" (ELit (PInt32 1)) ENil))
            (EVar (Label () "n"))
        -- field { name = n.0 | r.1 } = { name = "Bob", id = 1 } in n.0
        , EFocus
            (Focus (Label () "name") (Label () "n.0") (Label () "r.1"))
            (EExt "name" (ELit (PString "Bob")) (EExt "id" (ELit (PInt32 1)) ENil))
            (EVar (Label () "n.0"))
        )
      , -- let f = 1 ; g = 2 in let f  = 2 in f
        ( eLet [(Label () "f", ELit (PInt32 1)), (Label () "g", ELit (PInt32 2))] (eLet [(Label () "f", ELit (PInt32 2))] (EVar (Label () "f")))
        -- let f.0 = 1 ; g.1 = 2 in let f.2  = 2 in f.2
        , eLet
            [
              (Label () "f.0", ELit (PInt32 1))
            , (Label () "g.1", ELit (PInt32 2))
            ]
            (eLet
              [(Label () "f.2", ELit (PInt32 2))]
              (EVar (Label () "f.2"))
            )
        )
      , ( -- let
          --   f =
          --     lam(n) =>
          --       if n == 0
          --         then 1
          --         else g(n) ;
          --   g =
          --     lam(n) =>
          --       let
          --         m =
          --           n - 1
          --         in
          --           f(m)
          --   in
          --     let
          --       id =
          --         lam(x) => x
          --       in
          --         (id(f))(id(5))
          eLet
            [
              ( Label () "f"
              , eLam
                  [Label () "n"]
                  (EIf
                    (EOp2
                      ((), OEq)
                      (EVar (Label () "n"))
                      (ELit (PInt32 0))
                    )
                    (ELit (PInt32 1))
                    (eApp
                      ()
                      (EVar (Label () "g"))
                      [ EVar (Label () "n") ]
                    ))
              )
            , ( Label () "g"
              , eLam
                  [Label () "n"]
                  (eLet
                    [ ( Label () "m"
                      , EOp2
                          ((), OSub)
                          (EVar (Label () "n"))
                          (ELit (PInt32 1))
                      )
                    ]
                    (eApp
                      ()
                      (EVar (Label () "f"))
                      [ EVar (Label () "m") ]
                    ))
              )
            ]
            (eLet
              [
                ( Label () "id"
                , eLam [Label () "x"] (EVar (Label () "x"))
                )
              ]
              (eApp
                ()
                (eApp
                  ()
                  (EVar (Label () "id"))
                  [ EVar (Label () "f") ]
                )
                [ eApp
                    ()
                    (EVar (Label () "id"))
                    [ ELit (PInt32 5) ]
                ])
              )
        , eLet
            [
              ( Label () "f.0"
              , eLam
                  [Label () "n.2"]
                  (EIf
                    (EOp2
                      ((), OEq)
                      (EVar (Label () "n.2"))
                      (ELit (PInt32 0))
                    )
                    (ELit (PInt32 1))
                    (eApp
                      ()
                      (EVar (Label () "g.1"))
                      [ EVar (Label () "n.2") ]
                    ))
              )
            , ( Label () "g.1"
              , eLam
                  [Label () "n.3"]
                  (eLet
                    [ ( Label () "m.4"
                      , EOp2
                          ((), OSub)
                          (EVar (Label () "n.3"))
                          (ELit (PInt32 1))
                      )
                    ]
                    (eApp
                      ()
                      (EVar (Label () "f.0"))
                      [ EVar (Label () "m.4") ]
                    ))
              )
            ]
            (eLet
              [
                ( Label () "id.5"
                , eLam [Label () "x.6"] (EVar (Label () "x.6"))
                )
              ]
              (eApp
                ()
                (eApp
                  ()
                  (EVar (Label () "id.5"))
                  [ EVar (Label () "f.0") ]
                )
                [ eApp
                    ()
                    (EVar (Label () "id.5"))
                    [ ELit (PInt32 5) ]
                ])
              )
        )
      , ( ePat
            (EVar (Label () "x"))
            [
              clause [ Label () "y" ] (EVar (Label () "y"))
            , clause [ Label () "y" ] (EVar (Label () "y"))
            , clause [ Label () "y" ] (EVar (Label () "y"))
            ]
        , ePat
            (EVar (Label () "x"))
            [
              clause [ Label () "y.0" ] (EVar (Label () "y.0"))
            , clause [ Label () "y.1" ] (EVar (Label () "y.1"))
            , clause [ Label () "y.2" ] (EVar (Label () "y.2"))
            ]
        )
      ]
    mapM_ (uncurry qualifyNamesShouldEqual)
      [
        ( eLet
            [
              ( Label (tInt32 ~> tInt32) "f"
              , eLam
                  [Label tInt32 "n"]
                  (EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt32 "n"))
                      (ELit (PInt32 0))
                    )
                    (ELit (PInt32 1))
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "g"))
                      [ EVar (Label tInt32 "n") ]
                    ))
              )
            , ( Label (tInt32 ~> tInt32) "g"
              , eLam
                  [Label tInt32 "n"]
                  (eLet
                    [ ( Label tInt32 "m"
                      , EOp2
                          (tInt32, OSub)
                          (EVar (Label tInt32 "n"))
                          (ELit (PInt32 1))
                      )
                    ]
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "f"))
                      [ EVar (Label tInt32 "m") ]
                    ))
              )
            ]
            (eLet
              [
                ( Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id.0"
                , eLam [Label (tInt32 ~> tInt32) "x"] (EVar (Label (tInt32 ~> tInt32) "x"))
                )
              , ( Label (tInt32 ~> tInt32) "id.1"
                , eLam [Label tInt32 "x"] (EVar (Label tInt32 "x"))
                )
              ]
              (eApp
                tInt32
                (eApp
                  (tInt32 ~> tInt32)
                  (EVar (Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id.0"))
                  [ EVar (Label (tInt32 ~> tInt32) "f") ]
                )
                [ eApp
                    tInt32
                    (EVar (Label (tInt32 ~> tInt32) "id.1"))
                    [ ELit (PInt32 5) ]
                ])
              )
        , eLet
            [
              ( Label (tInt32 ~> tInt32) "f.0"
              , eLam
                  [Label tInt32 "n.2"]
                  (EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt32 "n.2"))
                      (ELit (PInt32 0))
                    )
                    (ELit (PInt32 1))
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "g.1"))
                      [ EVar (Label tInt32 "n.2") ]
                    ))
              )
            , ( Label (tInt32 ~> tInt32) "g.1"
              , eLam
                  [Label tInt32 "n.3"]
                  (eLet
                    [ ( Label tInt32 "m.4"
                      , EOp2
                          (tInt32, OSub)
                          (EVar (Label tInt32 "n.3"))
                          (ELit (PInt32 1))
                      )
                    ]
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "f.0"))
                      [ EVar (Label tInt32 "m.4") ]
                    ))
              )
            ]
            (eLet
              [
                ( Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id.0.5"
                , eLam [Label (tInt32 ~> tInt32) "x.7"] (EVar (Label (tInt32 ~> tInt32) "x.7"))
                )
              , ( Label (tInt32 ~> tInt32) "id.1.6"
                , eLam [Label tInt32 "x.8"] (EVar (Label tInt32 "x.8"))
                )
              ]
              (eApp
                tInt32
                (eApp
                  (tInt32 ~> tInt32)
                  (EVar (Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id.0.5"))
                  [ EVar (Label (tInt32 ~> tInt32) "f.0") ]
                )
                [ eApp
                    tInt32
                    (EVar (Label (tInt32 ~> tInt32) "id.1.6"))
                    [ ELit (PInt32 5) ]
                ])
              )
        )
      , ( eLet
            [
              ( Label (TCon "List" [TVar 0] ~> tInt32) "f.0"
              , eLam
                  [ Label (TCon "List" [TVar 0]) "x" ]
                  ( ePat
                      (EVar (Label (TCon "List" [TVar 0]) "x"))
                      [ clause [ Label (TCon "List" [TVar 0]) "Nil" ] (ELit (PInt32 0))
                      , clause
                          [ Label (TVar 0 ~> TCon "List" [TVar 0] ~> TCon "List" [TVar 0]) "Cons"
                          , Label (TVar 0) "a"
                          , Label (TCon "List" [TVar 0]) "as" ]
                          (EOp2
                            (tInt32, OAdd)
                            (ELit (PInt32 1))
                            (eApp
                              tInt32
                              (EVar (Label (TCon "List" [TVar 0] ~> tInt32) "f.0"))
                              [ EVar (Label (TCon "List" [TVar 0]) "as")
                              ]
                            )
                          )
                      ]
                  )
              )
            , ( Label (TCon "List" [tInt32] ~> tInt32) "f.1"
              , eLam
                  [ Label (TCon "List" [tInt32]) "x" ]
                  ( ePat
                      (EVar (Label (TCon "List" [tInt32]) "x"))
                      [ clause [ Label (TCon "List" [tInt32]) "Nil" ] (ELit (PInt32 0))
                      , clause
                          [ Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"
                          , Label tInt32 "a"
                          , Label (TCon "List" [tInt32]) "as" ]
                          (EOp2
                            (tInt32, OAdd)
                            (ELit (PInt32 1))
                            (eApp
                              tInt32
                              (EVar (Label (TCon "List" [tInt32] ~> tInt32) "f.1"))
                              [ EVar (Label (TCon "List" [tInt32]) "as")
                              ]
                            )
                          )
                      ]
                  )
              )
            ]
            (eLet
              [
                ( Label (TCon "List" [tInt32]) "xs"
                , eApp
                    (TCon "List" [tInt32])
                    (EVar (Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"))
                    [ ELit (PInt32 5)
                    , EVar (Label (TCon "List" [tInt32]) "Nil")
                    ]
                )
              ]
              (eLet
                [
                  ( Label (TCon "List" [tInt32]) "ys"
                  , eApp
                      (TCon "List" [tInt32])
                      (EVar (Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"))
                      [ ELit (PInt32 4)
                      , EVar (Label (TCon "List" [tInt32]) "xs")
                      ]
                  )
                ]
                (eApp
                  tInt32
                  (EVar (Label (TCon "List" [tInt32] ~> tInt32) "f.1"))
                  [ EVar (Label (TCon "List" [tInt32]) "ys") ]
                )
              )
            )
        , eLet
            [
              ( Label (TCon "List" [TVar 0] ~> tInt32) "f.0.0"
              , eLam
                  [ Label (TCon "List" [TVar 0]) "x.2" ]
                  ( ePat
                      (EVar (Label (TCon "List" [TVar 0]) "x.2"))
                      [ clause [ Label (TCon "List" [TVar 0]) "Nil" ] (ELit (PInt32 0))
                      , clause
                          [ Label (TVar 0 ~> TCon "List" [TVar 0] ~> TCon "List" [TVar 0]) "Cons"
                          , Label (TVar 0) "a.3"
                          , Label (TCon "List" [TVar 0]) "as.4" ]
                          (EOp2
                            (tInt32, OAdd)
                            (ELit (PInt32 1))
                            (eApp
                              tInt32
                              (EVar (Label (TCon "List" [TVar 0] ~> tInt32) "f.0.0"))
                              [ EVar (Label (TCon "List" [TVar 0]) "as.4")
                              ]
                            )
                          )
                      ]
                  )
              )
            , ( Label (TCon "List" [tInt32] ~> tInt32) "f.1.1"
              , eLam
                  [ Label (TCon "List" [tInt32]) "x.5" ]
                  ( ePat
                      (EVar (Label (TCon "List" [tInt32]) "x.5"))
                      [ clause [ Label (TCon "List" [tInt32]) "Nil" ] (ELit (PInt32 0))
                      , clause
                          [ Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"
                          , Label tInt32 "a.6"
                          , Label (TCon "List" [tInt32]) "as.7" ]
                          (EOp2
                            (tInt32, OAdd)
                            (ELit (PInt32 1))
                            (eApp
                              tInt32
                              (EVar (Label (TCon "List" [tInt32] ~> tInt32) "f.1.1"))
                              [ EVar (Label (TCon "List" [tInt32]) "as.7")
                              ]
                            )
                          )
                      ]
                  )
              )
            ]
            (eLet
              [
                ( Label (TCon "List" [tInt32]) "xs.8"
                , eApp
                    (TCon "List" [tInt32])
                    (EVar (Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"))
                    [ ELit (PInt32 5)
                    , EVar (Label (TCon "List" [tInt32]) "Nil")
                    ]
                )
              ]
              (eLet
                [
                  ( Label (TCon "List" [tInt32]) "ys.9"
                  , eApp
                      (TCon "List" [tInt32])
                      (EVar (Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"))
                      [ ELit (PInt32 4)
                      , EVar (Label (TCon "List" [tInt32]) "xs.8")
                      ]
                  )
                ]
                (eApp
                  tInt32
                  (EVar (Label (TCon "List" [tInt32] ~> tInt32) "f.1.1"))
                  [ EVar (Label (TCon "List" [tInt32]) "ys.9") ]
                )
              )
            )
        )
      ]

--

substVarShouldEqual :: (Expr (), Name, Name) -> Expr () -> SpecWith ()
substVarShouldEqual (input, s, t) result =
  it (trimStr is) (substVar s t input == result)
  where
    is = "substVar (" <> show input <> ") == " <> show result

testSubstVar :: SpecWith ()
testSubstVar =
  describe "substVar" $
    mapM_ (uncurry substVarShouldEqual)
      [
        ( (eLet [(Label () "f", eLam [Label () "x"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y"))))] (ELit (PInt32 1)), "y", "z")
        , eLet [(Label () "f", eLam [Label () "x"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "z"))))] (ELit (PInt32 1))
        )
      , ( (ePat (EVar (Label () "a"))
            [ clause [Label () "a"] (EVar (Label () "a"))
            , clause [Label () "x"] (EVar (Label () "a"))
            ], "a", "b")
        , ePat (EVar (Label () "b"))
            [ clause [Label () "a"] (EVar (Label () "a"))
            , clause [Label () "x"] (EVar (Label () "b"))
            ]
        )
      ]

--

dictionaryShouldEqual :: Expr () -> Dictionary Type -> SpecWith ()
dictionaryShouldEqual input result =
  forM_ (Map.keys result) $
    \key -> it (show key) (Right (result ! key) == ((!) <$> dict <*> pure key))
  where
    dict = dictionary . zeroIndexed <$> typeExpr mempty input

dictionaryShouldEqual2 :: Expr Type -> Dictionary Type -> SpecWith ()
dictionaryShouldEqual2 input result =
  it (trimStr is) (dictionary input == result)
  where
    is = "dictionary (" <> show input <> ") == " <> show result

testDictionary :: SpecWith ()
testDictionary =
  describe "dictionary" $ do
    mapM_ (uncurry dictionaryShouldEqual)
      [
        ( eLet
            [
              ( Label () "n"
              , ELit (PInt32 500)
              )
            ]
            (eLet
              [
                ( Label () "f"
                , eLam [Label () "x"]
                    (eApp ()
                      (EVar (Label () "g"))
                      [ EOp2
                          ((), OAdd)
                          (EVar (Label () "x"))
                          (ELit (PInt32 1))
                      ])
                )
              ,
                ( Label () "g"
                , eLam [Label () "y"]
                    (EIf
                      (EOp2
                        ((), OEq)
                        (EVar (Label () "y"))
                        (ELit (PInt32 100)))
                      (EVar (Label () "n"))
                      (eApp ()
                        (EVar (Label () "f"))
                        [ EVar (Label () "y") ]))
                )
              ]
              (eLet
                [
                  ( Label () "z"
                  , eLam [Label () "x"] (EVar (Label () "x"))
                  )
                ]
                (eApp ()
                  (EVar (Label () "z"))
                  [ eApp ()
                      (EVar (Label () "f"))
                      [ ELit (PInt32 1) ]
                  ])
              ))
        , Map.fromList
            [
              ( "$fun.0",
                ( [Label tInt32 "x"]
                , eApp tInt32
                    (EVar (Label (tInt32 ~> tInt32) "g"))
                    [ EOp2
                        (tInt32, OAdd)
                        (EVar (Label tInt32 "x"))
                        (ELit (PInt32 1))
                    ]
                )
              )
            , ( "$fun.1",
                ( [Label tInt32 "y"]
                , EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt32 "y"))
                      (ELit (PInt32 100)))
                    (EVar (Label tInt32 "n"))
                    (eApp tInt32
                      (EVar (Label (tInt32 ~> tInt32) "f"))
                      [ EVar (Label tInt32 "y") ])
                )
              )
            , ( "$fun.2",
                ( [Label (TVar 0) "x"]
                , EVar (Label (TVar 0) "x")
                )
              )
            , ( "$fun._",
                ( []
                , eLet
                    [
                      ( Label tInt32 "n"
                      , ELit (PInt32 500)
                      )
                    ]
                    (eLet
                      [
                        ( Label (tInt32 ~> tInt32) "f"
                        , EVar (Label (tInt32 ~> tInt32) "$fun.0")
                        )
                      ,
                        ( Label (tInt32 ~> tInt32) "g"
                        , EVar (Label (tInt32 ~> tInt32) "$fun.1")
                        )
                      ]
                      (eLet
                        [
                          ( Label (TVar 0 ~> TVar 0) "z"
                          , EVar (Label (TVar 0 ~> TVar 0) "$fun.2")
                          )
                        ]
                        (eApp tInt32
                          (EVar (Label (tInt32 ~> tInt32) "z"))
                          [ eApp tInt32
                              (EVar (Label (tInt32 ~> tInt32) "f"))
                              [ ELit (PInt32 1) ]
                          ])
                      ))
                    )
              )
            ]
        )
      ]
    mapM_ (uncurry dictionaryShouldEqual2)
      [
        ( eLet
            [
              ( Label (tInt32 ~> tInt32) "f.0"
              , eLam
                  [Label tInt32 "n.2"]
                  (EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt32 "n.2"))
                      (ELit (PInt32 0))
                    )
                    (ELit (PInt32 1))
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "g.1"))
                      [ EVar (Label tInt32 "n.2") ]
                    ))
              )
            , ( Label (tInt32 ~> tInt32) "g.1"
              , eLam
                  [Label tInt32 "n.3"]
                  (eLet
                    [ ( Label tInt32 "m.4"
                      , EOp2
                          (tInt32, OSub)
                          (EVar (Label tInt32 "n.3"))
                          (ELit (PInt32 1))
                      )
                    ]
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "f.0"))
                      [ EVar (Label tInt32 "m.4") ]
                    ))
              )
            ]
            (eLet
              [
                ( Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id.0.5"
                , eLam [Label (tInt32 ~> tInt32) "x.7"] (EVar (Label (tInt32 ~> tInt32) "x.7"))
                )
              , ( Label (tInt32 ~> tInt32) "id.1.6"
                , eLam [Label tInt32 "x.8"] (EVar (Label tInt32 "x.8"))
                )
              ]
              (eApp
                tInt32
                (eApp
                  (tInt32 ~> tInt32)
                  (EVar (Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id.0.5"))
                  [ EVar (Label (tInt32 ~> tInt32) "f.0") ]
                )
                [ eApp
                    tInt32
                    (EVar (Label (tInt32 ~> tInt32) "id.1.6"))
                    [ ELit (PInt32 5) ]
                ])
              )
        , Map.fromList
            [
              ( "$fun.0"
              , ( [Label tInt32 "n.2"]
                , EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt32 "n.2"))
                      (ELit (PInt32 0))
                    )
                    (ELit (PInt32 1))
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "g.1"))
                      [ EVar (Label tInt32 "n.2") ]
                    )
                )
              )
            , ( "$fun.1"
              , ( [Label tInt32 "n.3"]
                , eLet
                    [ ( Label tInt32 "m.4"
                      , EOp2
                          (tInt32, OSub)
                          (EVar (Label tInt32 "n.3"))
                          (ELit (PInt32 1))
                      )
                    ]
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "f.0"))
                      [ EVar (Label tInt32 "m.4") ]
                    )
                )
              )
            , ( "$fun.2"
              , ( [Label (tInt32 ~> tInt32) "x.7"]
                , EVar (Label (tInt32 ~> tInt32) "x.7")
                )
              )
            , ( "$fun.3"
              , ( [Label tInt32 "x.8"]
                , EVar (Label tInt32 "x.8")
                )
              )
            , ( "$fun._"
              , ( []
                , eLet
                    [
                      ( Label (tInt32 ~> tInt32) "f.0"
                      , EVar (Label (tInt32 ~> tInt32) "$fun.0")
                      )
                    , ( Label (tInt32 ~> tInt32) "g.1"
                      , EVar (Label (tInt32 ~> tInt32) "$fun.1")
                      )
                    ]
                    (eLet
                      [
                        ( Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id.0.5"
                        , EVar (Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "$fun.2")
                        )
                      , ( Label (tInt32 ~> tInt32) "id.1.6"
                        , EVar (Label (tInt32 ~> tInt32) "$fun.3")
                        )
                      ]
                      (eApp
                        tInt32
                        (eApp
                          (tInt32 ~> tInt32)
                          (EVar (Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id.0.5"))
                          [ EVar (Label (tInt32 ~> tInt32) "f.0") ]
                        )
                        [ eApp
                            tInt32
                            (EVar (Label (tInt32 ~> tInt32) "id.1.6"))
                            [ ELit (PInt32 5) ]
                        ])
                    )
                )
              )
            ]
        )
--      , ( undefined
--        , undefined
--        )
      ]



--


-- flattenedExprShouldEqual :: Expr () -> Expr () -> SpecWith ()
-- flattenedExprShouldEqual input result =
--   it (trimStr is) (flattenLambdas input == result)
--   where
--     is = "flattenLambdas (" <> show input <> ") == " <> show result
--
-- testFlattenLambdas :: SpecWith ()
-- testFlattenLambdas =
--   describe "flattenLambdas" $ do
--     mapM_ (uncurry flattenedExprShouldEqual)
--       [ ( eLam [Label () "x"] (eLam [Label () "y"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y"))))
--         , eLam [Label () "x", Label () "y"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y")))
--         )
--       , ( eLam [Label () "x"] (eLam [Label () "y"] (eLam [Label () "z"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y")))))
--         , eLam [Label () "x", Label () "y", Label () "z"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y")))
--         )
--       ]
--
-- --

monomorphizeShouldEqual :: Expr Type -> Expr Type -> SpecWith ()
monomorphizeShouldEqual input result =
  it (trimStr is) (monomorphize input == result)
  where
    is = "monomorphize (" <> show input <> ") == " <> show result

testMonomorphize :: SpecWith ()
testMonomorphize =
  describe "monomorphize" $ do
    mapM_ (uncurry monomorphizeShouldEqual)
      [ ( eLet
            [
              ( Label (TVar 0 ~> TVar 0) "id"
              , eLam
                  [Label (TVar 0) "x"]
                  (EVar (Label (TVar 0) "x"))
              )
            ]
            (EIf
              (eApp tBool
                (EVar (Label (tBool ~> tBool) "id"))
                [ELit (PBool True)])
              (eApp tInt32
                (EVar (Label (tInt32 ~> tInt32) "id"))
                [ELit (PInt32 1)])
              (ELit (PInt32 2)))
        , eLet
            [
              ( Label (tBool ~> tBool) "id.0"
              , eLam
                  [Label tBool "x"]
                  (EVar (Label tBool "x"))
              )
            , ( Label (tInt32 ~> tInt32) "id.1"
              , eLam
                  [Label tInt32 "x"]
                  (EVar (Label tInt32 "x"))
              )
            ]
            (EIf
              (eApp tBool
                (EVar (Label (tBool ~> tBool) "id.0"))
                [ELit (PBool True)])
              (eApp tInt32
                (EVar (Label (tInt32 ~> tInt32) "id.1"))
                [ELit (PInt32 1)])
              (ELit (PInt32 2))))
      , ( eLet
            [
              ( Label ((tInt32 ~> TVar 0) ~> TVar 0) "f"
              , eLam
                  [Label (tInt32 ~> TVar 0) "g"]
                  (eApp
                    (TVar 0)
                    (EVar (Label (tInt32 ~> TVar 0) "g"))
                    [ELit (PInt32 100)])
              )
            ]
            (eLet
              [
                ( Label (TVar 1 ~> TVar 1 ~> TVar 1) "h"
                , eLam
                    [Label (TVar 1) "x", Label (TVar 1) "y"]
                    (EOp2
                      (TVar 1, OAdd)
                      (EVar (Label (TVar 1) "x"))
                      (EVar (Label (TVar 1) "y")))
                )
              ]
              (eApp
                tInt32
                (eApp
                  (tInt32 ~> tInt32)
                  (EVar (Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "f"))
                  [EVar (Label (tInt32 ~> tInt32 ~> tInt32) "h")])
                [ELit (PInt32 1)]))
        , eLet
            [
              ( Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "f.0"
              , eLam
                  [Label (tInt32 ~> tInt32 ~> tInt32) "g"]
                  (eApp
                    (tInt32 ~> tInt32)
                    (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "g"))
                    [ELit (PInt32 100)])
              )
            ]
            (eLet
              [
                ( Label (tInt32 ~> tInt32 ~> tInt32) "h.0"
                , eLam
                    [Label tInt32 "x", Label tInt32 "y"]
                    (EOp2
                      (tInt32, OAdd)
                      (EVar (Label tInt32 "x"))
                      (EVar (Label tInt32 "y")))
                )
              ]
              (eApp
                tInt32
                (eApp
                  (tInt32 ~> tInt32)
                  (EVar (Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "f.0"))
                  [EVar (Label (tInt32 ~> tInt32 ~> tInt32) "h.0")])
                [ELit (PInt32 1)]))
        )
        , ( eLet
              [
                ( Label (TVar 0 ~> TVar 0 ~> TVar 0) "f"
                , eLam
                    [Label (TVar 0) "x", Label (TVar 0) "y"]
                    (EOp2
                      (TVar 0, OAdd)
                      (EVar (Label (TVar 0) "x"))
                      (EVar (Label (TVar 0) "y")))
                )
              ]
              (eLet
                [
                  ( Label ((tInt32 ~> TVar 1) ~> TVar 1) "g"
                  , eLam
                      [Label (tInt32 ~> TVar 1) "$f"]
                      (eApp
                        (TVar 1)
                        (EVar (Label (tInt32 ~> TVar 1) "$f"))
                        [ELit (PInt32 1)])
                  )
                ]
                (eApp
                  tInt32
                  (EVar (Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "g"))
                  [EVar (Label (tInt32 ~> tInt32 ~> tInt32) "f"), ELit (PInt32 2)]))
          , eLet
              [
                ( Label (tInt32 ~> tInt32 ~> tInt32) "f.0"
                , eLam
                    [Label tInt32 "x", Label tInt32 "y"]
                    (EOp2
                      (tInt32, OAdd)
                      (EVar (Label tInt32 "x"))
                      (EVar (Label tInt32 "y")))
                )
              ]
              (eLet
                [
                  ( Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "g.0"
                  , eLam
                      [Label (tInt32 ~> tInt32 ~> tInt32) "$f"]
                      (eApp
                        (tInt32 ~> tInt32)
                        (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "$f"))
                        [ELit (PInt32 1)])
                  )
                ]
                (eApp
                  tInt32
                  (EVar (Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "g.0"))
                  [EVar (Label (tInt32 ~> tInt32 ~> tInt32) "f.0"), ELit (PInt32 2)]))
          )
        , ( eLet
              [
                ( Label (tInt32 ~> tInt32) "f"
                , eLam
                    [Label tInt32 "n"]
                    (EIf
                      (EOp2
                        (tBool, OEq)
                        (EVar (Label tInt32 "n"))
                        (ELit (PInt32 0))
                      )
                      (ELit (PInt32 1))
                      (eApp
                        tInt32
                        (EVar (Label (tInt32 ~> tInt32) "g"))
                        [ EVar (Label tInt32 "n") ]
                      ))
                )
              , ( Label (tInt32 ~> tInt32) "g"
                , eLam
                    [Label tInt32 "n"]
                    (eLet
                      [ ( Label tInt32 "m"
                        , EOp2
                            (tInt32, OSub)
                            (EVar (Label tInt32 "n"))
                            (ELit (PInt32 1))
                        )
                      ]
                      (eApp
                        tInt32
                        (EVar (Label (tInt32 ~> tInt32) "f"))
                        [ EVar (Label tInt32 "m") ]
                      ))
                )
              ]
              (eLet
                [
                  ( Label (TVar 0 ~> TVar 0) "id"
                  , eLam [Label (TVar 0) "x"] (EVar (Label (TVar 0) "x"))
                  )
                ]
                (eApp
                  tInt32
                  (eApp
                    (tInt32 ~> tInt32)
                    (EVar (Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id"))
                    [ EVar (Label (tInt32 ~> tInt32) "f") ]
                  )
                  [ eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "id"))
                      [ ELit (PInt32 5) ]
                  ])
                )
          , eLet
              [
                ( Label (tInt32 ~> tInt32) "f"
                , eLam
                    [Label tInt32 "n"]
                    (EIf
                      (EOp2
                        (tBool, OEq)
                        (EVar (Label tInt32 "n"))
                        (ELit (PInt32 0))
                      )
                      (ELit (PInt32 1))
                      (eApp
                        tInt32
                        (EVar (Label (tInt32 ~> tInt32) "g"))
                        [ EVar (Label tInt32 "n") ]
                      ))
                )
              , ( Label (tInt32 ~> tInt32) "g"
                , eLam
                    [Label tInt32 "n"]
                    (eLet
                      [ ( Label tInt32 "m"
                        , EOp2
                            (tInt32, OSub)
                            (EVar (Label tInt32 "n"))
                            (ELit (PInt32 1))
                        )
                      ]
                      (eApp
                        tInt32
                        (EVar (Label (tInt32 ~> tInt32) "f"))
                        [ EVar (Label tInt32 "m") ]
                      ))
                )
              ]
              (eLet
                [
                  ( Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id.0"
                  , eLam [Label (tInt32 ~> tInt32) "x"] (EVar (Label (tInt32 ~> tInt32) "x"))
                  )
                , ( Label (tInt32 ~> tInt32) "id.1"
                  , eLam [Label tInt32 "x"] (EVar (Label tInt32 "x"))
                  )
                ]
                (eApp
                  tInt32
                  (eApp
                    (tInt32 ~> tInt32)
                    (EVar (Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id.0"))
                    [ EVar (Label (tInt32 ~> tInt32) "f") ]
                  )
                  [ eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "id.1"))
                      [ ELit (PInt32 5) ]
                  ])
                )
          )
        , ( eLet
              [
                ( Label (TCon "List" [TVar 0] ~> tInt32) "f"
                , eLam
                    [ Label (TCon "List" [TVar 0]) "x" ]
                    ( ePat
                        (EVar (Label (TCon "List" [TVar 0]) "x"))
                        [ clause [ Label (TCon "List" [TVar 0]) "Nil" ] (ELit (PInt32 0))
                        , clause
                            [ Label (TVar 0 ~> TCon "List" [TVar 0] ~> TCon "List" [TVar 0]) "Cons"
                            , Label (TVar 0) "a"
                            , Label (TCon "List" [TVar 0]) "as" ]
                            (EOp2
                              (tInt32, OAdd)
                              (ELit (PInt32 1))
                              (eApp
                                tInt32
                                (EVar (Label (TCon "List" [TVar 0] ~> tInt32) "f"))
                                [ EVar (Label (TCon "List" [TVar 0]) "as")
                                ]
                              )
                            )
                        ]
                    )
                )
              ]
              (eLet
                [
                  ( Label (TCon "List" [tInt32]) "xs"
                  , eApp
                      (TCon "List" [tInt32])
                      (EVar (Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"))
                      [ ELit (PInt32 5)
                      , EVar (Label (TCon "List" [tInt32]) "Nil")
                      ]
                  )
                ]
                (eLet
                  [
                    ( Label (TCon "List" [tInt32]) "ys"
                    , eApp
                        (TCon "List" [tInt32])
                        (EVar (Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"))
                        [ ELit (PInt32 4)
                        , EVar (Label (TCon "List" [tInt32]) "xs")
                        ]
                    )
                  ]
                  (eApp
                    tInt32
                    (EVar (Label (TCon "List" [tInt32] ~> tInt32) "f"))
                    [ EVar (Label (TCon "List" [tInt32]) "ys") ]
                  )
                )
              )
          , eLet
              [
                ( Label (TCon "List" [TVar 0] ~> tInt32) "f.0"
                , eLam
                    [ Label (TCon "List" [TVar 0]) "x" ]
                    ( ePat
                        (EVar (Label (TCon "List" [TVar 0]) "x"))
                        [ clause [ Label (TCon "List" [TVar 0]) "Nil" ] (ELit (PInt32 0))
                        , clause
                            [ Label (TVar 0 ~> TCon "List" [TVar 0] ~> TCon "List" [TVar 0]) "Cons"
                            , Label (TVar 0) "a"
                            , Label (TCon "List" [TVar 0]) "as" ]
                            (EOp2
                              (tInt32, OAdd)
                              (ELit (PInt32 1))
                              (eApp
                                tInt32
                                (EVar (Label (TCon "List" [TVar 0] ~> tInt32) "f.0"))
                                [ EVar (Label (TCon "List" [TVar 0]) "as")
                                ]
                              )
                            )
                        ]
                    )
                )
              , ( Label (TCon "List" [tInt32] ~> tInt32) "f.1"
                , eLam
                    [ Label (TCon "List" [tInt32]) "x" ]
                    ( ePat
                        (EVar (Label (TCon "List" [tInt32]) "x"))
                        [ clause [ Label (TCon "List" [tInt32]) "Nil" ] (ELit (PInt32 0))
                        , clause
                            [ Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"
                            , Label tInt32 "a"
                            , Label (TCon "List" [tInt32]) "as" ]
                            (EOp2
                              (tInt32, OAdd)
                              (ELit (PInt32 1))
                              (eApp
                                tInt32
                                (EVar (Label (TCon "List" [tInt32] ~> tInt32) "f.1"))
                                [ EVar (Label (TCon "List" [tInt32]) "as")
                                ]
                              )
                            )
                        ]
                    )
                )
              ]
              (eLet
                [
                  ( Label (TCon "List" [tInt32]) "xs"
                  , eApp
                      (TCon "List" [tInt32])
                      (EVar (Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"))
                      [ ELit (PInt32 5)
                      , EVar (Label (TCon "List" [tInt32]) "Nil")
                      ]
                  )
                ]
                (eLet
                  [
                    ( Label (TCon "List" [tInt32]) "ys"
                    , eApp
                        (TCon "List" [tInt32])
                        (EVar (Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"))
                        [ ELit (PInt32 4)
                        , EVar (Label (TCon "List" [tInt32]) "xs")
                        ]
                    )
                  ]
                  (eApp
                    tInt32
                    (EVar (Label (TCon "List" [tInt32] ~> tInt32) "f.1"))
                    [ EVar (Label (TCon "List" [tInt32]) "ys") ]
                  )
                )
              )
          )
--      , ( undefined
--        , undefined
--        )
      ]

--

simplifyLetsShouldEqual :: (Show t, Eq t) => Dictionary t -> Dictionary t -> SpecWith ()
simplifyLetsShouldEqual input result =
  it (trimStr is) (simplifyLets input == result)
  where
    is = "simplifyLets (" <> show input <> ") == " <> show result

testSimplifyLets :: SpecWith ()
testSimplifyLets =
  describe "simplifyLets" $ do
    mapM_ (uncurry simplifyLetsShouldEqual)
      [
        ( Map.fromList
            [
              ( "$fun.0",
                ( [Label tInt32 "x"]
                , eApp tInt32
                    (EVar (Label (tInt32 ~> tInt32) "g"))
                    [ EOp2
                        (tInt32, OAdd)
                        (EVar (Label tInt32 "x"))
                        (ELit (PInt32 1))
                    ]
                )
              )
            , ( "$fun.1",
                ( [Label tInt32 "y"]
                , EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt32 "y"))
                      (ELit (PInt32 100)))
                    (EVar (Label tInt32 "n"))
                    (eApp tInt32
                      (EVar (Label (tInt32 ~> tInt32) "f"))
                      [ EVar (Label tInt32 "y") ])
                )
              )
            , ( "$fun.2",
                ( [Label (TVar 0) "x"]
                , EVar (Label (TVar 0) "x")
                )
              )
            , ( "$fun._",
                ( []
                , eLet
                    [
                      ( Label tInt32 "n"
                      , ELit (PInt32 500)
                      )
                    ]
                    (eLet
                      [
                        ( Label (tInt32 ~> tInt32) "f"
                        , EVar (Label (tInt32 ~> tInt32) "$fun.0")
                        )
                      ,
                        ( Label (tInt32 ~> tInt32) "g"
                        , EVar (Label (tInt32 ~> tInt32) "$fun.1")
                        )
                      ]
                      (eLet
                        [
                          ( Label (TVar 0 ~> TVar 0) "z"
                          , EVar (Label (TVar 0 ~> TVar 0) "$fun.2")
                          )
                        ]
                        (eApp tInt32
                          (EVar (Label (tInt32 ~> tInt32) "z"))
                          [ eApp tInt32
                              (EVar (Label (tInt32 ~> tInt32) "f"))
                              [ ELit (PInt32 1) ]
                          ])
                      ))
                    )
              )
            ]
          , Map.fromList
              [
                ( "$fun.0",
                  ( [Label tInt32 "x"]
                  , eApp tInt32
                      (EVar (Label (tInt32 ~> tInt32) "$fun.1"))
                      [ EOp2
                          (tInt32, OAdd)
                          (EVar (Label tInt32 "x"))
                          (ELit (PInt32 1))
                      ]
                  )
                )
              , ( "$fun.1",
                  ( [Label tInt32 "y"]
                  , EIf
                      (EOp2
                        (tBool, OEq)
                        (EVar (Label tInt32 "y"))
                        (ELit (PInt32 100)))
                      (EVar (Label tInt32 "n"))
                      (eApp tInt32
                        (EVar (Label (tInt32 ~> tInt32) "$fun.0"))
                        [ EVar (Label tInt32 "y") ])
                  )
                )
              , ( "$fun.2",
                  ( [Label (TVar 0) "x"]
                  , EVar (Label (TVar 0) "x")
                  )
                )
              , ( "$fun._",
                  ( []
                  , eLet
                      [
                        ( Label tInt32 "n"
                        , ELit (PInt32 500)
                        )
                      ]
                      (eApp tInt32
                        (EVar (Label (tInt32 ~> tInt32) "$fun.2"))
                        [ eApp tInt32
                            (EVar (Label (tInt32 ~> tInt32) "$fun.0"))
                            [ ELit (PInt32 1) ]
                        ])
                      )
                )
              ]
        )
      , ( Map.fromList
           [
             ( "$fun.0"
             , ( [Label tInt32 "n.2"]
               , EIf
                   (EOp2
                     (tBool, OEq)
                     (EVar (Label tInt32 "n.2"))
                     (ELit (PInt32 0))
                   )
                   (ELit (PInt32 1))
                   (eApp
                     tInt32
                     (EVar (Label (tInt32 ~> tInt32) "g.1"))
                     [ EVar (Label tInt32 "n.2") ]
                   )
               )
             )
           , ( "$fun.1"
             , ( [Label tInt32 "n.3"]
               , eLet
                   [ ( Label tInt32 "m.4"
                     , EOp2
                         (tInt32, OSub)
                         (EVar (Label tInt32 "n.3"))
                         (ELit (PInt32 1))
                     )
                   ]
                   (eApp
                     tInt32
                     (EVar (Label (tInt32 ~> tInt32) "f.0"))
                     [ EVar (Label tInt32 "m.4") ]
                   )
               )
             )
           , ( "$fun.2"
             , ( [Label (tInt32 ~> tInt32) "x.7"]
               , EVar (Label (tInt32 ~> tInt32) "x.7")
               )
             )
           , ( "$fun.3"
             , ( [Label tInt32 "x.8"]
               , EVar (Label tInt32 "x.8")
               )
             )
           , ( "$fun._"
             , ( []
               , eLet
                   [
                     ( Label (tInt32 ~> tInt32) "f.0"
                     , EVar (Label (tInt32 ~> tInt32) "$fun.0")
                     )
                   , ( Label (tInt32 ~> tInt32) "g.1"
                     , EVar (Label (tInt32 ~> tInt32) "$fun.1")
                     )
                   ]
                   (eLet
                     [
                       ( Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id.0.5"
                       , EVar (Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "$fun.2")
                       )
                     , ( Label (tInt32 ~> tInt32) "id.1.6"
                       , EVar (Label (tInt32 ~> tInt32) "$fun.3")
                       )
                     ]
                     (eApp
                       tInt32
                       (eApp
                         (tInt32 ~> tInt32)
                         (EVar (Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "id.0.5"))
                         [ EVar (Label (tInt32 ~> tInt32) "f.0") ]
                       )
                       [ eApp
                           tInt32
                           (EVar (Label (tInt32 ~> tInt32) "id.1.6"))
                           [ ELit (PInt32 5) ]
                       ])
                   )
               )
             )
           ]
        , Map.fromList
            [
              ( "$fun.0"
              , ( [Label tInt32 "n.2"]
                , EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt32 "n.2"))
                      (ELit (PInt32 0))
                    )
                    (ELit (PInt32 1))
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "$fun.1"))
                      [ EVar (Label tInt32 "n.2") ]
                    )
                )
              )
            , ( "$fun.1"
              , ( [Label tInt32 "n.3"]
                , eLet
                    [ ( Label tInt32 "m.4"
                      , EOp2
                          (tInt32, OSub)
                          (EVar (Label tInt32 "n.3"))
                          (ELit (PInt32 1))
                      )
                    ]
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "$fun.0"))
                      [ EVar (Label tInt32 "m.4") ]
                    )
                )
              )
            , ( "$fun.2"
              , ( [Label (tInt32 ~> tInt32) "x.7"]
                , EVar (Label (tInt32 ~> tInt32) "x.7")
                )
              )
            , ( "$fun.3"
              , ( [Label tInt32 "x.8"]
                , EVar (Label tInt32 "x.8")
                )
              )
            , ( "$fun._"
              , ( []
                , eApp
                    tInt32
                    (eApp
                      (tInt32 ~> tInt32)
                      (EVar (Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "$fun.2"))
                      [ EVar (Label (tInt32 ~> tInt32) "$fun.0") ]
                    )
                    [ eApp
                        tInt32
                        (EVar (Label (tInt32 ~> tInt32) "$fun.3"))
                        [ ELit (PInt32 5) ]
                    ]
                )
              )
            ]
        )
      ]

--

closeDefsShouldEqual :: Dictionary Type -> Dictionary Type -> SpecWith ()
closeDefsShouldEqual input result =
  forM_ (Map.keys result) $
    \key -> it (show key) (result ! key == closeDefs input ! key)

testCloseDefs :: SpecWith ()
testCloseDefs =
  describe "closeDefs " $ do
    mapM_ (uncurry closeDefsShouldEqual)
      [ ( Map.fromList
              [
                ( "$fun.0",
                  ( [Label tInt32 "x"]
                  , eApp tInt32
                      (EVar (Label (tInt32 ~> tInt32) "$fun.1"))
                      [ EOp2
                          (tInt32, OAdd)
                          (EVar (Label tInt32 "x"))
                          (ELit (PInt32 1))
                      ]
                  )
                )
              , ( "$fun.1",
                  ( [Label tInt32 "y"]
                  , EIf
                      (EOp2
                        (tBool, OEq)
                        (EVar (Label tInt32 "y"))
                        (ELit (PInt32 100)))
                      (EVar (Label tInt32 "n"))
                      (eApp tInt32
                        (EVar (Label (tInt32 ~> tInt32) "$fun.0"))
                        [ EVar (Label tInt32 "y") ])
                  )
                )
              , ( "$fun.2",
                  ( [Label (TVar 0) "x"]
                  , EVar (Label (TVar 0) "x")
                  )
                )
              , ( "$fun._",
                  ( []
                  , eLet
                      [
                        ( Label tInt32 "n"
                        , ELit (PInt32 500)
                        )
                      ]
                      (eApp tInt32
                        (EVar (Label (tInt32 ~> tInt32) "$fun.2"))
                        [ eApp tInt32
                            (EVar (Label (tInt32 ~> tInt32) "$fun.0"))
                            [ ELit (PInt32 1) ]
                        ])
                      )
                )
              ]
        , Map.fromList
              [
                ( "$fun.0",
                  ( [Label tInt32 "n", Label tInt32 "x"]
                  , eApp tInt32
                      (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "$fun.1"))
                      [ EVar (Label tInt32 "n")
                      , EOp2
                          (tInt32, OAdd)
                          (EVar (Label tInt32 "x"))
                          (ELit (PInt32 1))
                      ]
                  )
                )
              , ( "$fun.1",
                  ( [Label tInt32 "n", Label tInt32 "y"]
                  , EIf
                      (EOp2
                        (tBool, OEq)
                        (EVar (Label tInt32 "y"))
                        (ELit (PInt32 100)))
                      (EVar (Label tInt32 "n"))
                      (eApp tInt32
                        (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "$fun.0"))
                        [ EVar (Label tInt32 "n")
                        , EVar (Label tInt32 "y")
                        ])
                  )
                )
              , ( "$fun.2",
                  ( [Label (TVar 0) "x"]
                  , EVar (Label (TVar 0) "x")
                  )
                )
              , ( "$fun._",
                  ( []
                  , eLet
                      [
                        ( Label tInt32 "n"
                        , ELit (PInt32 500)
                        )
                      ]
                      (eApp tInt32
                        (EVar (Label (tInt32 ~> tInt32) "$fun.2"))
                        [ eApp tInt32
                            (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "$fun.0"))
                            [ EVar (Label tInt32 "n")
                            , ELit (PInt32 1)
                            ]
                        ])
                      )
                )
              ]
        )
      ]

--

flattenAppsShouldEqual :: Expr () -> Expr () -> SpecWith ()
flattenAppsShouldEqual input result =
  it (trimStr is) (flattenApps input == result)
  where
    is = "flattenApps (" <> show input <> ") == " <> show result

testFlattenApps :: SpecWith ()
testFlattenApps =
  describe "flattenApps" $ do
    mapM_ (uncurry flattenAppsShouldEqual)
      [ ( -- (f(1))(2)
          eApp () (eApp () (EVar (Label () "f")) [ ELit (PInt32 1) ]) [ ELit (PInt32 2) ]
        , -- f(1, 2)
          eApp () (EVar (Label () "f")) [ ELit (PInt32 1), ELit (PInt32 2) ]
        )
      ]

--

addImplicitArgsShouldEqual :: Dictionary Type -> Dictionary Type -> SpecWith ()
addImplicitArgsShouldEqual input result =
  it (trimStr is) (addImplicitArgs input == result)
  where
    is = "addImplicitArgs (" <> show input <> ") == " <> show result

testAddImplicitArgs :: SpecWith ()
testAddImplicitArgs =
  describe "addImplicitArgs" $ do
    mapM_ (uncurry addImplicitArgsShouldEqual)
      [
        ( Map.fromList
            [
              ( "$fun.0",
                ( [ Label tUnit "u" ]
                , EVar (Label (tBool ~> tInt32 ~> tChar) "f")
                )
              )
            ]
        , Map.fromList
            [
              ( "$fun.0",
                ( [ Label tUnit "u", Label tBool "$v.0", Label tInt32 "$v.1" ]
                , eApp
                    tChar
                    (EVar (Label (tBool ~> tInt32 ~> tChar) "f"))
                    [ EVar (Label tBool "$v.0")
                    , EVar (Label tInt32 "$v.1")
                    ]
                )
              )
            ]
        )
      , ( Map.fromList
            [
              ( "$fun.0",
                ( []
                , ELam (Label tInt32 "x" :| []) (EVar (Label tInt32 "x"))
                )
              )
            ]
        , Map.fromList
            [
              ( "$fun.0",
                ( [ Label tInt32 "$v.0" ]
                , EVar (Label tInt32 "$v.0")
                )
              )
            ]
        )
      ,
        ( Map.fromList
            [
              ( "$fun.0",
                ( []
                , ELam (Label tInt32 "x" <| Label tInt32 "y" :| []) (EVar (Label tInt32 "x"))
                )
              )
            ]
        , Map.fromList
            [
              ( "$fun.0",
                ( [ Label tInt32 "$v.0"
                  , Label tInt32 "$v.1"
                  ]
                , EVar (Label tInt32 "$v.0")
                )
              )
            ]
        )
      ,
        ( Map.fromList
            [
              ( "$fun.0",
                ( []
                , ELam (Label tInt32 "x" <| Label tInt32 "y" :| []) (EVar (Label tInt32 "y"))
                )
              )
            ]
        , Map.fromList
            [
              ( "$fun.0",
                ( [ Label tInt32 "$v.0"
                  , Label tInt32 "$v.1"
                  ]
                , EVar (Label tInt32 "$v.1")
                )
              )
            ]
        )
      ,
        ( Map.fromList
            [
              ( "$fun.0",
                ( []
                , EVar (Label (tInt32 ~> tInt32) "f")
                )
              )
            ]
        , Map.fromList
            [
              ( "$fun.0",
                ( [Label tInt32 "$v.0"]
                , eApp tInt32 (EVar (Label (tInt32 ~> tInt32) "f")) [EVar (Label tInt32 "$v.0")]
                )
              )
            ]
        )
      ,
        ( Map.fromList
            [
              ( "$fun.0",
                ( []
                , eApp (tInt32 ~> tInt32) (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "g")) [EVar (Label tInt32 "$x")]
                )
              )
            ]
        , Map.fromList
            [
              ( "$fun.0",
                ( [Label tInt32 "$v.0"]
                , eApp tInt32 (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "g"))
                    [ EVar (Label tInt32 "$x")
                    , EVar (Label tInt32 "$v.0")
                    ]
                )
              )
            ]
        )
      , ( Map.fromList
            [
              ( "$fun.0"
              , ( [Label tInt32 "n.2"]
                , EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt32 "n.2"))
                      (ELit (PInt32 0))
                    )
                    (ELit (PInt32 1))
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "$fun.1"))
                      [ EVar (Label tInt32 "n.2") ]
                    )
                )
              )
            , ( "$fun.1"
              , ( [Label tInt32 "n.3"]
                , eLet
                    [ ( Label tInt32 "m.4"
                      , EOp2
                          (tInt32, OSub)
                          (EVar (Label tInt32 "n.3"))
                          (ELit (PInt32 1))
                      )
                    ]
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "$fun.0"))
                      [ EVar (Label tInt32 "m.4") ]
                    )
                )
              )
            , ( "$fun.2"
              , ( [Label (tInt32 ~> tInt32) "x.7"]
                , EVar (Label (tInt32 ~> tInt32) "x.7")
                )
              )
            , ( "$fun.3"
              , ( [Label tInt32 "x.8"]
                , EVar (Label tInt32 "x.8")
                )
              )
            , ( "$fun._"
              , ( []
                , eApp
                    tInt32
                    (eApp
                      (tInt32 ~> tInt32)
                      (EVar (Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "$fun.2"))
                      [ EVar (Label (tInt32 ~> tInt32) "$fun.0") ]
                    )
                    [ eApp
                        tInt32
                        (EVar (Label (tInt32 ~> tInt32) "$fun.3"))
                        [ ELit (PInt32 5) ]
                    ]
                )
              )
            ]
        , Map.fromList
            [
              ( "$fun.0"
              , ( [Label tInt32 "n.2"]
                , EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt32 "n.2"))
                      (ELit (PInt32 0))
                    )
                    (ELit (PInt32 1))
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "$fun.1"))
                      [ EVar (Label tInt32 "n.2") ]
                    )
                )
              )
            , ( "$fun.1"
              , ( [Label tInt32 "n.3"]
                , eLet
                    [ ( Label tInt32 "m.4"
                      , EOp2
                          (tInt32, OSub)
                          (EVar (Label tInt32 "n.3"))
                          (ELit (PInt32 1))
                      )
                    ]
                    (eApp
                      tInt32
                      (EVar (Label (tInt32 ~> tInt32) "$fun.0"))
                      [ EVar (Label tInt32 "m.4") ]
                    )
                )
              )
            , ( "$fun.2"
              , ( [Label (tInt32 ~> tInt32) "x.7", Label tInt32 "$v.0"]
                , eApp
                    tInt32
                    (EVar (Label (tInt32 ~> tInt32) "x.7"))
                    [ EVar (Label tInt32 "$v.0") ]
                )
              )
            , ( "$fun.3"
              , ( [Label tInt32 "x.8"]
                , EVar (Label tInt32 "x.8")
                )
              )
            , ( "$fun._"
              , ( []
                , eApp
                    tInt32
                    (eApp
                      (tInt32 ~> tInt32)
                      (EVar (Label ((tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "$fun.2"))
                      [ EVar (Label (tInt32 ~> tInt32) "$fun.0") ]
                    )
                    [ eApp
                        tInt32
                        (EVar (Label (tInt32 ~> tInt32) "$fun.3"))
                        [ ELit (PInt32 5) ]
                    ]
                )
              )
            ]
        )
      ]

--

pipeline :: Expr () -> Either String (Dictionary Type)
pipeline e = typeExpr testTypeEnv e <&> steps
  where
    steps = zeroIndexed
      >>> monomorphize
      >>> qualifyNames
      >>> dictionary
      >>> simplifyLets
      >>> closeDefs
      >>> addImplicitArgs

-- let
--   f =
--     lam(n) =>
--       if n == 0
--         then 1
--         else n * g(n) ;
--   g =
--     lam(n) =>
--       let
--         m =
--           n - 1
--         in
--           f(m)
--   in
--     let
--       id =
--         lam(x) => x
--       in
--         (id(f))(id(5))
progI :: Expr ()
progI =
  eLet
    [
      ( Label () "f"
      , eLam
          [Label () "n"]
          (EIf
            (EOp2
              ((), OEq)
              (EVar (Label () "n"))
              (ELit (PInt32 0))
            )
            (ELit (PInt32 1))
            (EOp2
              ((), OMul)
              (EVar (Label () "n"))
              (eApp
                ()
                (EVar (Label () "g"))
                [ EVar (Label () "n") ]
              ))
          )
      )
    , ( Label () "g"
      , eLam
          [Label () "n"]
          (eLet
            [ ( Label () "m"
              , EOp2
                  ((), OSub)
                  (EVar (Label () "n"))
                  (ELit (PInt32 1))
              )
            ]
            (eApp
              ()
              (EVar (Label () "f"))
              [ EVar (Label () "m") ]
            ))
      )
    ]
    (eLet
      [
        ( Label () "id"
        , eLam [Label () "x"] (EVar (Label () "x"))
        )
      ]
      (eApp
        ()
        (eApp
          ()
          (EVar (Label () "id"))
          [ EVar (Label () "f") ]
        )
        [ eApp
            ()
            (EVar (Label () "id"))
            [ ELit (PInt32 10) ]
        ])
      )

--
-- let
--   f =
--     lam(x) =>
--       match x {
--         | Nil => 0
--         | Cons(a, as) => 1 + f(as)
--       }
--   in
--     let
--       xs =
--         Cons(5, Nil)
--       in
--         let
--           ys =
--             Cons(4, xs)
--           in
--             f(ys)
--
progJ :: Expr ()
progJ =
  eLet
    [
      ( Label () "f"
      , eLam
          [ Label () "x" ]
          ( ePat
              (EVar (Label () "x"))
              [ clause
                  [ Label () "Cons"
                  , Label () "a"
                  , Label () "as" ]
                  (EOp2
                    ((), OAdd)
                    (ELit (PInt32 1))
                    (eApp
                      ()
                      (EVar (Label () "f"))
                      [ EVar (Label () "as")
                      ]
                    )
                  )
              , clause
                  [ Label () "Nil" ]
                  (ELit (PInt32 0))
              ]
          )
      )
    ]
    (eLet
      [
        ( Label () "xs"
        , eApp
            ()
            (EVar (Label () "Cons"))
            [ ELit (PInt32 5)
            , EVar (Label () "Nil")
            ]
        )
      ]
      (eLet
        [
          ( Label () "ys"
          , eApp
              ()
              (EVar (Label () "Cons"))
              [ ELit (PInt32 4)
              , EVar (Label () "xs")
              ]
          )
        ]
        (eApp
          ()
          (EVar (Label () "f"))
          [ EVar (Label () "ys") ]
        )
      )
    )

--
-- let
--   add =
--     lam(x, y) =>
--       x + y
--     in
--       let
--         succ =
--           add(1)
--         in
--           let
--             f =
--               lam(g) =>
--                 g(100)
--             in
--               f(succ)
--
progK :: Expr ()
progK =
  eLet
    [
      ( Label () "add"
      , eLam
          [ Label () "x"
          , Label () "y" ]
          (EOp2
            ((), OAdd)
            (EVar (Label () "x"))
            (EVar (Label () "y"))
          )
      )
    ]
    (eLet
      [
        ( Label () "succ"
        , eApp ()
            (EVar (Label () "add"))
            [ ELit (PInt32 1) ]
        )
      ]
      (eLet
        [
          ( Label () "f"
          , eLam
              [ Label () "g" ]
              (eApp ()
                (EVar (Label () "g"))
                [ ELit (PInt32 100) ]
              )
          )
        ]
        (eApp ()
          (EVar (Label () "f"))
          [ EVar (Label () "succ") ]
        )
      )
    )

--
-- field
--   { name = n | r } =
--     { name = "Plissken", id = 1 }
--   in
--     n
--
progL :: Expr ()
progL =
  EFocus
    (Focus (Label () "name") (Label () "n") (Label () "r"))
    (EExt "name" (ELit (PString "Plissken")) (EExt "id" (ELit (PInt32 1)) ENil))
    (EVar (Label () "n"))


--
-- field
--   { name = n | r } =
--     { name = "Plissken", id = 1 }
--   in
--     { name = "Snake" | r }
--
progL2 :: Expr ()
progL2 =
  EFocus
    (Focus (Label () "name") (Label () "n") (Label () "r"))
    (EExt "name" (ELit (PString "Plissken")) (EExt "id" (ELit (PInt32 1)) ENil))
    (EExt "name" (ELit (PString "Snake")) (EVar (Label () "r")))

-- let
--   z =
--     field
--       { name = n | r } =
--         { name = "Plissken", id = 1 }
--       in
--         { name = "Snake" | r }
--   in
--     field
--       { name = m | q } =
--         z
--       in
--         m
--
progL3 :: Expr ()
progL3 =
  eLet
    [
      ( Label () "z"
      , EFocus
        (Focus (Label () "name") (Label () "n") (Label () "r"))
        (EExt "name" (ELit (PString "Plissken")) (EExt "id" (ELit (PInt32 1)) ENil))
        (EExt "name" (ELit (PString "Snake")) (EVar (Label () "r")))
      )
    ]
    (EFocus
      (Focus (Label () "name") (Label () "m") (Label () "q"))
      (EVar (Label () "z"))
      (EVar (Label () "m")))
--      (ELit (PInt32 11111)))


--
-- field
--   { name = n | r } =
--     { name = "Plissken", id = 1 }
--   in
--     run[print(n)] 0
--
progL4 :: Expr ()
progL4 =
  EFocus
    (Focus (Label () "name") (Label () "n") (Label () "r"))
    (EExt "name" (ELit (PString "Plissken")) (EExt "id" (ELit (PInt32 1)) ENil))
    --(EExt "name" (ELit (PString "Snake")) (EVar (Label () "r")))
    (ECall (Label () "print_string") [EVar (Label () "n")] (ELit (PInt32 0)))


--
-- field
--   { name = n | r } =
--     { name = "Plissken", id = 1 }
--   in
--     r
--
progM :: Expr ()
progM =
  EFocus
    (Focus (Label () "name") (Label () "n") (Label () "r"))
    (EExt "name" (ELit (PString "Plissken")) (EExt "id" (ELit (PInt32 1)) ENil))
    (EVar (Label () "r"))


--
-- { id = 1 }
--
progM2 :: Expr ()
progM2 =
  eLet
    [
      ( Label () "g"
      , EExt "id" (ELit (PInt32 1)) ENil
      )
    ]
    (ELit (PInt32 108))

--
-- field
--   { id = a | r } =
--     { id = 123 }
--   in
--     a
--
progM3 :: Expr ()
progM3 =
  EFocus
    (Focus (Label () "id") (Label () "a") (Label () "r"))
    (EExt "id" (ELit (PInt32 123)) ENil)
    (EVar (Label () "a"))

--
-- let
--   g =
--     lam(y) => y + 1
--   in
--     field
--       { fun = f | r } =
--         { fun = lam(x) => x, id = 1 }
--       in
--         (f(g))(f(5))
--
progN :: Expr ()
progN =
  eLet
    [
      ( Label () "g"
      , eLam [Label () "y"]
          (EOp2 ((), OAdd) (EVar (Label () "y")) (ELit (PInt32 1)))
      )
    ]
    (EFocus
      (Focus (Label () "fun") (Label () "f") (Label () "r"))
      (EExt "fun" (eLam [Label () "x"] (EVar (Label () "x"))) (EExt "id" (ELit (PInt32 1)) ENil))
      (eApp ()
        (eApp () (EVar (Label () "f")) [ EVar (Label () "g") ])
        [ eApp () (EVar (Label () "f")) [ ELit (PInt32 5) ]
        ]
      )
    )

progP :: Expr ()
progP =
--  eLet
--    [
--      ( Label () "Nil"
--      , eApp
--          ()
--          (EVar (Label () "Nil"))
----          [ ELit (PInt32 2) ]
--          [ ]
--      )
--    ]
--    (eLet
--      [
--        ( Label () "Cons"
--        , eLam
--            [Label () "x", Label () "xs"]
--            (eApp
--              ()
--              (EVar (Label () "Cons"))
--              [ -- ELit (PInt32 1)
--                EVar (Label () "x")
--              , EVar (Label () "xs")
--              ])
--        )
--      ]
      eLet
        [
          ( Label () "xs"
          , eApp
              ()
              (EVar (Label () "Cons"))
              [ ELit (PInt32 5)
              , EVar (Label () "Nil")
              ]
          )
        ]
      (ELit (PInt32 9999))
--    )

progQ =
  eLet
    [
      ( Label () "xs"
      , eApp
          ()
          (EVar (Label () "Cons"))
          [ ELit (PInt32 5)
          , EVar (Label () "Nil")
          ]
      )
    ]
  (ePat
    (EVar (Label () "xs"))
    [ clause [Label () "Cons", Label () "y", Label () "ys"] (EVar (Label () "y"))
    , clause [Label () "Nil"] (ELit (PInt32 0))
    ]
  )

progO =
  eLet
  [
    ( Label () "r"
    , ENil
    )
  ]
  (ELit (PInt32 1))

runDictShouldEqual :: Expr () -> Value -> SpecWith ()
runDictShouldEqual prog result =
  case pipeline prog of
    Right dict -> do
      val <- runIO $ runDict dict
      it (trimStr is) (result == val)
    Left{} ->
      error "Unexpected error"
  where
    is = show prog

testPipeline :: SpecWith ()
testPipeline =
  describe "pipeline" $ do
    runDictShouldEqual progI (VPrim (PInt32 3628800))
    runDictShouldEqual progJ (VPrim (PInt32 2))
    runDictShouldEqual progK (VPrim (PInt32 101))
    runDictShouldEqual progL (VPrim (PString "Plissken"))
    runDictShouldEqual progL2 (VExt "name" (VPrim (PString "Snake")) (VExt "id" (VPrim (PInt32 1)) VNil))
    runDictShouldEqual progL3 (VPrim (PString "Snake"))
    runDictShouldEqual progM (VExt "id" (VPrim (PInt32 1)) VNil)
    runDictShouldEqual progN (VPrim (PInt32 6))

--

runMExprShouldEqual :: Expr () -> Value -> SpecWith ()
runMExprShouldEqual input result = do
  val <- runIO $ mapM runDict (pipeline input)
  it (trimStr is) (val == Right result)
  where
    is = "compileMExpr (" <> show input <> ") == " <> show result

testMExpr :: MExpr (Expr ())
testMExpr =
  match
    [ "u1", "u2", "u3" ]
    [
      ( [ MVar "f", MCon "Nil" [], MVar "ys"
        ]
      , Expr (ELit (PInt32 1))
      )
    ,
      ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Nil" []
        ]
      , Expr (ELit (PInt32 2))
      )
    ,
      ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Cons" [MVar "y", MVar "ys"]
        ]
      , Expr (EVar (Label () "y"))
      )
    ]
    Fail

testMExpr2 :: MExpr (Expr ())
testMExpr2 =
  match
    [ "u1", "u2", "u3" ]
    [
      ( [ MVar "f", MCon "Nil" [], MVar "ys"
        ]
      , Expr (ELit (PInt32 1))
      )
    ,
      ( [ MVar "f", MCon "Cons" [MLit (ELit (PInt32 3)), MVar "xs"], MCon "Nil" []
        ]
      , Expr (ELit (PInt32 2))
      )
    ,
      ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Cons" [MVar "y", MVar "ys"]
        ]
      , Expr (EVar (Label () "y"))
      )
    ,
      ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Nil" []
        ]
      , Expr (ELit (PInt32 789))
      )
    ]
    Fail

testMExpr6 :: MExpr (Expr ())
testMExpr6 =
  match
    [ "u1", "u2", "u3" ]
    [
      ( [ MVar "f", MCon "Nil" [], MVar "ys"
        ]
      , Expr (ELit (PInt32 1))
      )
    ,
      ( [ MVar "f", MCon "Cons" [MLit (ELit (PInt32 3)), MVar "xs"], MCon "Nil" []
        ]
      , Expr (ELit (PInt32 2))
      )
    ,
      ( [ MVar "f", MCon "Cons" [MLit (ELit (PInt32 4)), MVar "xs"], MCon "Nil" []
        ]
      , Expr (ELit (PInt32 3))
      )
    ,
      ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Cons" [MVar "y", MVar "ys"]
        ]
      , Expr (EVar (Label () "y"))
      )
    ,
      ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Nil" []
        ]
      , Expr (ELit (PInt32 789))
      )
    ]
    Fail

testMExpr3 :: MExpr (Expr ())
testMExpr3 =
  match
    [ "u1", "u2" ]
    [
      ( [ MLit (ELit (PInt32 3)), MVar "xs" ]
      , Expr (ELit (PInt32 2))
      )
    ,
      ( [ MVar "x", MVar "xs" ]
      , Expr (EVar (Label () "y"))
      )
    ]
    Fail

testMExpr5 :: MExpr (Expr ())
testMExpr5 =
  match
    [ "u1", "u2" ]
    [
      ( [ MLit (ELit (PInt32 3)), MVar "xs" ]
      , Expr (ELit (PInt32 2))
      )
    ]
    (match
      [ "u1", "u2" ]
      [
        ( [ MVar "x", MVar "xs" ]
        , Expr (EVar (Label () "y"))
        )
      ]
      Fail)

testCompileMExpr :: SpecWith ()
testCompileMExpr =
  describe "compileMExpr" $ do
    runMExprShouldEqual
      (eApp
        ()
        (eLam
          [Label () "u1", Label () "u2", Label () "u3"]
          (compileMExpr testMExpr))
        [ eApp () (EVar (Label () "Nil")) []
        , eApp () (EVar (Label () "Nil")) []
        , eApp () (EVar (Label () "Nil")) []
        ])
      (VPrim (PInt32 1))
    runMExprShouldEqual
      (eApp
        ()
        (eLam
          [Label () "u1", Label () "u2", Label () "u3"]
          (compileMExpr testMExpr))
        [ eApp () (EVar (Label () "Nil")) []
        , eApp () (EVar (Label () "Cons")) [ELit (PInt32 100), EVar (Label () "Nil")]
        , eApp () (EVar (Label () "Nil")) []
        ])
      (VPrim (PInt32 2))
    runMExprShouldEqual
      (eApp
        ()
        (eLam
          [Label () "u1", Label () "u2", Label () "u3"]
          (compileMExpr testMExpr))
        [ eApp () (EVar (Label () "Nil")) []
        , eApp () (EVar (Label () "Cons")) [ELit (PInt32 100), EVar (Label () "Nil")]
        , eApp () (EVar (Label () "Cons")) [ELit (PInt32 200), EVar (Label () "Nil")]
        ])
      (VPrim (PInt32 200))
    runMExprShouldEqual
      (eApp
        ()
        (eLam
          [Label () "u1", Label () "u2", Label () "u3"]
          (compileMExpr testMExpr2))
        [ eApp () (EVar (Label () "Nil")) []
        , eApp () (EVar (Label () "Nil")) []
        , eApp () (EVar (Label () "Nil")) []
        ])
      (VPrim (PInt32 1))
    runMExprShouldEqual
      (eApp
        ()
        (eLam
          [Label () "u1", Label () "u2", Label () "u3"]
          (compileMExpr testMExpr2))
        -- [] [100] []
        [ eApp () (EVar (Label () "Nil")) []
        , eApp () (EVar (Label () "Cons")) [ELit (PInt32 100), EVar (Label () "Nil")]
        , eApp () (EVar (Label () "Nil")) []
        ])
      (VPrim (PInt32 789))
    runMExprShouldEqual
      (eApp
        ()
        (eLam
          [Label () "u1", Label () "u2", Label () "u3"]
          (compileMExpr testMExpr2))
        -- [] [3] []
        [ eApp () (EVar (Label () "Nil")) []
        , eApp () (EVar (Label () "Cons")) [ELit (PInt32 3), EVar (Label () "Nil")]
        , eApp () (EVar (Label () "Nil")) []
        ])
      (VPrim (PInt32 2))
    runMExprShouldEqual
      (eApp
        ()
        (eLam
          [Label () "u1", Label () "u2", Label () "u3"]
          (compileMExpr testMExpr2))
        -- [] [100] [200]
        [ eApp () (EVar (Label () "Nil")) []
        , eApp () (EVar (Label () "Cons")) [ELit (PInt32 100), EVar (Label () "Nil")]
        , eApp () (EVar (Label () "Cons")) [ELit (PInt32 200), EVar (Label () "Nil")]
        ])
      (VPrim (PInt32 200))
    runMExprShouldEqual
      (eApp
        ()
        (eLam
          [Label () "u1", Label () "u2", Label () "u3"]
          (compileMExpr testMExpr6))
        -- [] [3] []
        [ eApp () (EVar (Label () "Nil")) []
        , eApp () (EVar (Label () "Cons")) [ELit (PInt32 3), EVar (Label () "Nil")]
        , eApp () (EVar (Label () "Nil")) []
        ])
      (VPrim (PInt32 2))
    runMExprShouldEqual
      (eApp
        ()
        (eLam
          [Label () "u1", Label () "u2", Label () "u3"]
          (compileMExpr testMExpr6))
        -- [] [3] []
        [ eApp () (EVar (Label () "Nil")) []
        , eApp () (EVar (Label () "Cons")) [ELit (PInt32 4), EVar (Label () "Nil")]
        , eApp () (EVar (Label () "Nil")) []
        ])
      (VPrim (PInt32 3))

--

testUnifyRowsShouldEqual :: Type -> Type -> Either String Bool -> SpecWith ()
testUnifyRowsShouldEqual r1 r2 result = it (trimStr is) (outcome == result)
  where
    is = "unifyRows " <> show (r1, r2) <> " == " <> show result
    outcome = case substitution . snd <$> runInferCount (m + 1) mempty (unify r1 r2) of
      Right sub ->
        let update = normalizeRow . apply sub
         in Right (update r1 == update r2)
      Left err -> Left err
    s = tvars r1 <> tvars r2
    m | Set.null s = 0
      | otherwise = maximum s

testUnifyRows :: SpecWith ()
testUnifyRows = do
  describe "unifyRows" $ do
    testUnifyRowsShouldEqual
      (RExt "name" [tString] (RExt "id" [tInt32] RNil))
      (RExt "name" [tString] (RExt "id" [tInt32] RNil))
      (Right True)
    testUnifyRowsShouldEqual
      (RExt "name" [tString] (RExt "id" [tInt32] (TVar 0)))
      (RExt "name" [tString] (RExt "id" [tInt32] RNil))
      (Right True)
    testUnifyRowsShouldEqual
      (RExt "id" [tInt32] (RExt "name" [tString] RNil))
      (RExt "name" [tString] (RExt "id" [tInt32] RNil))
      (Right True)
    testUnifyRowsShouldEqual
      (RExt "id" [tInt32] (RExt "name" [tString] (TVar 0)))
      (RExt "name" [tString] (RExt "id" [tInt32] RNil))
      (Right True)
    testUnifyRowsShouldEqual
      (RExt "id" [tInt32] (TVar 0))
      (RExt "name" [tString] (RExt "id" [tInt32] RNil))
      (Right True)
    testUnifyRowsShouldEqual
      (RExt "id" [tString] (TVar 0))
      (RExt "name" [tString] (RExt "id" [tInt32] RNil))
      (Left "Cannot unify")
    testUnifyRowsShouldEqual
      -- { name : string, admin : bool, id : int32 }
      (RExt "name" [tString] (RExt "admin" [tBool] (RExt "id" [tInt32] RNil)))
      -- { name : string, id : int32 | 0 }
      (RExt "name" [tString] (RExt "id" [tInt32] (TVar 0)))
      (Right True)
    testUnifyRowsShouldEqual
      -- { name : string, admin : bool, id : int32 }
      (RExt "admin" [tBool] (RExt "id" [tInt32] RNil))
      -- { name : string, id : int32 | 0 }
      (RExt "id" [tInt32] (TVar 0))
      (Right True)
    testUnifyRowsShouldEqual
      -- { name : string | 0 }
      (RExt "name" [tString] (TVar 0))
      -- { id : int | 1 }
      (RExt "id" [tInt32] (TVar 1))
      (Right True)
    testUnifyRowsShouldEqual
      -- { name : string | 0 }
      (RExt "name" [tString] (TVar 0))
      -- { id : int | 0 }
      (RExt "id" [tInt32] (TVar 0))
      (Left "Cannot unify")


-- convertClosuresShouldEqual :: Expr () -> Expr () -> SpecWith ()
-- convertClosuresShouldEqual input result =
--   it (trimStr is) (convertClosures input == result)
--   where
--     is = "convertClosures (" <> show input <> ") == " <> show result
--
-- testconvertClosures :: SpecWith ()
-- testconvertClosures =
--   describe "convertClosures" $ do
--     mapM_ (uncurry convertClosuresShouldEqual)
--       [ -- let f = lam(y) => x + y in f(1)
--         ( eLet
--             [
--               ( Label () "f"
--               , eLam
--                   [Label () "y"]
--                   (EOp2
--                     ((), OAdd)
--                     (EVar (Label () "x"))
--                     (EVar (Label () "y")))
--               )
--             ]
--             (eApp
--               ()
--               (EVar (Label () "f"))
--               [ELit (PInt32 1)])
--         -- let f = lam($x, y) => $x + y in f(x, 1)
--         , eLet
--             [
--               ( Label () "f"
--               , eLam
--                   [Label () "$x", Label () "y"]
--                   (EOp2
--                     ((), OAdd)
--                     (EVar (Label () "$x"))
--                     (EVar (Label () "y")))
--               )
--             ]
--             (eApp
--               ()
--               (EVar (Label () "f"))
--               [EVar (Label () "x"), ELit (PInt32 1)])
--         )
--         -- let x = 1 in let f = lam(y) => x + y in f(1)
--       , ( eLet
--             [
--               ( Label () "x"
--               , ELit (PInt32 1)
--               )
--             ]
--             (eLet
--               [
--                 ( Label () "f"
--                 , eLam
--                     [Label () "y"]
--                     (EOp2
--                       ((), OAdd)
--                       (EVar (Label () "x"))
--                       (EVar (Label () "y")))
--                 )
--               ]
--               (eApp
--                 ()
--                 (EVar (Label () "f"))
--                 [ELit (PInt32 1)]))
--         -- let x = 1 in let f = lam($x, y) => $x + y in f(x, 1)
--         , eLet
--             [
--               ( Label () "x"
--               , ELit (PInt32 1)
--               )
--             ]
--             (eLet
--               [
--                 ( Label () "f"
--                 , eLam
--                     [Label () "$x", Label () "y"]
--                     (EOp2
--                       ((), OAdd)
--                       (EVar (Label () "$x"))
--                       (EVar (Label () "y")))
--                 )
--               ]
--               (eApp
--                 ()
--                 (EVar (Label () "f"))
--                 [EVar (Label () "x"), ELit (PInt32 1)]))
--         )
--       , -- let f = lam(x, y) => x + y in let g = f(1) in g(2)
--         ( eLet
--             [
--               ( Label () "f"
--               , eLam
--                   [Label () "x", Label () "y"]
--                   (EOp2
--                     ((), OAdd)
--                     (EVar (Label () "x"))
--                     (EVar (Label () "y")))
--               )
--             ]
--             (eLet
--               [
--                 ( Label () "g"
--                 , eApp
--                     ()
--                     (EVar (Label () "f"))
--                     [ELit (PInt32 1)]
--                 )
--               ]
--               (eApp
--                 ()
--                 (EVar (Label () "g"))
--                 [ELit (PInt32 2)]))
--         -- let f = lam(x, y) => x + y in let g = lam($f) => $f(1) in g(f, 2)
--         , eLet
--             [
--               ( Label () "f"
--               , eLam
--                   [Label () "x", Label () "y"]
--                   (EOp2
--                     ((), OAdd)
--                     (EVar (Label () "x"))
--                     (EVar (Label () "y")))
--               )
--             ]
--             (eLet
--               [
--                 ( Label () "g"
--                 , eLam
--                     [Label () "$f"]
--                     (eApp
--                       ()
--                       (EVar (Label () "$f"))
--                       [ELit (PInt32 1)])
--                 )
--               ]
--               (eApp
--                 ()
--                 (EVar (Label () "g"))
--                 [EVar (Label () "f"), ELit (PInt32 2)]))
--         )
--       , -- let n = 500 in let f = lam(x) => g(x + 1); g = lam(y) => if y == 100 then n else f(y) in f(1)
--         ( eLet
--             [
--               ( Label () "n"
--               , ELit (PInt32 500)
--               )
--             ]
--             (eLet
--               [
--                 ( Label () "f"
--                 , eLam [Label () "x"]
--                     (eApp ()
--                       (EVar (Label () "g"))
--                       [ EOp2
--                           ((), OAdd)
--                           (EVar (Label () "x"))
--                           (ELit (PInt32 1))
--                       ])
--                 )
--               ,
--                 ( Label () "g"
--                 , eLam [Label () "y"]
--                     (EIf
--                       (EOp2
--                         ((), OEq)
--                         (EVar (Label () "y"))
--                         (ELit (PInt32 100)))
--                       (EVar (Label () "n"))
--                       (eApp ()
--                         (EVar (Label () "f"))
--                         [ EVar (Label () "y") ]))
--                 )
--               ]
--               (eApp ()
--                 (EVar (Label () "f"))
--                 [ ELit (PInt32 1) ]))
--         , -- let n = 500 in let f = lam($g, x) => $g(x + 1); g = lam($f, $n, y) => if y == 100 then $n else $f(y) in f(g(f, n), 1)
--           eLet
--             [
--               ( Label () "n"
--               , ELit (PInt32 500)
--               )
--             ]
--             (eLet
--               [
--                 ( Label () "f"
--                 , eLam [Label () "x"]
--                     (eApp ()
--                       (EVar (Label () "g"))
--                       [ EOp2
--                           ((), OAdd)
--                           (EVar (Label () "x"))
--                           (ELit (PInt32 1))
--                       ])
--                 )
--               ,
--                 ( Label () "g"
--                 , eLam [Label () "y"]
--                     (EIf
--                       (EOp2
--                         ((), OEq)
--                         (EVar (Label () "y"))
--                         (ELit (PInt32 100)))
--                       (EVar (Label () "n"))
--                       (eApp ()
--                         (EVar (Label () "f"))
--                         [ EVar (Label () "y") ]))
--                 )
--               ]
--               (eApp ()
--                 (EVar (Label () "f"))
--                 [ ELit (PInt32 1) ]))
--         )
-- --      ,
-- --        ( undefined
-- --        , undefined
-- --        )
--       ]
--
-- --
--
-- insertImplicitArgsShouldEqual :: Expr Type -> Expr Type -> SpecWith ()
-- insertImplicitArgsShouldEqual input result =
--   it (trimStr is) (insertImplicitArgs input == result)
--   where
--     is = "insertImplicitArgs (" <> show input <> ") == " <> show result
--
-- testInsertImplicitArgs :: SpecWith ()
-- testInsertImplicitArgs =
--   describe "insertImplicitArgs" $ do
--     mapM_ (uncurry insertImplicitArgsShouldEqual)
--       [ -- let f.0 = lam(x, y) => x + y in let g.0 = lam($f) => $f(1) in g.0(f.0, 2)
--         ( eLet
--             [
--               ( Label (tInt32 ~> tInt32 ~> tInt32) "f.0"
--               , eLam
--                   [Label tInt32 "x", Label tInt32 "y"]
--                   (EOp2
--                     (tInt32, OAdd)
--                     (EVar (Label tInt32 "x"))
--                     (EVar (Label tInt32 "y")))
--               )
--             ]
--             (eLet
--               [
--                 ( Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "g.0"
--                 , eLam
--                     [Label (tInt32 ~> tInt32 ~> tInt32) "$f"]
--                     (eApp
--                       (tInt32 ~> tInt32)
--                       (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "$f"))
--                       [ELit (PInt32 1)])
--                 )
--               ]
--               (eApp
--                 tInt32
--                 (EVar (Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "g.0"))
--                 [EVar (Label (tInt32 ~> tInt32 ~> tInt32) "f.0"), ELit (PInt32 2)]))
--         -- let f.0 = lam(x, y) => x + y in let g.0 = lam($f, $.0) => $f(1, $.0) in g.0(f.0, 2)
--         , eLet
--             [
--               ( Label (tInt32 ~> tInt32 ~> tInt32) "f.0"
--               , eLam
--                   [Label tInt32 "x", Label tInt32 "y"]
--                   (EOp2
--                     (tInt32, OAdd)
--                     (EVar (Label tInt32 "x"))
--                     (EVar (Label tInt32 "y")))
--               )
--             ]
--             (eLet
--               [
--                 ( Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "g.0"
--                 , eLam
--                     [Label (tInt32 ~> tInt32 ~> tInt32) "$f", Label tInt32 "$.0"]
--                     (eApp
--                       tInt32
--                       (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "$f"))
--                       [ELit (PInt32 1), EVar (Label tInt32 "$.0")])
--                 )
--               ]
--               (eApp
--                 tInt32
--                 (EVar (Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "g.0"))
--                 [EVar (Label (tInt32 ~> tInt32 ~> tInt32) "f.0"), ELit (PInt32 2)]))
--         )
--       ]
--
-- --
--
-- flattenLetsShouldEqual :: (Show t, Eq t) => Expr t -> Expr t -> SpecWith ()
-- flattenLetsShouldEqual input result =
--   it (trimStr is) (flattenLets input == result)
--   where
--     is = "flattenLets (" <> show input <> ") == " <> show result
--
-- testFlattenLets :: SpecWith ()
-- testFlattenLets =
--   describe "flattenLets" $ do
--     mapM_ (uncurry flattenLetsShouldEqual)
--       [ -- let a = b in a + b
--         ( eLet [(Label () "a", EVar (Label () "b"))] (EOp2 ((), OAdd) (EVar (Label () "a")) (EVar (Label () "b")))
--         -- b + b
--         , EOp2 ((), OAdd) (EVar (Label () "b")) (EVar (Label () "b"))
--         )
--       ]
--
-- --
--
-- toMap :: [Definition a] -> Map Name ([Label Type], a)
-- toMap = Map.fromList . fmap (\(Def name labels expr) -> (name, (labels, expr)))
--
-- liftLambdasShouldEqual :: Expr Type -> [Definition (Expr Type)] -> SpecWith ()
-- liftLambdasShouldEqual input result =
--   it (trimStr is) (toMap (liftLambdas input) == toMap result)
--   where
--     is = "liftLambdas (" <> show input <> ") == " <> show result
--
-- testLiftLambdas :: SpecWith ()
-- testLiftLambdas =
--   describe "liftLambdas" $ do
--     mapM_ (uncurry liftLambdasShouldEqual)
--       [
--         ( eLet
--             [
--               ( Label (tInt32 ~> tInt32 ~> tInt32) "f.0"
--               , eLam
--                   [Label tInt32 "x", Label tInt32 "y"]
--                   (EOp2
--                     (tInt32, OAdd)
--                     (EVar (Label tInt32 "x"))
--                     (EVar (Label tInt32 "y")))
--               )
--             ]
--             (eLet
--               [
--                 ( Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "g.0"
--                 , eLam
--                     [Label (tInt32 ~> tInt32 ~> tInt32) "$f", Label tInt32 "$.0"]
--                     (eApp
--                       tInt32
--                       (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "$f"))
--                       [ELit (PInt32 1), EVar (Label tInt32 "$.0")])
--                 )
--               ]
--               (eApp
--                 tInt32
--                 (EVar (Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "g.0"))
--                 [EVar (Label (tInt32 ~> tInt32 ~> tInt32) "f.0"), ELit (PInt32 2)]))
--         , [ Def "_" [] (eApp tInt32 (EVar (Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "$fun.1")) [EVar (Label (tInt32 ~> tInt32 ~> tInt32) "$fun.0"), ELit (PInt32 2)])
--           , Def "$fun.0" [Label tInt32 "x", Label tInt32 "y"] (EOp2 (tInt32, OAdd) (EVar (Label tInt32 "x")) (EVar (Label tInt32 "y")))
--           , Def "$fun.1" [Label (tInt32 ~> tInt32 ~> tInt32) "$f", Label tInt32 "$.0"] (eApp tInt32 (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "$f")) [ELit (PInt32 1), EVar (Label tInt32 "$.0")])
--           ]
--         )
--       ,
--         ( eLet
--             [
--               ( Label (tBool ~> tBool) "id.0"
--               , eLam
--                   [Label tBool "x"]
--                   (EVar (Label tBool "x"))
--               )
--             ]
--             (eLet
--               [
--                 ( Label (tInt32 ~> tInt32) "id.1"
--                 , eLam
--                     [Label tInt32 "x"]
--                     (EVar (Label tInt32 "x"))
--                 )
--               ]
--               (EIf
--                 (eApp tBool
--                   (EVar (Label (tBool ~> tBool) "id.0"))
--                   [ELit (PBool True)])
--                 (eApp tInt32
--                   (EVar (Label (tInt32 ~> tInt32) "id.1"))
--                   [ELit (PInt32 1)])
--                 (ELit (PInt32 2))))
--         , [ Def "_" []
--               (EIf
--                 (eApp tBool
--                   (EVar (Label (tBool ~> tBool) "$fun.0"))
--                   [ELit (PBool True)])
--                 (eApp tInt32
--                   (EVar (Label (tInt32 ~> tInt32) "$fun.1"))
--                   [ELit (PInt32 1)])
--                 (ELit (PInt32 2)))
--           , Def "$fun.0" [Label (TCon "Bool" []) "x"] (EVar (Label (TCon "Bool" []) "x"))
--           , Def "$fun.1" [Label (TCon "Int" []) "x"] (EVar (Label (TCon "Int" []) "x"))
--           ]
--         )
--       , ( eLet
--             [
--               ( Label (TCon "List" [tInt32] ~> tInt32) "f"
--               , eLam
--                   [Label (TCon "List" [tInt32]) "x"]
--                   (ePat
--                     (EVar (Label (TCon "List" [tInt32]) "x"))
--                     [ clause [Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons", Label tInt32 "y", Label (TCon "List" [tInt32]) "ys"] (EVar (Label tInt32 "y"))
--                     , clause [Label (TCon "List" [tInt32]) "Nil"] (ELit (PInt32 0))
--                     ])
--               )
--             ]
--             (eApp
--               tInt32
--               (EVar (Label (TCon "List" [tInt32] ~> tInt32) "f"))
--               [ eApp
--                   (TCon "List" [tInt32])
--                   (EVar (Label (tInt32 ~> TCon "List" [tInt32] ~> TCon "List" [tInt32]) "Cons"))
--                   [ ELit (PInt32 5)
--                   , EVar (Label (TCon "List" [tInt32]) "Nil")
--                   ]
--             ])
--         , [ Def "_" [] (eApp tInt32 (EVar (Label (TCon "List" [TCon "Int" []] ~> TCon "Int" []) "$fun.0"))
--               [ eApp
--                   (TCon "List" [TCon "Int" []])
--                   (EVar (Label (tInt32 ~> TCon "List" [TCon "Int" []] ~> TCon "List" [TCon "Int" []]) "Cons"))
--                   [ELit (PInt32 5), EVar (Label (TCon "List" [TCon "Int" []]) "Nil")]
--               ])
--           , Def "$fun.0" [Label (TCon "List" [TCon "Int" []]) "x"]
--               (ePat
--                 (EVar (Label (TCon "List" [TCon "Int" []]) "x"))
--                 [ clause [Label (tInt32 ~> TCon "List" [TCon "Int" []] ~> TCon "List" [TCon "Int" []]) "Cons", Label tInt32 "y", Label (TCon "List" [TCon "Int" []]) "ys"] (EVar (Label tInt32 "y"))
--                 , clause [Label (TCon "List" [TCon "Int" []]) "Nil"] (ELit (PInt32 0))
--                 ])
--           ]
--         )
--       ]
--
-- --
--
-- toLExprShouldEqual :: [Definition (Expr Type)] -> [Definition LExpr] -> SpecWith ()
-- toLExprShouldEqual input result =
--   it (trimStr is) (toMap (toLExprs input) == toMap result)
--   where
--     is = "toLExpr (" <> show input <> ") == " <> show result
--
-- testToLExpr :: SpecWith ()
-- testToLExpr =
--   describe "toLExpr" $ do
--     mapM_ (uncurry toLExprShouldEqual)
--       [
--         (
--           [ Def "_" [] (eApp tInt32 (EVar (Label ((tInt32 ~> tInt32 ~> tInt32) ~> tInt32 ~> tInt32) "$fun.1")) [EVar (Label (tInt32 ~> tInt32 ~> tInt32) "$fun.0"), ELit (PInt32 2)])
--           , Def "$fun.0" [Label tInt32 "x", Label tInt32 "y"] (EOp2 (tInt32, OAdd) (EVar (Label tInt32 "x")) (EVar (Label tInt32 "y")))
--           , Def "$fun.1" [Label (tInt32 ~> tInt32 ~> tInt32) "$f", Label tInt32 "$.0"] (eApp tInt32 (EVar (Label (tInt32 ~> tInt32 ~> tInt32) "$f")) [ELit (PInt32 1), EVar (Label tInt32 "$.0")])
--           ]
--         , [ Def "_" [] (LCal (Label tInt32 "$fun.1") [LVar (Label (tInt32 ~> tInt32 ~> tInt32) "$fun.0"), LLit (PInt32 2)])
--           , Def "$fun.0" [Label tInt32 "x", Label tInt32 "y"] (LOp2 (tInt32, OAdd) (LVar (Label tInt32 "x")) (LVar (Label tInt32 "y")))
--           , Def "$fun.1" [Label (tInt32 ~> tInt32 ~> tInt32) "$f", Label tInt32 "$.0"] (LCal (Label tInt32 "$f") [LLit (PInt32 1), LVar (Label tInt32 "$.0")])
--           ]
--         )
--       , (
--           [ Def "_" [] (EIf
--               (eApp tBool
--                 (EVar (Label (tBool ~> tBool) "$fun.0"))
--                 [ELit (PBool True)])
--               (eApp tInt32
--                 (EVar (Label (tInt32 ~> tInt32) "$fun.1"))
--                 [ELit (PInt32 1)])
--               (ELit (PInt32 2)))
--           , Def "$fun.0" [Label (TCon "Bool" []) "x"] (EVar (Label (TCon "Bool" []) "x"))
--           , Def "$fun.1" [Label (TCon "Int" []) "x"] (EVar (Label (TCon "Int" []) "x"))
--           ]
--         , [ Def "_" []
--               (lPat
--                 (LCal
--                   (Label tBool "$fun.0")
--                   [LLit (PBool True)])
--                 [ ([Label tBool "True"],
--                     LCal
--                       (Label tInt32 "$fun.1")
--                       [LLit (PInt32 1)])
--                 , ([Label tBool "False"], LLit (PInt32 2))
--                 ])
--           , Def "$fun.0" [Label (TCon "Bool" []) "x"] (LVar (Label (TCon "Bool" []) "x"))
--           , Def "$fun.1" [Label (TCon "Int" []) "x"] (LVar (Label (TCon "Int" []) "x"))
--           ]
--         )
--       , (
--           [ Def "_" [] (eApp tInt32 (EVar (Label (TCon "List" [TCon "Int" []] ~> TCon "Int" []) "$fun.0"))
--               [ eApp
--                   (TCon "List" [TCon "Int" []])
--                   (EVar (Label (tInt32 ~> TCon "List" [TCon "Int" []] ~> TCon "List" [TCon "Int" []]) "Cons"))
--                   [ELit (PInt32 5), EVar (Label (TCon "List" [TCon "Int" []]) "Nil")]
--               ])
--           , Def "$fun.0" [Label (TCon "List" [TCon "Int" []]) "x"]
--               (ePat
--                 (EVar (Label (TCon "List" [TCon "Int" []]) "x"))
--                 [ clause [Label (tInt32 ~> TCon "List" [TCon "Int" []] ~> TCon "List" [TCon "Int" []]) "Cons", Label tInt32 "y", Label (TCon "List" [TCon "Int" []]) "ys"] (EVar (Label tInt32 "y"))
--                 , clause [Label (TCon "List" [TCon "Int" []]) "Nil"] (ELit (PInt32 0))
--                 ])
--           ]
--         , [ Def "_" [] (LCal (Label (TCon "Int" []) "$fun.0")
--               [ LCon
--                   (Label (TCon "List" [TCon "Int" []]) "Cons")
--                   [LLit (PInt32 5), LCon (Label (TCon "List" [TCon "Int" []]) "Nil") []]
--               ])
--           , Def "$fun.0" [Label (TCon "List" [TCon "Int" []]) "x"]
--               (lPat
--                 (LVar (Label (TCon "List" [TCon "Int" []]) "x"))
--                 [ ([Label (tInt32 ~> TCon "List" [TCon "Int" []] ~> TCon "List" [TCon "Int" []]) "Cons", Label tInt32 "y", Label (TCon "List" [TCon "Int" []]) "ys"], LVar (Label tInt32 "y"))
--                 , ([Label (TCon "List" [TCon "Int" []]) "Nil"], LLit (PInt32 0))
--                 ])
--           ]
--         )
--       ]
--
-- --
--
-- evalShouldEqual :: [Definition LExpr] -> Value -> SpecWith ()
-- evalShouldEqual input result =
--   it (trimStr is) (let e = unsafePerformIO (runEval env (eval expr)) in e == result)
--   where
--     is = "eval (" <> show input <> ") == " <> show result
--     expr = justLeft (envLookup "_" env)
--     env = foldr (\(Def k lls e) -> envInsert k (Left (lls, e))) mempty input
--     justLeft (Just (Left (_, v))) = v
--     justLeft _ = error "Implementation error"
--
-- testEval :: SpecWith ()
-- testEval =
--   describe "eval" $ do
--     mapM_ (uncurry evalShouldEqual)
--       [
--         (
--           [ Def "_" [] (LCal (Label tInt32 "$fun.1") [LVar (Label (tInt32 ~> tInt32 ~> tInt32) "$fun.0"), LLit (PInt32 2)])
--           , Def "$fun.0"
--               [Label tInt32 "x", Label tInt32 "y"]
--               (LOp2 (tInt32, OAdd) (LVar (Label tInt32 "x")) (LVar (Label tInt32 "y")))
--           , Def "$fun.1"
--               [Label (tInt32 ~> tInt32 ~> tInt32) "$f", Label tInt32 "$.0"]
--               (LCal (Label tInt32 "$f") [LLit (PInt32 1), LVar (Label tInt32 "$.0")])
--           ]
--         , VPrim (PInt32 3)
--         )
--       , ( [ Def "_" []
--               (lPat
--                 (LCal
--                   (Label tBool "$fun.0")
--                   [LLit (PBool True)])
--                 [ ([Label tBool "True"],
--                     LCal
--                       (Label tInt32 "$fun.1")
--                       [LLit (PInt32 1)])
--                 , ([Label tBool "False"], LLit (PInt32 2))
--                 ])
--           , Def "$fun.0"
--               [Label (TCon "Bool" []) "x"]
--               (LVar (Label (TCon "Bool" []) "x"))
--           , Def "$fun.1"
--               [Label (TCon "Int" []) "x"]
--               (LVar (Label (TCon "Int" []) "x"))
--           ]
--         , VPrim (PInt32 1)
--         )
--       , ( [ Def "_" [] (LCal (Label (TCon "Int" []) "$fun.0")
--               [ LCon
--                   (Label (TCon "List" [TCon "Int" []]) "Cons")
--                   [LLit (PInt32 5), LCon (Label (TCon "List" [TCon "Int" []]) "Nil") []]
--               ])
--           , Def "$fun.0" [Label (TCon "List" [TCon "Int" []]) "x"]
--               (lPat
--                 (LVar (Label (TCon "List" [TCon "Int" []]) "x"))
--                 [ ([Label (tInt32 ~> TCon "List" [TCon "Int" []] ~> TCon "List" [TCon "Int" []]) "Cons", Label tInt32 "y", Label (TCon "List" [TCon "Int" []]) "ys"], LVar (Label tInt32 "y"))
--                 , ([Label (TCon "List" [TCon "Int" []]) "Nil"], LLit (PInt32 0))
--                 ])
--           ]
--         , VPrim (PInt32 5)
--         )
--       , ( [ Def "_" [] (LCon (Label (tInt32 ~> TCon "Maybe" [TCon "Int" []]) "Just") [LLit (PInt32 1)])
--           ]
--         , VData "Just" [VPrim (PInt32 1)]
--         )
--       ]

main :: IO ()
main =
  hspec $ do
    testTvars
    testFreeVars
    testTagExpr
    testTypeOf
    testInferExpr
    testQualifyNames
    testSubstVar
    testDictionary
    testMonomorphize
    testSimplifyLets
    testCloseDefs
    testFlattenApps
    testAddImplicitArgs
    testPipeline
    testCompileMExpr
--    testUnify
    testUnifyRows

--    testFlattenLambdas
--    testconvertClosures
--    testInsertImplicitArgs
--    testFlattenLets
--    testLiftLambdas
--    testToLExpr
--    testEval

--
--data Some
--data None
--
--data Option c a
--  = Some a
--  | None
--  deriving (Show)
--
--some :: a -> Option Some a
--some = Some
--
--none :: Option None a
--none = None
--
--instance Functor (Option c) where
--  fmap f None = None
--  fmap f (Some v) = Some (f v)
--
--withDefault :: a -> Option c a -> a
--withDefault d None = d
--withDefault _ (Some v) = v
--
--takeSome :: Option Some a -> a
--takeSome (Some a) = a
----takeSome None = error "error"
----

--
-- type Option(a : *)
--   = Some a
--   | None

-- def foo(v : Int) : Int {
--   v + 1
-- }
--
-- match xs {
--   Nil | Cons _ _ -> True ;
-- }
--
--
-- foo(v : int) : int {
--   match xs
--     | Cons(a, as) => a
--     | Nil => 4
-- }
--
data Ast
  = AVar Name
  | AAdd Ast Ast
  | ASub Ast Ast
  deriving (Show, Eq)

instance Match Ast
  where
    replace old new =
      \case
        AVar var
          | var == old -> AVar new
          | otherwise -> AVar var
        AAdd a b ->
          AAdd (replace old new a) (replace old new b)
        ASub a b ->
          ASub (replace old new a) (replace old new b)

-- --

--class Match a where
--  replace :: Name -> Name -> a -> a
--
--instance Match a => Match (MExpr a) where
--  replace _ _ Fail = Fail
--  replace old new (Expr e) = Expr (replace old new e)
--
--data MPattern
--  = MCon Name [MPattern]
--  | MVar Name
--  deriving (Show, Eq)
--
--data MClause a = MClause Name [Name] (MExpr a)
--  deriving (Show, Eq)
--
--data MExpr a
--  = Fail
--  | Expr a
--  | Case Name [MClause a]
--  deriving (Show, Eq)
--
--type MEquation a = ([MPattern], MExpr a)
--
--fstPat :: (MPattern -> a) -> MEquation e -> a
--fstPat f (p:_, _) = f p
--
--conName :: MPattern -> Name
--conName (MCon name _) = name
--
--isCon :: MPattern -> Bool
--isCon MCon{} = True
--isCon MVar{} = False
--
--isVar :: MPattern -> Bool
--isVar MVar{} = True
--isVar MCon{} = False
--
--patPredicate :: MPattern -> MPattern -> Bool
--patPredicate MVar{} = isVar
--patPredicate MCon{} = isCon
--
--next :: State Int Int
--next = do
--  s <- get
--  modify (+1)
--  pure s
--
--match :: (Match a) => [Name] -> [MEquation a] -> MExpr a -> MExpr a
--match us qs e = evalState (matchM us qs e) 0 where
--  -- Empty rule
--  matchM [] [([], e)] _ = pure e
--  matchM (u:us) qs e
--    -- Variable rule
--    | all (fstPat isVar) qs = matchM us (varRule <$> qs) e
--    -- Constructor rule
--    | all (fstPat isCon) qs = Case u <$> traverse conRule (groupByConstructor qs)
--    -- Mixed rule
--    | otherwise = matchM us qs2 e >>= matchM us qs1
--    where
--      (qs1, qs2) = partitionEqs qs
--
--      varRule (MVar var:ps, e) =
--        (ps, replace var u e)
--
--      conRule qs@((MCon con ps:_, _):_) = do
--        vs <- (\f -> "$v." <> showt f) <$$> replicateM (length ps) next
--        MClause con vs <$> matchM (vs <> us) (ps' <$> qs) e
--
--      ps' (MCon _ ps:qs, e) = (ps <> qs, e)
--
--partitionEqs :: [MEquation a] -> ([MEquation a], [MEquation a])
--partitionEqs (eq:eqs) = span (fstPat (fstPat patPredicate eq)) (eq:eqs)
--
--groupByConstructor :: [MEquation a] -> [[MEquation a]]
--groupByConstructor = grouped . sorted
--  where
--    grouped = groupBy (on (==) (fstPat conName))
--    sorted  = sortBy (compare `on` fstPat conName)
--
--match :: (Match a) => [Var] -> [MEquation a] -> MExpr a -> MExpr a
--  -- Empty rule
--match [] [([], e)] _ = e
--match (u:us) qs e
--  -- Variable rule
--  | all (fstPat isVar) qs = match us (varRule u <$> qs) e
--match (u:us) qs e
--  -- Constructor rule
--  | all (fstPat isCon) qs = Case u (conRule e us <$> groupByConstructor qs)
--  -- Mixed rule
--  | otherwise = let (qs1, qs2) = partitionEqs qs in match us qs1 (match us qs2 e)
--
--partitionEqs :: [MEquation a] -> ([MEquation a], [MEquation a])
--partitionEqs (eq:eqs) = span (fstPat (fstPat patPredicate eq)) (eq:eqs)
--
--varRule :: (Match a) => Name -> MEquation a -> MEquation a
--varRule u (MVar var:ps, e) = (ps, replace var u e)
--
--conRule :: (Match a) => MExpr a -> [Name] -> [MEquation a] -> MClause (MExpr a)
--conRule e us qs@((MCon con ps:_, _):_) =
--  MClause con vs (match (vs <> us) (ps' <$> qs) e)
--  where
--    vs = (\f -> "$v." <> showt f) <$> [0 .. length ps - 1]
--    ps' (MCon _ ps:qs, e) = (ps <> qs, e)
--
--groupByConstructor :: [MEquation a] -> [[MEquation a]]
--groupByConstructor eqs = groupBy (on (==) (fstPat conName)) sorted
--  where
--    sorted = sortBy (compare `on` fstPat conName) eqs

--match us qs e
--  | null us    = undefined  -- Empty rule
--  | allVars qs = undefined  -- Variable rule
--  | allCons qs = undefined  -- Constructor rule
--  | otherwise  = undefined  -- Mixed rule

testCall =
  match
    [ "u1", "u2", "u3" ]
    [
      ( [ MVar "f", MCon "Nil" [], MVar "ys"
        ]
      , Expr (AVar "x")
      )
    ,
      ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Nil" []
        ]
      , Expr (AVar "y")
      )
    ,
      ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Cons" [MVar "y", MVar "ys"]
        ]
      , Expr (AVar "z")
      )
    ]
    Fail

testCall2 =
  match
    [ "u1", "u2", "u3" ]
    [
      ( [ MVar "f", MCon "Nil" [], MVar "ys"
        ]
      , Expr (AVar "x")
      )
    ,
      ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Nil" []
        ]
      , Expr (AVar "y")
      )
    ,
      ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Cons" [MVar "y", MVar "ys"]
        ]
      , Expr (AAdd (AVar "x") (AVar "y"))
      )
    ]
    Fail


--ps1 =
--  [
--    ( [ MVar "f", MCon "Nil" [], MVar "ys"
--      ]
--    , Expr (AVar "x")
--    )
--  ,
--    ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Nil" []
--      ]
--    , Expr (AVar "x")
--    )
--  ,
--    ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Cons" [MVar "y", MVar "ys"]
--      ]
--    , Expr (AVar "x")
--    )
--  ]
--
--ps2 =
--  [
--    ( [ MCon "Nil" [], MCon "Nil" [], MVar "ys"
--      ]
--    , Expr (AVar "x")
--    )
--  , ( [ MVar "f", MCon "Nil" [], MVar "ys"
--      ]
--    , Expr (AVar "x")
--    )
--  ,
--    ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Nil" []
--      ]
--    , Expr (AVar "x")
--    )
--  ,
--    ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Cons" [MVar "y", MVar "ys"]
--      ]
--    , Expr (AVar "x")
--    )
--  ]
--
--ps3 :: [MEquation Ast]
--ps3 =
--  [
--    ( [ MCon "Cons" [MVar "x", MVar "xs"], MCon "Nil" [], MVar "ys"
--      ]
--    , Expr (AVar "x")
--    )
--  , ( [ MCon "Nil" [], MCon "Nil" [], MVar "ys"
--      ]
--    , Expr (AVar "x")
--    )
--  , ( [ MVar "f", MCon "Nil" [], MVar "ys"
--      ]
--    , Expr (AVar "x")
--    )
--  ,
--    ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Nil" []
--      ]
--    , Expr (AVar "x")
--    )
--  ,
--    ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Cons" [MVar "y", MVar "ys"]
--      ]
--    , Expr (AVar "x")
--    )
--  ]
--
--ps4 :: [MEquation Ast]
--ps4 =
--  [
--    ( [ MCon "Nil" [], MCon "Nil" [], MVar "ys"
--      ]
--    , Expr (AVar "x")
--    )
--  , ( [ MCon "Cons" [MVar "x", MVar "xs"], MCon "Nil" [], MVar "ys"
--      ]
--    , Expr (AVar "x")
--    )
--  , ( [ MCon "Nil" [], MCon "Nil" [], MVar "ys"
--      ]
--    , Expr (AVar "x")
--    )
--  ]
--
--

--testCall3 :: MExpr (Expr ())
--testCall3 =
--  match
--    [ "u1", "u2", "u3" ]
--    [
--      ( [ MVar "f", MCon "Nil" [], MVar "ys"
--        ]
--      , Expr (ELit (PInt32 1))
--      )
--    ,
--      ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Nil" []
--        ]
--      , Expr (ELit (PInt32 2))
--      )
--    ,
--      ( [ MVar "f", MCon "Cons" [MVar "x", MVar "xs"], MCon "Cons" [MVar "y", MVar "ys"]
--        ]
--      , Expr (EVar (Label () "y"))
--      )
--    ]
--    Fail
--
--foo123 :: Expr ()
--foo123 =
--  eApp
--    ()
--    (eLam
--      [Label () "u1", Label () "u2", Label () "u3"]
--      (compileMExpr testCall3))
--    [ eApp () (EVar (Label () "Nil")) []
--    , eApp () (EVar (Label () "Nil")) []
--    , eApp () (EVar (Label () "Nil")) []
--    ]
--
--
--foo456 :: Expr ()
--foo456 =
--  eApp
--    ()
--    (eLam
--      [Label () "u1", Label () "u2", Label () "u3"]
--      (compileMExpr testCall3))
--    [ eApp () (EVar (Label () "Nil")) []
--    , eApp () (EVar (Label () "Cons")) [ELit (PInt32 100), EVar (Label () "Nil")]
--    , eApp () (EVar (Label () "Nil")) []
--    ]
--
--foo789 :: Expr ()
--foo789 =
--  eApp
--    ()
--    (eLam
--      [Label () "u1", Label () "u2", Label () "u3"]
--      (compileMExpr testCall3))
--    [ eApp () (EVar (Label () "Nil")) []
--    , eApp () (EVar (Label () "Cons")) [ELit (PInt32 100), EVar (Label () "Nil")]
--    , eApp () (EVar (Label () "Cons")) [ELit (PInt32 200), EVar (Label () "Nil")]
--    ]
--
--
--
----data NatF a = Z | S a
----  deriving (Functor, Foldable, Traversable)
----
----type Nat = Fix NatF
----
----xx2 :: Int -> Int
----xx2 n = n + 1
----
------xx2 :: NatF Int -> Int
------xx2 n =
------  case foldFix n of
------    Z ->
------      undefined -- 0
------    S m ->
------      undefined -- 1 + m
----
----xxx :: Fix NatF -> Int
----xxx = foldFix $ \case
----  Z -> undefined
----  S a -> undefined
----
----foo :: Nat -> Nat
----foo =
----  foldFix $ \case
----    Z ->
----      Fix Z-- Fix (S (Fix Zero))
----    S m ->
----      undefined -- (*) m (m - 1)
----
----three = Fix (S (Fix (S (Fix (S (Fix Z))))))
----
--
---- { id : int, name : string, age : int }
--myrec2 = (1, ("Plissken", (35, ())))
--
---- { id : int, name : string, age : int }
--myrec3 = ("Plissken", (1, (35, ())))
--
--xxx :: (a, (b, c)) -> (b, (a, c))
--xxx (a, (b, c)) = (b, (a, c))
--
---- { name : string, age : int }
--myrec1 = ("Bob", (45, ()))
--
---- { name : string | a } -> string
--testf1 (name, a) = name
--
---- { name : string | a } -> { name : string, isAdmin : bool }
--testf2 :: (String, a) -> (String, (Bool, a))
--testf2 (name, a) = (name, (True, a))

----

--ac = Map.fromList
--  [
--    ("id", 3 :: Int)
--  , ("name", "Bob" :: String)
--  ]


--
--data Obj = forall a. (Show a) => Obj a
--
--xs :: [Obj]
--xs = [Obj 1, Obj "foo", Obj 'c']
--
--doShow :: [Obj] -> String
--doShow [] = ""
--doShow ((Obj x):xs) = show x ++ doShow xs
--
--
--
---- { id : int, name : string, age : int }
--data Foo_age_id_name a = Foo
--  a
--  (a -> Int)
--  (a -> Int)
--  (a -> String)
--
--p1 (a, _)                = a
--p2 (_, (a, _))           = a
--p3 (_, (_, (a, _)))      = a
--p4 (_, (_, (_, (a, _)))) = a
--
--getAge :: Foo_age_id_name a -> Int
--getAge  (Foo r f _ _) = f r
--
--getId :: Foo_age_id_name a -> Int
--getId   (Foo r _ f _) = f r
--
--getName :: Foo_age_id_name a -> String
--getName (Foo r _ _ f) = f r
--
---- { age : int, id : int, name : string }
----myrec9 = (1, ("Plissken", (35, ())))
--
---- { 0.age : int, 1.id : int, 2.name : string }
----myrec9 = (1, ("Plissken", (35, ())))
--
--myrec10 :: Foo_age_id_name (Int, (String, (Int, ())))
--myrec10 = Foo (1, ("Plissken", (35, ()))) p3 p1 p2
--
--myrec11 :: Foo_age_id_name (String, (Int, (Int, ())))
--myrec11 = Foo ("Plissken", (1, (35, ()))) p3 p2 p1
--
--
--blooom a b = runInferCount (m + 1) mempty (unify a b)
--  where
--    s = tvars a <> tvars b
--    m | Set.null s = 0
--      | otherwise = maximum s
--
--
--zoom a b = substitution . snd <$> runInferCount (m + 1) mempty (unify a b)
--  where
--    s = tvars a <> tvars b
--    m | Set.null s = 0
--      | otherwise = maximum s
--
--foam a b =
--  case zoom a b of
--    Right sub -> let foo = normalizeRow . apply sub
--                  in Right (foo a == foo b)
--    Left err -> Left err
--
--zoofoo1 = foam a b
--  where
--    a = RExt "name" [tString] (RExt "id" [tInt32] RNil)
--    b = RExt "name" [tString] (RExt "id" [tInt32] RNil)
--
--zoofoo2 = foam a b
--  where
--    a = RExt "name" [tString] (RExt "id" [tInt32] (TVar 0))
--    b = RExt "name" [tString] (RExt "id" [tInt32] RNil)
--
--zoofoo3 = foam a b
--  where
--    a = RExt "id" [tInt32] (RExt "name" [tString] RNil)
--    b = RExt "name" [tString] (RExt "id" [tInt32] RNil)
--
--zoofoo4 = foam a b
--  where
--    a = RExt "id" [tInt32] (RExt "name" [tString] (TVar 0))
--    b = RExt "name" [tString] (RExt "id" [tInt32] RNil)
--
--zoofoo5 = foam a b
--  where
--    a = RExt "id" [tInt32] (TVar 0)
--    b = RExt "name" [tString] (RExt "id" [tInt32] RNil)
--
--zoofoo6 = foam a b
--  where
--    a = RExt "id" [tString] (TVar 0)
--    b = RExt "name" [tString] (RExt "id" [tInt32] RNil)
--
--zoofoo7 = foam a b
--  where
--    -- { name : string, admin : bool, id : int32 }
--    a = RExt "name" [tString] (RExt "admin" [tBool] (RExt "id" [tInt32] RNil))
--    -- { name : string, id : int32 | 0 }
--    b = RExt "name" [tString] (RExt "id" [tInt32] (TVar 0))
--
--zoofoo8 = foam a b
--  where
--    -- { name : string, admin : bool, id : int32 }
--    a = RExt "admin" [tBool] (RExt "id" [tInt32] RNil)
--    -- { name : string, id : int32 | 0 }
--    b = RExt "id" [tInt32] (TVar 0)




--
-- let
--   add =
--     lam(x, y) =>
--       x + y
--     in
--       let
--         succ =
--           add(1)
--         in
--           let
--             f =
--               lam(g) =>
--                 g(100)
--             in
--               f(succ)
--
--llvmType123123 =
--  [
--    ( "$fun.0", ([Label (TCon "Int32" []) "x.1",Label (TCon "Int32" []) "y.2"],EOp2 (TCon "Int32" [],OAdd) (EVar (Label (TCon "Int32" []) "x.1")) (EVar (Label (TCon "Int32" []) "y.2"))))
--  , ( "$fun.1", ([Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "g.5"],EApp (TCon "Int32" []) (EVar (Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "g.5")) (ELit (PInt32 100) :| [])))
--  , ( "$fun._", ([],ELet ((Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "succ.3",EApp (TCon "->" [TCon "Int32" [],TCon "Int32" []]) (EVar (Label (TCon "->" [TCon "Int32" [],TCon "->" [TCon "Int32" [],TCon "Int32" []]]) "$fun.0")) (ELit (PInt32 1) :| [])) :| []) (EApp (TCon "Int32" []) (EVar (Label (TCon "->" [TCon "->" [TCon "Int32" [],TCon "Int32" []],TCon "Int32" []]) "$fun.1")) (EVar (Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "succ.3") :| []))))
--  ]

--



--instance Show (IRCode IRValue) where
--  show a = "X"

--instance Show (IRConstruct (IRState, [Text])) where
--  show =
--    \case
--      CDefine t as (y, xx) -> show y <> show xx

--instance Show (IRConstruct (IRCode IRState)) where
--  show =
--    \case
--      CDefine t as v -> "CDefine " <> show t <> " " <> show as <> " " <> Text.unpack (fofofo v)
--      CDeclare t as  -> "CDeclare " <> show t <> " " <> show as
--      CType t -> "CType " <> show t

--fofofo :: IRCode IRValue -> Text

  -- deriving (Show)

--newtype IRInstr v t a = IRInstr { unIRInstr :: ReaderT (Environment IRValue) (StateT IRState (Free (IRInstrF v t))) a }
--  deriving (Functor, Applicative, Monad, MonadReader (Environment IRValue), MonadState IRState, MonadFree (IRInstrF v t))

--  deriving (Functor, Applicative, Monad, MonadReader (Environment IRValue), MonadState IRState, MonadFree (IRInstrF v t))

-- TODO!!
--toIRType2 :: Type -> IRType
--toIRType2 =
--  \case
--    t@(TCon "->" _) -> uncurry TFun (irFunTypeOf t)
--    t               -> toIRType t
--
--toIRType3 :: Type -> IRType
--toIRType3 =
--  \case
--    t@(TCon "->" _) -> ptr i8
--    t               -> toIRType t


--runIRCode :: IRCode a -> ((a, [Text]), Int)
--runIRCode code = runState (runWriterT (iterM interpreter code)) 1

--interpreter :: IRInstrF IRValue IRType (Codegen a) -> Codegen a

--
--runX :: Free (IRInstrF IRValue IRType) a -> a
--runX = undefined


-- --runIRCode :: Set Name -> IRCode a -> [Text]
--runIRCode env code = evalState (runWriterT (iterM interpreter zzz)) 1
--  where
--    zzz = runStateT (runReaderT (unIRInstr code) env) initialIRState

-- --

--------------------------------
-- saturated?  |  local?
--
-- FALSE          FALSE       create closure
-- FALSE          TRUE        build new closure
-- TRUE           FALSE       call function
-- TRUE           TRUE        call completer

--xyz :: Expr Type -> IREval IRValue
--xyz =
--  \case
--    EVar (Label t var) -> do
--      env <- ask
--      pure $ case envLookup var env of
--        Just val -> val
--        Nothing  -> Global (toIRType t) var
--
--    expr -> irEval expr
--
--packClosure v vs t ts = do
--  tc <- topType "Closure" (struct s)
--  td <- topDefine "resume" t ((ptr i8, "f") : ixArgs ts') (resume tc vs t ts)
--  r1 <- alloca tc
--  r2 <- getelementptr tc r1 (I32 0) (I32 0)
--  store td r2
--  r3 <- getelementptr tc r1 (I32 0) (I32 1)
--  store v r3
--  forM_ (zip vs [2 ..]) $ \(a, n) -> do
--    rn <- getelementptr tc r1 (I32 0) (I32 n)
--    store a rn
--  bitcast r1 (ptr i8)
--  where
--    ts' = drop (length vs) ts
--    s = [ TFun t (ptr i8 : ts'), TFun t ts ] <> (irTypeOf <$> vs)
--
--fnork f x y e = undefined -- uncurry (f x y) (irFunTypeOf e)
--
--irEval :: Expr Type -> IREval IRValue
--irEval =
--  undefined
--  \case
--    e@(EVar (Label t _)) -> do
--      x <- xyz e
--      if arity t == 0
--        then pure x
--        --else uncurry (packClosure x []) (irFunTypeOf e)
--        else do
--          case x of
--            v@Local{} -> do --error "TODO"
--              let (t, ts) = undefined -- irFunTypeOf e
--                  vs = []
--                  ts' = drop (length vs) ts
--                  s = [ TFun t (ptr i8 : ts'), TFun t (ptr i8 : ts') ] <> (ptr i8 : (irTypeOf <$> vs))
--
--              q1 <- bitcast v (struct [TFun t (ptr i8 : ts')])
--              q2 <- getelementptr (struct [TFun t (ptr i8 : ts')]) q1 (I32 0) (I32 0)
--              q3 <- load t q2
--
--              tc <- topType "Closure" (struct s)
--              td <- topDefine "resume" t ((ptr i8, "f") : ixArgs ts') (resume tc (v : vs) t (ptr i8 : ts'))
--              r1 <- alloca tc
--              r2 <- getelementptr tc r1 (I32 0) (I32 0)
--              store td r2
--              r3 <- getelementptr tc r1 (I32 0) (I32 1)
--              r4 <- bitcast v (TFun t (ptr i8 : ts'))
--              store r4 r3
--              forM_ (zip vs [2 ..]) $ \(a, n) -> do
--                rn <- getelementptr tc r1 (I32 0) (I32 n)
--                store a rn
--              bitcast r1 (ptr i8)
--
--            Global{}  -> do
--              traceShowM "======"
--              traceShowM t
--              traceShowM e
--              traceShowM "======"
--              fnork packClosure x [] e
--
----define i32 @"$fun.1"(i8* %"g.5") {
----  %1 = bitcast i8* %"g.5" to { i32 (i8*, i32)* }*
----  %2 = getelementptr { i32 (i8*, i32)* }, { i32 (i8*, i32)* }* %1, i32 0, i32 0
----  %3 = load i32 (i8*, i32)*, i32 (i8*, i32)** %2
----  %4 = call i32 %3(i8* %"g.5", i32 100)
----  ret i32 %4
----}
--
--    EApp t e1 es -> do
--      v1 <- xyz e1
--      vs <- NonEmpty.toList <$> traverse irEval es
--      if arity t == 0
--        -- Fully applied
--        then
--          case v1 of
--            v@Local{} -> undefined -- fnork callPtr v vs e1
--            Global{}  -> call (toIRType t) v1 vs
--        else do
--          case v1 of
--            v@Local{} -> error "TODO"
--            Global{}  -> do
--              traceShowM "******"
--              traceShowM t
--              traceShowM e1
--              traceShowM "******"
--              fnork packClosure v1 vs e1
--
--    ELit prim ->
--      pure (irPrim prim)
--
--    EOp2 (t, OAdd) e1 e2 -> do
--      v1 <- irEval e1
--      v2 <- irEval e2
--      add (toIRType t) v1 v2
--
--    EOp2 (t, OSub) e1 e2 -> do
--      v1 <- irEval e1
--      v2 <- irEval e2
--      sub (toIRType t) v1 v2
--
--    EOp2 (t, OMul) e1 e2 -> do
--      v1 <- irEval e1
--      v2 <- irEval e2
--      mul (toIRType t) v1 v2
--
--    EOp2 (t, OEq) e1 e2 -> do
--      v1 <- irEval e1
--      v2 <- irEval e2
--      icmpEq (toIRType t) v1 v2
--
--    ELet vs e1 -> do
--      vals <- forM vs $ \(Label t n, e) -> do
--        v <- irEval e
--        pure (n, v)
--      local (envInserts vals) (irEval e1)
--
--    e -> error (show e)



--pasta :: Expr Type -> IREval IRValue
--pasta e = do
--  a <- irEval e
--  ret (irTypeOf a) a
--
----irEval :: Expr Type -> IREval IRValue
----irEval =
----  \case
----    EVar (Label t var) -> do
----      env <- ask
----      pure $ case envLookup var env of
----        Just val -> val
----        Nothing  -> Global (toIRType2 t) var
----
----    ELit prim ->
----      pure (irPrim prim)
----
----    ELet vs e1 -> do
----      vals <- forM vs $ \(Label t n, e) -> do
----        v <- irEval e
----        pure (n, v)
----      local (envInserts vals) (irEval e1)
----
----    EOp2 (t, OAdd) e1 e2 -> do
----      v1 <- irEval e1
----      v2 <- irEval e2
----      add (toIRType t) v1 v2
----
----    EOp2 (t, OSub) e1 e2 -> do
----      v1 <- irEval e1
----      v2 <- irEval e2
----      sub (toIRType t) v1 v2
----
----    EOp2 (t, OMul) e1 e2 -> do
----      v1 <- irEval e1
----      v2 <- irEval e2
----      mul (toIRType t) v1 v2
----
----    EOp2 (t, OEq) e1 e2 -> do
----      v1 <- irEval e1
----      v2 <- irEval e2
----      icmpEq (toIRType t) v1 v2
----
----    EIf e1 e2 e3 -> do
----      irEval e1
----      block "true"
----      pasta e2
----      block "false"
----      pasta e3
----      --v1 <- irEval e1
----      --v2 <- irEval e2
----      --v3 <- irEval e3
----      ---- TODO
----      ----block "true" v2
----      ----block "false" v3
----      --pure v3
----
----    EApp t e1 es -> do
----      v1 <- irEval e1
----      vs <- NonEmpty.toList <$> traverse irEval es
----      traceShowM e1
----      traceShowM t
----      traceShowM (arity t)
----      traceShowM "-----------------"
----      if arity t > 0
----        then do
----          -- Partially applied function
----          let (t1, ts) = irFunTypeOf e1
----              ts' = drop (length vs) ts
----              s = [ TFun t1 (ptr i8 : ts'), TFun t1 ts ] <> (irTypeOf <$> vs)
----          t2 <- topType "Closure" (struct s)
----          v2 <- topDefine "resume" t1 ((ptr i8, "f") : ixArgs ts') (resume t1 ts t2 vs)
----          partiallyApply t2 v1 v2 vs
----        else case v1 of
----          v@Local{} -> uncurry (callPtr v vs) (irFunTypeOf e1)
----          Global{}  -> call (toIRType t) v1 vs
----
----    e -> error (show e)
--
----unpackClosure (Local  _ v) = Local (ptr i8) v
----unpackClosure (Global _ v) = Global (ptr i8) v
--
--callPtr :: (MonadFree (IRInstrF IRValue IRType) m) => IRValue -> [IRValue] -> IRType -> [IRType] -> m IRValue
--callPtr v vs t ts = do
--  r1 <- bitcast v (ptr (struct [t1]))
--  r2 <- getelementptr (struct [t1]) r1 (I32 0) (I32 0)
--  r3 <- load t1 r2
--  call t r3 (v : vs)
--  where
--    t1 = fun t (ptr i8 : ts)
--
--partiallyApply :: (MonadFree (IRInstrF IRValue IRType) m) => IRType -> IRValue -> IRValue -> [IRValue] -> m IRValue
--partiallyApply tc v1 td vs = do
--  r1 <- alloca tc
--  r2 <- getelementptr tc r1 (I32 0) (I32 0)
--  store td r2
--  r3 <- getelementptr tc r1 (I32 0) (I32 1)
--  store v1 r3
--  forM_ (zip vs [2 ..]) $ \(a, n) -> do
--    r <- getelementptr tc r1 (I32 0) (I32 n)
--    store a r
--  bitcast r1 (ptr i8)
--
----resume :: (MonadFree (IRInstrF IRValue IRType) m) => IRType -> [IRType] -> IRType -> [IRValue] -> m IRValue
--resume tc vs t ts = do
--  r1 <- bitcast (Local (ptr i8) "f") (ptr tc)
--  r2 <- getelementptr tc r1 (I32 0) (I32 1)
--  r3 <- load (TFun t ts) r2
--  args <- forM (zip vs [2 ..]) $ \(a, n) -> do
--    r <- getelementptr tc r1 (I32 0) (I32 n)
--    load (irTypeOf a) r
--  r4 <- call t r3 (args <> (uncurry Local <$> ixArgs (drop (length vs) ts)))
--  ret t r4

  --r1 <- bitcast (Local (ptr i8) "f") (ptr tc)
  --r2 <- getelementptr tc r1 (I32 0) (I32 1)
  --r3 <- load (TFun t1 ts) r2
  --args <- forM (zip vs [2 ..]) $ \(a, n) -> do
  --  r <- getelementptr tc r1 (I32 0) (I32 n)
  --  load (irTypeOf a) r
  --r4 <- call t1 r3 (args <> (uncurry Local <$> ixArgs (drop (length vs) ts)))
  --ret t1 r4

ixArgs :: [a] -> [(a, Text)]
ixArgs ts = ts `zip` (("a" <>) . showt <$> [0 :: Int ..])

-- --

irName :: Name -> IREval Name
irName n = do
  count <- gets globalCount
  modifyGlobalCount (+1)
  pure (n <> "." <> showt count)

--topDefine :: Name -> IRType -> [(IRType, Name)] -> IRCode IRValue -> IREval IRValue
--topDefine n t args code = do
--  name <- irName n
--  insertDefinition name (CDefine t args (initialIRState <$ code))
--  pure (Global (TFun t (fst <$> args)) name)
--
--topDeclare n = do
--  undefined
--
--topType :: Name -> IRType -> IREval IRType
--topType n ty = do
--  name <- irName n
--  insertDefinition name (CType ty)
--  pure (TName name ty)
--  -- return a Local ??

testEnv :: Environment IRValue
testEnv = envFromList
  [
    ( "x.1"
    , Local i32 "x.1"
    )
  ,
    ( "y.2"
    , Local i32 "y.2"
    )
  , ( "g.5"
    , Local (fun i32 [i32]) "g.5"
--    , Clsosure (TFun i32 [i32]) "g.5" []
    )
--  , ( "print_int32"
--    , Global (fun TVoid [i32]) "print_int32"
--    )
  ]

dict1 :: Dictionary Type
dict1 =
  Map.fromList [
    ( "$fun.0",
        ( [Label (TCon "Int32" []) "x.1",Label (TCon "Int32" []) "y.2"]
        , EOp2 (TCon "Int32" [],OAdd) (EVar (Label (TCon "Int32" []) "x.1")) (EVar (Label (TCon "Int32" []) "y.2")))
        )


  , ( "$fun.1",
        ( [Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "g.5"]
        , EApp (TCon "Int32" []) (EVar (Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "g.5")) (ELit (PInt32 100) :| [])
        )
    )
  , ( "$fun._",
        ( [],ELet ((Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "succ.3"
        , EApp
            (TCon "->" [TCon "Int32" [],TCon "Int32" []])
            (EVar (Label (TCon "->" [TCon "Int32" [],TCon "->" [TCon "Int32" [],TCon "Int32" []]]) "$fun.0"))
            (ELit (PInt32 1) :| [])) :| [])
            (EApp (TCon "Int32" [])
              (EVar (Label (TCon "->" [TCon "->" [TCon "Int32" [],TCon "Int32" []],TCon "Int32" []]) "$fun.1"))
              (EVar (Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "succ.3") :| []))
        )
    )
  ]

--  eLet
--    [
--      ( Label () "xs"
--      , eApp
--          ()
--          (EVar (Label () "Cons"))
--          [ ELit (PInt32 5)
--          , EVar (Label () "Nil")
--          ]
--      )
--    ]
--  (ELit (PInt32 9999))
dictX =
  [
    ( "Nil",
       ( []
       , undefined
       )
    )
  , ( "Cons",
       ( []
       , undefined
       )
    )
  , ( "$fun._",
       ( []
       , ELet
           (
             ( Label (TCon "List" [TCon "Int32" []]) "xs.0"
             , EApp (TCon "List" [TCon "Int32" []])
                 (EVar (Label (TCon "->" [TCon "Int32" [],TCon "->" [TCon "List" [TCon "Int32" []],TCon "List" [TCon "Int32" []]]]) "Cons"))
                 (ELit (PInt32 5) :| [EVar (Label (TCon "List" [TCon "Int32" []]) "Nil")]
                 )
              ) :| []
            )
           (ELit (PInt32 9999))
        )
    )
  ]

--crew4 :: Dictionary Type ->
--crew4 dict = undefined

crew6 = crew5 "$fun._"
        ( [],ELet ((Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "succ.3"
        , EApp
            (TCon "->" [TCon "Int32" [],TCon "Int32" []])
            (EVar (Label (TCon "->" [TCon "Int32" [],TCon "->" [TCon "Int32" [],TCon "Int32" []]]) "$fun.0"))
            (ELit (PInt32 1) :| [])) :| [])
            (EApp (TCon "Int32" [])
              (EVar (Label (TCon "->" [TCon "->" [TCon "Int32" [],TCon "Int32" []],TCon "Int32" []]) "$fun.1"))
              (EVar (Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "succ.3") :| []))
        )


--crew8 = runCodegen . runIRCode <$> (definitions (fst crew7) ! "$fun._")

--crew7 = runCodegen (runIRCode xx)
--  where
--    xx :: IRCode IRState
--    xx = runIREval mempty crew6

crew7 :: (IRState, [Text])
crew7 = runCodegen (runIRCode crew6)

crew8 :: IRState
crew8 = execCodegen (runIRCode crew6)

--fazo :: IRConstruct (IRCode IRState) -> IRConstruct (IRCode IRState) -> Ordering

crew9 :: Dictionary Type -> [(Name, IRConstruct (IRCode IRState))]
crew9 = sortBy (compare `on` weight . snd) . Map.toList . Map.unions . Map.mapWithKey foo
  where
    weight = \case
      CType{}    -> 1
      CDeclare{} -> 2
      CString{}  -> 3
      CDefine{}  -> 4

--foo :: Name -> ([Label Type], Expr Type) -> Map Name (IRConstruct (IRCode IRState)) -> Map Name (IRConstruct (IRCode IRState))
foo name as = definitions (execCodegen (runIRCode (crew5 name as)))

play :: [(Name, IRConstruct (IRCode IRState))] -> [Text]
play = fmap (uncurry goo)

goo :: Name -> IRConstruct (IRCode IRState) -> Text
goo = encode <$$> IRNamed

moo dict = Text.putStrLn cake
  where
    cake = Text.intercalate "\n" snake
    snake = play (crew9 dict)

moo_ dict = do
  Text.writeFile "tmp.ll" (intr <> cake)
  where
--    intr = "declare i8* @hashmap_init()\n\ndeclare void @print_int32(i32)\n\ndefine i32 @main() {\n  %1 = call i32 @\"$fun._\"()\n  call void @print_int32(i32 %1)\n  ret i32 0\n}\n\n"
    intr = "" -- "declare i8* @hashmap_init()\n\ndeclare void @print_int32(i32)\n\ndefine i32 @main() {\n  %1 = call i32 @\"$fun._\"()\n  call void @print_int32(i32 %1)\n  ret i32 0\n}\n\n"
    cake = Text.intercalate "\n" snake
    snake = play (crew9 dict)


mooz input = moo_ (let Right q = pipeline ((let Right r = runParser expr "" input in r)) in q)

--newtype IREval a = IREval { unIREval :: ReaderT (Environment IRValue) (StateT IRState IRCode) a }
--  deriving (Functor, Applicative, Monad, MonadReader (Environment IRValue), MonadState IRState, MonadFree (IRInstrF IRValue IRType))

--CDefine  !IRType ![(IRType, Name)] a

--faz :: IRCode (IRValue, IRState) -> Name -> IRState -> IRState
faz :: Name -> IRType -> [(IRType, Name)] -> IRCode IRState -> IRState -> IRState
faz name t args code IRState{..} =
  IRState{ definitions = Map.insert name (CDefine t args code) definitions , .. }

-- = IRState { definitions = f definitions, .. }

testConsEnv = envFromList
  [ ( "Cons", 0 )
  , ( "Nil", 1 )
  , ( "Record#", 0 )
  ]

crew5 :: Name -> ([Label Type], Expr Type) -> IRCode IRState
crew5 name (lls, e) = do
  fmap (faz name (toIRType (typeOf e)) args zz) zz
  where
    zz :: IRCode IRState
    zz = fmap snd dd
    dd :: IRCode (IRValue, IRState)
    --dd = runIREval (envFromList foo) (irEval e)
    dd = runIREval (envFromList (foo <> foobaz)) testConsEnv (pastaTmp e)
    foo = [(n, Local (funPtrType t) n) | Label t n <- lls ]
    args :: [(IRType, Name)]
    args = swap <$> (funPtrType <$$> unLabel <$> lls)

foobaz =
  [ ("print_int32", Global (fun TVoid [i32]) "print_int32")
  , ("print_int64", Global (fun TVoid [i64]) "print_int64")
  , ("print_string", Global (fun TVoid [ptr i8]) "print_string")
  , ("increment", Global (fun i32 [i32]) "increment")
  ]


pastaTmp :: Expr Type -> IREval IRValue
pastaTmp e = do
  a <- irEval e
  ret (irTypeOf a) a

----pastaTmp2 :: Expr Type -> IREval IRValue
--pastaTmp2 e l1 l2 = do
--  a <- irEval e
--  br a l1 l2

progX :: Expr ()
progX =
  eLet
    [
      ( Label () "f"
      , eLam [Label () "n"] (ELit (PInt32 7))
      )
    ]
    (eLet
      [
        ( Label () "id"
        , eLam [Label () "x"] (EVar (Label () "x"))
        )
      ]
      (eApp
        ()
        (eApp
          ()
          (EVar (Label () "id"))
          [ EVar (Label () "f") ]
        )
        [ ELit (PInt32 10) ]
        )
      )


progY :: Expr ()
progY =
  eLet
    [
      ( Label () "f"
      , eLam [Label () "n"] (ELit (PInt32 7))
      )
    ]
    (eLet
      [
        ( Label () "g"
        , eLam [Label () "n"] (ELit (PInt32 8))
        )
      ]
      (eApp
        ()
        (EIf
          (ELit (PBool True))
          (EVar (Label () "f"))
          (EVar (Label () "g"))
        )
        [ELit PUnit]
      )
    )


progY2 :: Expr ()
progY2 =
  EIf
    (ELit (PBool True))
    (ELit (PInt32 123))
    (ELit (PInt32 456))

progZ :: Expr ()
progZ =
  eLet
    [ ( Label () "f"
      , eLam [Label () "x", Label () "y", Label () "z"]
          (EOp2 ((), OAdd) (EVar (Label () "x")) (EOp2 ((), OAdd) (EVar (Label () "y")) (EVar (Label () "z"))))
      )
    ]
    ( eLet
        [ ( Label () "g"
          , eApp () (EVar (Label () "f")) [ELit (PInt32 1)] )
        ]
        ( eLet
            [ ( Label () "h"
              , eApp () (EVar (Label () "g")) [ELit (PInt32 2)] )
            ]
            (eApp () (EVar (Label () "h")) [ELit (PInt32 3)])
        )
    )


progZ2 :: Expr ()
progZ2 =
  eLet
    [ ( Label () "f"
      , eLam [Label () "x", Label () "y", Label () "z"]
          (EOp2 ((), OAdd) (EVar (Label () "x")) (EOp2 ((), OAdd) (EVar (Label () "y")) (EVar (Label () "z"))))
      )
    ]
    ( eLet
        [ ( Label () "g"
          , eApp () (EVar (Label () "f")) [ELit (PInt32 1), ELit (PInt32 2)] )
        ]
        ( eApp () (EVar (Label () "g")) [ELit (PInt32 3)]
        )
    )

--

topLiteral :: Name -> Text -> IREval IRValue
topLiteral n str = do
  name <- irName n
  insertDefinition name (CString name str)
  pure (Global (ptr (literalType str)) name)

topType :: Name -> IRType -> IREval IRType
topType n ty = do
  name <- irName n
  insertDefinition name (CType ty)
  pure (TName name ty)

topDefine :: Name -> IRType -> [(IRType, Name)] -> IRCode IRValue -> IREval IRValue
topDefine n t args code = do
  name <- irName n
  insertDefinition name (CDefine t args (initialIRState <$ code))
  pure (Global (fun t (fst <$> args)) name)

resume :: IRType -> [IRValue] -> IRType -> IRType -> [IRType] -> IRCode IRValue
resume ct vs t1 t ts = do
  r1 <- bitcast (Local (ptr i8) "f") (ptr ct)
  r2 <- getelementptr ct r1 (I32 0) (I32 1)
  r3 <- load t1 r2
  args <- forM (zip vs [2 ..]) $ \(a, n) -> do
    r <- getelementptr ct r1 (I32 0) (I32 n)
    load (irTypeOf a) r
  r4 <- call t r3 (args <> (uncurry Local <$> ixArgs ts))
  ret t r4

irLookupValue :: Type -> Name -> IREval IRValue
irLookupValue t var = do
  (env, _) <- ask
  pure $ case envLookup var env of
    Just val -> val
    Nothing  -> Global (toIRType t) var

unpackClosure :: IRType -> IRValue -> [IRValue] -> IREval IRValue
unpackClosure (TFun t ts) v vs = do
  r1 <- bitcast v (ptr s)
  r2 <- getelementptr s r1 (I32 0) (I32 0)
  load tfun r2
  where
    tfun = fun t (ptr i8 : ts <> (irTypeOf <$> vs))
    s = struct [tfun]
unpackClosure _ _ _ = error "Implementation error"

packClosure :: [IRValue] -> [IRValue] -> IREval IRValue
packClosure us vs = do
  tt <- topType "closure" s
  td <- topDefine "resume" t ((ptr i8, "f") : ixArgs ts') (resume tt us (TFun t ts) t ts')
  r1 <- alloca tt
  r2 <- getelementptr tt r1 (I32 0) (I32 0)
  void $ store td r2
  forM_ (zip vs [1 ..]) $ \(v, n) -> do
    rn <- getelementptr tt r1 (I32 0) (I32 n)
    store v rn
  bitcast r1 (ptr i8)
  where
    TFun t ts = irTypeOf (head vs)
    s = struct ([fun t (ptr i8 : ts'), TFun t ts] <> (irTypeOf <$> us))
    ts' = drop (length us) ts
packClosure _ _ = error "Implementation error"

irLabel :: Name -> IREval ()
irLabel ll = block ll >> setLabel ll

irBlock :: Name -> Name -> IREval IRValue -> IREval (IRValue, Name)
irBlock ll brll code = do
  irLabel ll
  r <- code
  l <- gets label
  br_ [brll]
  pure (r, l)

irEval :: Expr Type -> IREval IRValue
irEval =
  \case
    EVar (Label t var) | isUpper (Text.head var) -> do
      name <- irName var
      let s = struct [i32]
      tt <- topType name s
      r1 <- alloca tt
      r2 <- getelementptr tt r1 (I32 0) (I32 0)
      (_, env) <- ask
      case envLookup var env of
        Nothing -> error ("No constructor " <> Text.unpack var)
        Just ix -> store (I32 (fromIntegral ix)) r2
      bitcast r1 (ptr i8)

    EApp t (EVar (Label t1 var)) es | isUpper (Text.head var) -> do
      vs <- NonEmpty.toList <$> traverse irEval es
      name <- irName var
      let s = struct ([i32] <> (irTypeOf <$> vs))
      tt <- topType name s
      r1 <- alloca tt
      r2 <- getelementptr tt r1 (I32 0) (I32 0)
      (_, env) <- ask
      case envLookup var env of
        Nothing -> error ("No constructor " <> Text.unpack var)
        Just ix -> store (I32 (fromIntegral ix)) r2
      forM_ (zip vs [1 ..]) $ \(v, n) -> do
        rn <- getelementptr tt r1 (I32 0) (I32 n)
        store v rn
      bitcast r1 (ptr i8)

    EVar (Label t var) -> do
      v1 <- irLookupValue t var
      if arity t == 0
        then pure v1
        else do
          case v1 of
            Local{} -> do
              r1 <- unpackClosure (irTypeOf t) v1 []
              r2 <- bitcast r1 (ptr i8)
              packClosure [r2] [r1, v1]
            Global{} ->
              packClosure [] [v1]
            _ ->
              error "Implementation error"

    EApp t e1 es -> do
      v1 <- case e1 of
        EVar (Label t1 var) -> irLookupValue t1 var
        _ -> irEval e1
      vs <- NonEmpty.toList <$> traverse irEval es
      if arity t == 0
        -- Fully applied
        then case v1 of
          Local{} -> do
            r1 <- unpackClosure (irTypeOf e1) v1 []
            call (irTypeOf t) r1 (v1 : vs)
          Global{} ->
            call (toIRType t) v1 vs
          _ ->
            error "Implementation error"

        else case v1 of
          Local{} -> do
            r1 <- unpackClosure (irTypeOf e1) v1 vs
            r2 <- bitcast r1 (ptr i8)
            packClosure (r2 : vs) ([r1, v1] <> vs)
          Global{} -> do
            packClosure vs (v1 : vs)
          _ ->
            error "Implementation error"

    EIf e1 e2 e3 -> do
      r1 <- irEval e1
      r2 <- cmpEq i1 r1 (I1 True)
      lblTrue <- irName "true"
      lblFalse <- irName "false"
      lblEnd <- irName "end"
      br r2 [lblTrue, lblFalse]
      thenBlock <- irBlock lblTrue lblEnd (irEval e2)
      elseBlock <- irBlock lblFalse lblEnd (irEval e3)
      irLabel lblEnd
      phi (irTypeOf (fst thenBlock)) [thenBlock, elseBlock]

    EPat e1 cs -> do
      v1 <- irEval e1
      r1 <- bitcast v1 (ptr (struct [i32]))
      r2 <- getelementptr (struct [i32]) r1 (I32 0) (I32 0)
      r3 <- load i32 r2
      lblEnd <- irName "end"
      ds <- forM cs $ \(Clause ((Label _ con):|lls) e) -> do
        n <- irName con
        pure (n, lls, e)
      let ns = fst3 <$> ds
      switch r3 (NonEmpty.head ns) (zip (I32 <$> [0 ..]) (NonEmpty.toList ns))
      b:|bs <- forM ds $ \(ll, lls, e) -> do
        irBlock ll lblEnd $ do
          let s = struct (i32 : (irTypeOf . snd . unLabel <$> lls))
          r4 <- bitcast v1 (ptr s)
          vals <- forM (zip lls [1 .. ]) $ \(Label ti n, i) -> do
            ri <- getelementptr s r4 (I32 0) (I32 i)
            rj <- load (irTypeOf ti) ri
            pure (n, rj)
          local (first (envInserts vals)) (irEval e)
      irLabel lblEnd
      phi (irTypeOf (fst b)) (b:bs)

    ELit (PString str) ->
      topLiteral "lit" str

    ELit prim ->
      pure (irPrim prim)

    EOp2 (t, OAdd) e1 e2 -> do
      v1 <- irEval e1
      v2 <- irEval e2
      add (toIRType t) v1 v2

    EOp2 (t, OSub) e1 e2 -> do
      v1 <- irEval e1
      v2 <- irEval e2
      sub (toIRType t) v1 v2

    EOp2 (t, OMul) e1 e2 -> do
      v1 <- irEval e1
      v2 <- irEval e2
      mul (toIRType t) v1 v2

    EOp2 (t, OEq) e1 e2 -> do
      v1 <- irEval e1
      v2 <- irEval e2
      cmpEq (toIRType t) v1 v2

    ELet vs e1 -> do
      vals <- forM vs $ \(Label t n, e) -> do
        v <- irEval e
        pure (n, v)
      local (first (envInserts vals)) (irEval e1)

    ENil ->
      call (ptr i8) (Global (fun (ptr i8) [TVoid]) "hashmap_init") []

    EExt ll e1 e2 -> do
      t1 <- topLiteral ("label_" <> ll) ll
      t2 <- getelementptr (literalType ll) t1 (I32 0) (I32 0)
      v1 <- irEval e1
      v2 <- irEval e2
      r1 <- irConceal v1
      call (ptr i8) (Global (fun (ptr i8) [ptr i8, ptr i8]) "hashmap_insert") [v2, t2, r1]

    EFocus (Focus (Label _ ll) (Label t var) (Label _ r)) e1 e2 -> do
      t1 <- topLiteral ("label_" <> ll) ll
      t2 <- getelementptr (literalType ll) t1 (I32 0) (I32 0)
      v1 <- irEval e1
      v2 <- call (ptr i8) (Global (fun (ptr i8) [ptr i8]) "hashmap_lookup") [v1, t2]
      r2 <- irReveal v2 (irTypeOf t)
      local (first (envInserts [(var, r2), (r, v1)])) (irEval e2)

--    ECall TVar{} name es e -> do
--      vs <- traverse irEval es
--      call TVoid (Global (fun TVoid (irTypeOf <$> es)) name) vs
--
--      v1 <- irEval e
--      v2 <- irEval (ELit PUnit)
--      r1 <- irConceal v2
--      let t2 = NonEmpty.last (unfoldType (typeOf e))
--
--      case v1 of
--        Local{} -> do
--          r2 <- unpackClosure (irTypeOf e) v1 []
--          call (toIRType t2) r2 [v1, r1]
--        Global{} ->
--          call (toIRType t2) v1 [r1]
--        _ ->
--          error "Implementation error"

    ECall (Label t ll) es e -> do
      v1 <- irLookupValue t (Text.tail ll)
      --vs <- traverse (irConceal <=< irEval) es
      vs <- traverse irEval es                         --- ?
      let TFun t1 _ = irTypeOf v1
      r1 <- call t1 v1 vs
      v2 <- irEval e
      let t2 = toIRType (NonEmpty.last (unfoldType (typeOf e)))

      rx <- case irTypeOf r1 of
        TVoid -> irEval (ELit PUnit) >>= irConceal
        _     -> pure r1

      case v2 of
        Local{} -> do
          r2 <- unpackClosure (irTypeOf e) v2 []
          call t2 r2 [v2, rx]
        Global{} ->
          call t2 v2 [rx]
        _ ->
          error "Implementation error"

--      case irTypeOf r1 of
--        TVoid -> do
--          r2 <- irEval (ELit PUnit)
--          call (toIRType (NonEmpty.last (unfoldType (typeOf e)))) v2 [r2]
--        _ ->
--          call (toIRType (NonEmpty.last (unfoldType (typeOf e)))) v2 [r1]

      --pure (I32 134)
      --v1 <- irEval e

      --let t1 = TCon "Int32" []
      --r1 <- call i32 (Global (fun i32 (irTypeOf <$> es)) name) vs

      --ret i32 444

      --r2 <- irConceal r1

      --v1 <- irEval e

      --case v1 of
      --  Local{} -> do
      --    r2 <- unpackClosure (irTypeOf e) v1 []
      --    call (toIRType t2) r2 [v1, r1]
      --  Global{} ->
      --    call (toIRType t2) v1 [r1]
      --  _ ->
      --    error "Implementation error"

    e -> error (show e)

irConceal :: IRValue -> IREval IRValue
irConceal v =
  case irTypeOf v of
    TPtr TInt8 -> pure v
    TPtr{}     -> bitcast v (ptr i8)          -- ???
    TInt1      -> inttoptr v (ptr i8)
    TInt8      -> inttoptr v (ptr i8)
    TInt32     -> inttoptr v (ptr i8)
    TInt64     -> inttoptr v (ptr i8)
    t -> do
      r <- alloca t >>= store v
      bitcast r (ptr i8)

irReveal :: IRValue -> IRType -> IREval IRValue
irReveal v =
  \case
    TPtr TInt8 -> pure v
    TInt1      -> ptrtoint v i1
    TInt8      -> ptrtoint v i8
    TInt32     -> ptrtoint v i32
    TInt64     -> ptrtoint v i64
    t          -> bitcast v (ptr t) >>= load t

xtests = do
  moo_ dict1
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  print (read r :: Int, 101)

  moo_ (let Right r = pipeline progX in r)
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  print (read r :: Int, 7)

  moo_ (let Right r = pipeline progY in r)
  readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  print (read r :: Int, 7)

  moo_ (let Right r = pipeline progZ in r)
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  print (read r :: Int, 6)

  moo_ (let Right r = pipeline progZ2 in r)
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  print (read r :: Int, 6)

  moo_ (let Right r = pipeline progI in r)
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  print (read r :: Int, 3628800)

  moo_ (let Right r = pipeline progK in r)
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  print (read r :: Int, 101)

  moo_ (let Right r = pipeline progQ in r)
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  print (read r :: Int, 5)

  moo_ (let Right r = pipeline progJ in r)
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  print (read r :: Int, 2)

  moo_ (let Right r = pipeline progM3 in r)
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  print (read r :: Int, 123)

ytests = do
  mooz "let n = #1 ; f = fn(x) => if x == #0 then n else let m = x * @f(x - #1) in $call print_int32(m) (fn(_) => m) in @f(#15)"
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  putStrLn r

  mooz "let b = 1 ; f = fn(n) => if n == 0 then b else let m = n * @f(n - 1) in $call print_int64(m) fn(_) => m in let x = @f(15) in #1"
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  putStrLn r

  mooz "let xs = @Cons(#1, @Cons(#2, Nil)) in match xs { | Cons(x, xs) => match xs { | Cons(y, ys) => y | Nil => #102 } | Nil => #101 }"
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  putStrLn r

  mooz "if #1 == #2 then #3 else if #4 == #5 then #6 else #7"
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  putStrLn r

  mooz "let r = { count = #5 | {} } in focus { count = c | q } = r in c"
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  putStrLn r

  mooz "let r = @Record#({ count = #5 | {} }) in match r { | Record#(z) => focus { count = c | q } = z in c }"
  void $ readProcess "./build.sh" [] ""
  r <- readProcess "./tmp" [] ""
  putStrLn r


--let
--  r =
--    @Record#({ count = #5 | {} })
--  in
--    match r {
--      | Record#(z) =>
--          focus
--            { count = c | q } =
--              z
--            in
--              c
--    }

endend input =
  case runParser expr "" input of
    Left e -> error (show e)
    Right r ->
      case pipeline r of
        Left e -> error (show e)
        Right q ->
          runDict q
