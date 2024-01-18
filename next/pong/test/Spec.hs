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
import Test.Hspec

import Debug.Trace
import Data.Tuple.Extra
import Data.Char (isAlphaNum)
import Control.Monad.Writer
import Data.Fix
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State
import Data.Text (Text)
import Control.Arrow ((>>>))
import Data.Foldable
import Data.Functor
import Data.Map.Strict (Map)
import Data.Int (Int32, Int64)
import Data.Function (on)
import Data.List (sortBy, groupBy, intersperse)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Map.Strict ((!))
import Data.Set (Set)
import TextShow
import Pong
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Control.Monad.Free as Free
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

testTypeEnv :: TypeEnv
testTypeEnv = envFromList
  [ ("Cons", scheme [0] (TVar 0 ~> TCon "List" [TVar 0] ~> TCon "List" [TVar 0]))
  , ("Nil", scheme [0] (TCon "List" [TVar 0]))
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
        ( ERes
            (Focus (Label () "name") (Label () "n") (Label () "r"))
            (EExt "id" (ELit (PInt32 1)) ENil)
            (EVar (Label () "n"))
        , Left "Cannot unify"
        )
      , -- field { name = n | r } = { name = "Bob", id = 1 } in n
        ( ERes
            (Focus (Label () "name") (Label () "n") (Label () "r"))
            (EExt "name" (ELit (PString "Bob")) (EExt "id" (ELit (PInt32 1)) ENil))
            (EVar (Label () "n"))
        , Right $ ERes
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
        ( ERes
            (Focus (Label () "name") (Label () "n") (Label () "r"))
            (EExt "name" (ELit (PString "Bob")) (EExt "id" (ELit (PInt32 1)) ENil))
            (EVar (Label () "n"))
        -- field { name = n.0 | r.1 } = { name = "Bob", id = 1 } in n.0
        , ERes
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
  ERes
    (Focus (Label () "name") (Label () "n") (Label () "r"))
    (EExt "name" (ELit (PString "Plissken")) (EExt "id" (ELit (PInt32 1)) ENil))
    (EVar (Label () "n"))

--
-- field
--   { name = n | r } =
--     { name = "Plissken", id = 1 }
--   in
--     r
--
progM :: Expr ()
progM =
  ERes
    (Focus (Label () "name") (Label () "n") (Label () "r"))
    (EExt "name" (ELit (PString "Plissken")) (EExt "id" (ELit (PInt32 1)) ENil))
    (EVar (Label () "r"))

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
    (ERes
      (Focus (Label () "fun") (Label () "f") (Label () "r"))
      (EExt "fun" (eLam [Label () "x"] (EVar (Label () "x"))) (EExt "id" (ELit (PInt32 1)) ENil))
      (eApp ()
        (eApp () (EVar (Label () "f")) [ EVar (Label () "g") ])
        [ eApp () (EVar (Label () "f")) [ ELit (PInt32 5) ]
        ]
      )
    )

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


data Some
data None

data Option c a
  = Some a
  | None
  deriving (Show)

some :: a -> Option Some a
some = Some

none :: Option None a
none = None

instance Functor (Option c) where
  fmap f None = None
  fmap f (Some v) = Some (f v)

withDefault :: a -> Option c a -> a
withDefault d None = d
withDefault _ (Some v) = v

takeSome :: Option Some a -> a
takeSome (Some a) = a
--takeSome None = error "error"
--

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

testCall3 :: MExpr (Expr ())
testCall3 =
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

foo123 :: Expr ()
foo123 =
  eApp
    ()
    (eLam
      [Label () "u1", Label () "u2", Label () "u3"]
      (compileMExpr testCall3))
    [ eApp () (EVar (Label () "Nil")) []
    , eApp () (EVar (Label () "Nil")) []
    , eApp () (EVar (Label () "Nil")) []
    ]


foo456 :: Expr ()
foo456 =
  eApp
    ()
    (eLam
      [Label () "u1", Label () "u2", Label () "u3"]
      (compileMExpr testCall3))
    [ eApp () (EVar (Label () "Nil")) []
    , eApp () (EVar (Label () "Cons")) [ELit (PInt32 100), EVar (Label () "Nil")]
    , eApp () (EVar (Label () "Nil")) []
    ]

foo789 :: Expr ()
foo789 =
  eApp
    ()
    (eLam
      [Label () "u1", Label () "u2", Label () "u3"]
      (compileMExpr testCall3))
    [ eApp () (EVar (Label () "Nil")) []
    , eApp () (EVar (Label () "Cons")) [ELit (PInt32 100), EVar (Label () "Nil")]
    , eApp () (EVar (Label () "Cons")) [ELit (PInt32 200), EVar (Label () "Nil")]
    ]



--data NatF a = Z | S a
--  deriving (Functor, Foldable, Traversable)
--
--type Nat = Fix NatF
--
--xx2 :: Int -> Int
--xx2 n = n + 1
--
----xx2 :: NatF Int -> Int
----xx2 n =
----  case foldFix n of
----    Z ->
----      undefined -- 0
----    S m ->
----      undefined -- 1 + m
--
--xxx :: Fix NatF -> Int
--xxx = foldFix $ \case
--  Z -> undefined
--  S a -> undefined
--
--foo :: Nat -> Nat
--foo =
--  foldFix $ \case
--    Z ->
--      Fix Z-- Fix (S (Fix Zero))
--    S m ->
--      undefined -- (*) m (m - 1)
--
--three = Fix (S (Fix (S (Fix (S (Fix Z))))))
--

-- { id : int, name : string, age : int }
myrec2 = (1, ("Plissken", (35, ())))

-- { id : int, name : string, age : int }
myrec3 = ("Plissken", (1, (35, ())))

xxx :: (a, (b, c)) -> (b, (a, c))
xxx (a, (b, c)) = (b, (a, c))

-- { name : string, age : int }
myrec1 = ("Bob", (45, ()))

-- { name : string | a } -> string
testf1 (name, a) = name

-- { name : string | a } -> { name : string, isAdmin : bool }
testf2 :: (String, a) -> (String, (Bool, a))
testf2 (name, a) = (name, (True, a))

----

--ac = Map.fromList
--  [
--    ("id", 3 :: Int)
--  , ("name", "Bob" :: String)
--  ]



data Obj = forall a. (Show a) => Obj a

xs :: [Obj]
xs = [Obj 1, Obj "foo", Obj 'c']

doShow :: [Obj] -> String
doShow [] = ""
doShow ((Obj x):xs) = show x ++ doShow xs



-- { id : int, name : string, age : int }
data Foo_age_id_name a = Foo
  a
  (a -> Int)
  (a -> Int)
  (a -> String)

p1 (a, _)                = a
p2 (_, (a, _))           = a
p3 (_, (_, (a, _)))      = a
p4 (_, (_, (_, (a, _)))) = a

getAge :: Foo_age_id_name a -> Int
getAge  (Foo r f _ _) = f r

getId :: Foo_age_id_name a -> Int
getId   (Foo r _ f _) = f r

getName :: Foo_age_id_name a -> String
getName (Foo r _ _ f) = f r

-- { age : int, id : int, name : string }
--myrec9 = (1, ("Plissken", (35, ())))

-- { 0.age : int, 1.id : int, 2.name : string }
--myrec9 = (1, ("Plissken", (35, ())))

myrec10 :: Foo_age_id_name (Int, (String, (Int, ())))
myrec10 = Foo (1, ("Plissken", (35, ()))) p3 p1 p2

myrec11 :: Foo_age_id_name (String, (Int, (Int, ())))
myrec11 = Foo ("Plissken", (1, (35, ()))) p3 p2 p1


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
llvmType123123 =
  [
    ( "$fun.0", ([Label (TCon "Int32" []) "x.1",Label (TCon "Int32" []) "y.2"],EOp2 (TCon "Int32" [],OAdd) (EVar (Label (TCon "Int32" []) "x.1")) (EVar (Label (TCon "Int32" []) "y.2"))))
  , ( "$fun.1", ([Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "g.5"],EApp (TCon "Int32" []) (EVar (Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "g.5")) (ELit (PInt32 100) :| [])))
  , ( "$fun._", ([],ELet ((Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "succ.3",EApp (TCon "->" [TCon "Int32" [],TCon "Int32" []]) (EVar (Label (TCon "->" [TCon "Int32" [],TCon "->" [TCon "Int32" [],TCon "Int32" []]]) "$fun.0")) (ELit (PInt32 1) :| [])) :| []) (EApp (TCon "Int32" []) (EVar (Label (TCon "->" [TCon "->" [TCon "Int32" [],TCon "Int32" []],TCon "Int32" []]) "$fun.1")) (EVar (Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "succ.3") :| []))))
  ]

--



(<+>) :: Text -> Text -> Text
a <+> b = a <> " " <> b

infixr 6 <+>

data IRType
  = TInt1
  | TInt8
  | TInt32
  | TInt64
  | TFloat
  | TDouble
  | TVoid
  | TFun !IRType ![IRType]
  | TPtr !IRType
  | TStruct ![IRType]
  | TName !Name !IRType
  deriving (Show, Eq, Ord, Read)

i1 :: IRType
i1 = TInt1

i8 :: IRType
i8 = TInt8

i32 :: IRType
i32 = TInt32

i64 :: IRType
i64 = TInt64

ptr :: IRType -> IRType
ptr = TPtr

fun :: IRType -> [IRType] -> IRType
fun = TFun

struct :: [IRType] -> IRType
struct = TStruct

data IRValue
  = Local  !IRType !Name
  | Global !IRType !Name
  | I1     !Bool
  | I32    !Int32
  | I64    !Int64
  | Float  !Float
  | Double !Double
  deriving (Show, Eq, Ord, Read)

irTypeOf :: IRValue -> IRType
irTypeOf =
  \case
    Local  t _ -> t
    Global t _ -> t
    I1     _   -> TInt1
    I32    _   -> TInt32
    I64    _   -> TInt64
    Float  _   -> TFloat
    Double _   -> TDouble

data IRInstrF v t a
  = IAdd   t v v   (v -> a)
  | ISub   t v v   (v -> a)
  | IMul   t v v   (v -> a)
  | IRet   t v     (v -> a)
  | ILoad  t v     (v -> a)
  | IStore v v     (v -> a)
  | IGep   t v v v (v -> a)
  | IAlloc t       (v -> a)
  | IBCast v t     (v -> a)
  | ICall  t v [v] (v -> a)
  deriving (Functor)

--instance Show (IRCode IRValue) where
--  show a = "X"

data IRConstruct a
  = CDefine  !IRType ![(IRType, Name)] a
  | CDeclare !IRType ![(IRType, Name)]
  | CType    !IRType
  deriving (Functor)

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
fofofo code = xxc
  where
    xxc = Text.unlines xxb
    xxb = snd (runCodegen xxa)
    xxa = runIRCode code

data IRState = IRState 
  { globalCount :: Int
  , definitions :: Map Name (IRConstruct (IRCode IRState))
  } 
  -- deriving (Show)

initialIRState :: IRState
initialIRState = IRState
  { globalCount = 1
  , definitions = mempty
  }

overGlobalCount :: (Int -> Int) -> IRState -> IRState
overGlobalCount f IRState{..} = IRState { globalCount = f globalCount, .. }

--overDefinitions :: (Map Name (IRConstruct (IRCode (IRValue, IRState))) -> Map Name (IRConstruct (IRCode (IRValue, IRState)))) -> IRState -> IRState
overDefinitions f IRState{..} = IRState { definitions = f definitions, .. }

modifyGlobalCount :: (MonadState IRState m) => (Int -> Int) -> m ()
modifyGlobalCount = modify . overGlobalCount

--modifyDefinitions :: (MonadState IRState m) => (Map Name IRConstruct -> Map Name IRConstruct) -> m ()
modifyDefinitions = modify . overDefinitions

--insertDefinition :: (MonadState IRState m) => Name -> IRConstruct -> m ()
insertDefinition = modifyDefinitions <$$> Map.insert

--newtype IRInstr v t a = IRInstr { unIRInstr :: ReaderT (Environment IRValue) (StateT IRState (Free (IRInstrF v t))) a }
--  deriving (Functor, Applicative, Monad, MonadReader (Environment IRValue), MonadState IRState, MonadFree (IRInstrF v t))

type IRInstr v t = Free (IRInstrF v t) 
--  deriving (Functor, Applicative, Monad, MonadReader (Environment IRValue), MonadState IRState, MonadFree (IRInstrF v t))

add :: (MonadFree (IRInstrF v t) m) => t -> v -> v -> m v
add t v w = wrap (IAdd t v w pure)

sub :: (MonadFree (IRInstrF v t) m) => t -> v -> v -> m v
sub t v w = wrap (ISub t v w pure)

mul :: (MonadFree (IRInstrF v t) m) => t -> v -> v -> m v
mul t v w = wrap (IMul t v w pure)

alloca :: (MonadFree (IRInstrF v t) m) => t -> m v
alloca t = wrap (IAlloc t pure)

load :: (MonadFree (IRInstrF v t) m) => t -> v -> m v
load t v = wrap (ILoad t v pure)

ret :: (MonadFree (IRInstrF v t) m) => t -> v -> m v
ret t v = wrap (IRet t v pure)

store :: (MonadFree (IRInstrF v t) m) => v -> v -> m v
store v w = wrap (IStore v w pure)

getelementptr :: (MonadFree (IRInstrF v t) m) => t -> v -> v -> v -> m v
getelementptr t u v w = wrap (IGep t u v w pure)

call :: (MonadFree (IRInstrF v t) m) => t -> v -> [v] -> m v
call t v vs = wrap (ICall t v vs pure)

bitcast :: (MonadFree (IRInstrF v t) m) => v -> t -> m v
bitcast v t = wrap (IBCast v t pure)

irPrim :: Prim -> IRValue
irPrim =
  \case
    PBool b    -> I1 b
    PInt32 i32 -> I32 i32
    PInt64 i64 -> I64 i64
    PFloat f   -> Float f
    PDouble d  -> Double d

class IR a where
  encode :: a -> Text

instance (IR a) => IR [a] where
  encode = Text.concat . intersperse ", " . (encode <$>)

instance IR IRValue where
  encode =
    \case
      Local  _ name  -> "%" <> enquote name
      Global _ name  -> "@" <> enquote name
      I1     False   -> "0"
      I1     True    -> "1"
      I32    n -> showt n
      I64    n -> showt n
      Float  f -> showt f
      Double d -> showt d

instance IR IRType where
  encode =
    \case
      TInt1     -> "i1"
      TInt8     -> "i8"
      TInt32    -> "i32"
      TInt64    -> "i64"
      TFloat    -> "float"
      TDouble   -> "double"
      TVoid     -> "void"
      TFun t ts -> encode t <+> "(" <> encode ts <> ")" <> "*"
      TPtr t    -> encode t <> "*"
      TStruct t -> "{" <+> encode t <+> "}"
      TName n _ -> "%" <> enquote n

enquote :: Text -> Text
enquote n
  | Text.all isAlphaNum n = n
  | otherwise = "\"" <> n <> "\""

newtype IRTyped v = IRTyped v
  deriving (Show, Eq, Ord, Read)

instance IR (IRTyped IRValue) where
  encode (IRTyped v) = encode (irTypeOf v) <+> encode v

instance IR (IRCode IRState) where
  encode code = Text.unlines (indent 2 <$> snd (runCodegen (runIRCode code)))

indent :: Int -> Text -> Text
indent level = (spaces <>) where spaces = Text.replicate level " "

instance IR (IRType, Name) where
  encode = encode . IRTyped . uncurry Local

data IRNamed = IRNamed Name (IRConstruct (IRCode IRState))

instance IR IRNamed where
  encode (IRNamed name ir) =
    case ir of
      CDefine t as e ->
        let signature = encode t <+> "@" <> enquote name <> "(" <> encode as <> ")"
            body = "{\n" <> encode e <> "}\n"
         in "define" <+> signature <+> body
      CDeclare _ _ -> 
        "TODO"
      CType t -> do
        let body = "{\n" <> encodeStruct t <> "\n}\n"
        "%" <> enquote name <+> "=" <+> "type" <+> body
        where
          encodeStruct = \case
            TStruct ts -> Text.intercalate ",\n" (indent 2 . encode <$> ts)
            t -> encode t

irFunType :: Type -> (IRType, [IRType])
irFunType =
  \case
    TCon "->" [t1, t2] -> second (toIRType t1 :) (irFunType t2)
    t                  -> (toIRType t, [])

irFunTypeOf :: (Typed t) => t -> (IRType, [IRType])
irFunTypeOf = irFunType . typeOf

toIRType :: Type -> IRType
toIRType =
  \case
    TCon "Bool"   [] -> i1
    TCon "Int32"  [] -> i32
    TCon "Int64"  [] -> i64
    TCon "Float"  [] -> TFloat
    TCon "Double" [] -> TDouble
    TCon "Char"   [] -> i8
    t@(TCon "->"  _) -> ptr i8

-- TODO!!
toIRType2 :: Type -> IRType
toIRType2 =
  \case
    t@(TCon "->" _) -> uncurry TFun (irFunTypeOf t)
    t               -> toIRType t

type Codegen = WriterT [Text] (State Int)
 
runCodegen :: Codegen a -> (a, [Text])
runCodegen a = evalState (runWriterT a) 1

execCodegen :: Codegen a -> a
execCodegen = fst . runCodegen

instruction :: IRType -> Text -> (IRValue -> Codegen a) -> Codegen a
instruction t s next = do
  var <- gets (Local t . showt)
  modify (+1)
  tell [encode var <+> "=" <+> s]
  next var

interpreter :: IRInstrF IRValue IRType (Codegen a) -> Codegen a
interpreter =
  \case
    IAdd t v w next ->
      instruction t ("add" <+> encode t <+> encode v <> "," <+> encode w) next
    ISub t v w next ->
      instruction t ("sub" <+> encode t <+> encode v <> "," <+> encode w) next
    IMul t v w next ->
      instruction t ("mul" <+> encode t <+> encode v <> "," <+> encode w) next
    IAlloc t next ->
      instruction (TPtr t) ("alloca" <+> encode t) next
    ILoad t v next ->
      instruction t ("load" <+> encode t <> "," <+> encode (IRTyped v)) next
    IBCast v t next ->
      instruction t ("bitcast" <+> encode (IRTyped v) <+> "to" <+> encode t) next
    IGep t u v w next ->
      instruction (ptr (member t w)) ("getelementptr" <+> encode t <> "," <+> encode (IRTyped u) <> "," <+> encode (IRTyped v) <> "," <+> encode (IRTyped w)) next
    ICall t v vs next ->
      instruction t ("call" <+> encode t <+> encode v <> "(" <> encode (IRTyped <$> vs) <> ")") next
    IStore v w next -> do
      tell ["store" <+> encode (IRTyped v) <> "," <+> encode (IRTyped w)] 
      next v
    IRet t v next -> do
      tell ["ret" <+> encode t <+> encode v]
      next v
    _ ->
      error "Not implemented"
  where
    member (TName _ t) n        = member t n
    member (TStruct ts) (I32 n) = ts !! fromIntegral n

--runIRCode :: IRCode a -> ((a, [Text]), Int)
--runIRCode code = runState (runWriterT (iterM interpreter code)) 1

--interpreter :: IRInstrF IRValue IRType (Codegen a) -> Codegen a

type IRCode = IRInstr IRValue IRType

runIRCode :: IRCode a -> Codegen a
runIRCode = iterM interpreter
--
--runX :: Free (IRInstrF IRValue IRType) a -> a
--runX = undefined


-- --runIRCode :: Set Name -> IRCode a -> [Text]
--runIRCode env code = evalState (runWriterT (iterM interpreter zzz)) 1
--  where
--    zzz = runStateT (runReaderT (unIRInstr code) env) initialIRState 

-- --

newtype IREval a = IREval { unIREval :: ReaderT (Environment IRValue) (StateT IRState IRCode) a }
  deriving (Functor, Applicative, Monad, MonadReader (Environment IRValue), MonadState IRState, MonadFree (IRInstrF IRValue IRType))

runIREval :: Environment IRValue -> IREval a -> IRCode (a, IRState)
runIREval env val = runStateT (runReaderT (unIREval val) env) initialIRState

pasta :: Expr Type -> IREval IRValue
pasta e = do
  a <- irEval e
  ret (irTypeOf a) a

irEval :: Expr Type -> IREval IRValue
irEval =
  \case
    EVar (Label t var) -> do
      env <- ask
      pure $ case envLookup var env of
        Just val -> val
        Nothing  -> Global (toIRType2 t) var

    ELit prim -> 
      pure (irPrim prim)

    ELet vs e1 -> do
      vals <- forM vs $ \(Label t n, e) -> do
        v <- irEval e
        pure (n, v)
      local (envInserts vals) (irEval e1)

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

    EApp t e1 es -> do
      v1 <- irEval e1
      vs <- NonEmpty.toList <$> traverse irEval es
      if arity t > 0
        then do
          -- Partially applied function
          let (t1, ts) = irFunTypeOf e1
              ts' = drop (length vs) ts
              s = [ TFun t1 (ptr i8 : ts'), TFun t1 ts ] <> (irTypeOf <$> vs)
          t2 <- topType "Closure" (struct s)
          v2 <- topDefine "resume" t1 ((ptr i8, "f") : ixArgs ts') (resume t1 ts t2 vs)
          partiallyApply t2 v1 v2 vs
        else case v1 of
          v@Local{} -> uncurry (callPtr v vs) (irFunTypeOf e1)
          Global{}  -> call (toIRType t) v1 vs

    e -> error (show e)

callPtr :: (MonadFree (IRInstrF IRValue IRType) m) => IRValue -> [IRValue] -> IRType -> [IRType] -> m IRValue
callPtr v vs t ts = do
  r1 <- bitcast v (ptr (struct [t1]))
  r2 <- getelementptr (struct [t1]) r1 (I32 0) (I32 0)
  r3 <- load t1 r2
  call t r3 (v : vs)
--  r4 <- call t r3 (v : vs)
--  ret t r4
  where
    t1 = fun t (ptr i8 : ts)

partiallyApply :: (MonadFree (IRInstrF IRValue IRType) m) => IRType -> IRValue -> IRValue -> [IRValue] -> m IRValue
partiallyApply tc v1 td vs = do
  r1 <- alloca tc
  r2 <- getelementptr tc r1 (I32 0) (I32 0)
  store td r2
  r3 <- getelementptr tc r1 (I32 0) (I32 1)
  store v1 r3
  forM_ (zip vs [2 ..]) $ \(a, n) -> do
    r <- getelementptr tc r1 (I32 0) (I32 n)
    store a r
  bitcast r1 (ptr i8)

resume :: (MonadFree (IRInstrF IRValue IRType) m) => IRType -> [IRType] -> IRType -> [IRValue] -> m IRValue
resume t1 ts tc vs = do
  r1 <- bitcast (Local (ptr i8) "f") (ptr tc)
  r2 <- getelementptr tc r1 (I32 0) (I32 1)
  r3 <- load (TFun t1 ts) r2
  args <- forM (zip vs [2 ..]) $ \(a, n) -> do
    r <- getelementptr tc r1 (I32 0) (I32 n)
    load (irTypeOf a) r
  r4 <- call t1 r3 (args <> (uncurry Local <$> ixArgs (drop (length vs) ts)))
  ret t1 r4

ixArgs :: [a] -> [(a, Text)]
ixArgs ts = ts `zip` (("a" <>) . showt <$> [0 :: Int ..]) 


--resume :: IRType -> IRType -> [IRValue] -> IRCode IRValue
--resume tfun@(TFun t1 ts) tc vs = do
--  r1 <- bitcast (Local (ptr i8) "f") (ptr tc)
--  r2 <- getelementptr tc r1 (I32 0) (I32 1)
--  r3 <- load tfun r2
--  args <- forM (zip vs [2 ..]) $ \(a, n) -> do
--    r <- getelementptr tc r1 (I32 0) (I32 n)
--    load (irTypeOf a) r
--  r4 <- call t1 r3 (args <> fooz)
--  ret t1 r4
--  where
--    fooz = uncurry Local <$> extra
--    extra = zip (drop (length vs) ts) (("a" <>) . showt <$> [0 :: Int ..]) 

-- --
-- 
bork :: Name -> IREval Name
bork n = do
  count <- gets globalCount
  modifyGlobalCount (+1)
  pure (n <> "." <> showt count)

topDefine :: Name -> IRType -> [(IRType, Name)] -> IRCode IRValue -> IREval IRValue
topDefine n t args code = do
  name <- bork n
  insertDefinition name (CDefine t args (initialIRState <$ code))
  pure (Global (TFun t (fst <$> args)) name)

topDeclare n = do
  undefined

topType :: Name -> IRType -> IREval IRType
topType n ty = do
  name <- bork n
  insertDefinition name (CType ty)
  pure (TName name ty)

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
    , Local (ptr i8) "g.5"
    ) 
  ]


--baz :: Expr Type -> ((IRValue, IRState), [Text])
--baz x = runCodegen xx2
--  where
--    xx2 = runIRCode (runStateT (runReaderT (unIREval xx1) testEnv) initialIRState)
--    xx1 = irEval x
--
--
--zooz = baz xx1
--  where
--    xx1 = EOp2 (TCon "Int32" [],OAdd) (EVar (Label (TCon "Int32" []) "x.1")) (EVar (Label (TCon "Int32" []) "y.2"))
--
--zooz5 = baz xx1
--  where
--    xx1 = EApp (TCon "Int32" []) (EVar (Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "f")) (ELit (PInt32 100) :| [])
--
--zooz2 = baz xx1
--  where
--    xx1 = EApp (TCon "Int32" []) (EVar (Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "g.5")) (ELit (PInt32 100) :| [])
--
--zooz3 = baz xx1
--  where
--    xx1 = EApp (TCon "->" [TCon "Int32" [],TCon "Int32" []]) 
--            (EVar (Label (TCon "->" [TCon "Int32" [],TCon "->" [TCon "Int32" [],TCon "Int32" []]]) "$fun.0")) 
--            (ELit (PInt32 1) :| [])
--

-- ---zooz2 :: [Text]
--zooz2 = runIRCode xx1
--  where
--    xx1 = irEval (EApp (TCon "Int32" []) (EVar (Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "g.5")) (ELit (PInt32 100) :| []))

-- --zzz :: IRCode ()
-- --zzz = do
-- --  r1 <- bitcast (ptr i8) "f" (ptr _Func)
-- --  r2 <- getelementptr _Func r1 (I32 0) (I32 1)
-- --  r3 <- load (fun i32 [i32, i32]) r2
-- --  ret i32 r2
-- 
-- --zooz3 :: [Text]
-- zooz3 = runIRCode testEnv xx1
--   where
--     xx1 = irEval (EApp (TCon "->" [TCon "Int32" [],TCon "Int32" []]) 
--             (EVar (Label (TCon "->" [TCon "Int32" [],TCon "->" [TCon "Int32" [],TCon "Int32" []]]) "$fun.0")) 
--             (ELit (PInt32 1) :| []))


--newtype IREval a = IREval { unIREval :: ReaderT (Environment IRValue) (StateT IRState IRCode) a }
--

dict1 :: Dictionary Type
dict1 =
  Map.fromList [
    ( "$fun.0", 
        ( [Label (TCon "Int32" []) "x.1",Label (TCon "Int32" []) "y.2"]
        , EOp2 (TCon "Int32" [],OAdd) (EVar (Label (TCon "Int32" []) "x.1")) (EVar (Label (TCon "Int32" []) "y.2")))
        )


  , ( "$fun.1", 
        ( [Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "g.5"]
        , EApp (TCon "Int32" []) (EVar (Label (TCon "->" [TCon "Int32" [],TCon "Int32" []]) "g.5")) 
           (ELit (PInt32 100) :| [])
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

crew9 :: Dictionary Type -> Map Name (IRConstruct (IRCode IRState))
crew9 dict = Map.foldrWithKey foo mempty dict

--foo :: Name -> ([Label Type], Expr Type) -> Map Name (IRConstruct (IRCode IRState)) -> Map Name (IRConstruct (IRCode IRState))
foo name as m = cc <> m
  where
    cc = definitions bb
    bb :: IRState
    bb = execCodegen (runIRCode aa)
    aa = crew5 name as


play :: Map Name (IRConstruct (IRCode IRState)) -> [Text]
play m = Map.elems (Map.mapWithKey goo m)

goo :: Name -> IRConstruct (IRCode IRState) -> Text 
goo = encode <$$> IRNamed

moo dict = Text.putStrLn cake
  where
    cake = Text.intercalate "\n" snake
    snake = play (crew9 dict)



--newtype IREval a = IREval { unIREval :: ReaderT (Environment IRValue) (StateT IRState IRCode) a }
--  deriving (Functor, Applicative, Monad, MonadReader (Environment IRValue), MonadState IRState, MonadFree (IRInstrF IRValue IRType))

--CDefine  !IRType ![(IRType, Name)] a

--faz :: IRCode (IRValue, IRState) -> Name -> IRState -> IRState
faz :: Name -> IRType -> [(IRType, Name)] -> IRCode IRState -> IRState -> IRState
faz name t args code IRState{..} = 
  IRState{ definitions = Map.insert name (CDefine t args code) definitions , .. }

-- = IRState { definitions = f definitions, .. }

crew5 :: Name -> ([Label Type], Expr Type) -> IRCode IRState
crew5 name (lls, e) = do
  fmap (faz name (toIRType (typeOf e)) args zz) zz
  where
    zz :: IRCode IRState
    zz = fmap snd dd
    dd :: IRCode (IRValue, IRState)
    --dd = runIREval (envFromList foo) (irEval e)
    dd = runIREval (envFromList foo) (pasta e)
    foo = [(n, Local (toIRType t) n) | Label t n <- lls ]
    args :: [(IRType, Name)]
    args = swap <$> (toIRType <$$> unLabel <$> lls)

