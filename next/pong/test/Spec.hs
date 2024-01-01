{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Control.Arrow ((>>>))
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Map.Strict ((!))
import Data.Set (Set)
import Pong
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
    [] -> error "Empty list"

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
      [ -- let id = lam(x) => x in if id(true) then id(1) else 2
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
      [ -- let f = 1 ; g = 2 in let f  = 2 in f
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

--

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

--    testFlattenLambdas
--    testconvertClosures
--    testInsertImplicitArgs
--    testFlattenLets
--    testLiftLambdas
--    testToLExpr
--    testEval


