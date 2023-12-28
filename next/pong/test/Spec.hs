{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Data.Fix
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map, (!))
import Data.Set (Set)
import Data.Foldable
import Pong
import System.IO.Unsafe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

testTypeEnv :: TypeEnv
testTypeEnv = envFromList
  [ ("Cons", scheme [0] (TVar 0 ~> TCon "List" [TVar 0] ~> TCon "List" [TVar 0]))
  , ("Nil", scheme [0] (TCon "List" [TVar 0]))
  ]

maxStrLen :: Int
maxStrLen = 40

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

lPat :: LExpr -> [([Label Type], LExpr)] -> LExpr
lPat e =
  \case 
    a:as -> LPat e (a:|as)
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
      tvarsShouldEqual tInt mempty
      tvarsShouldEqual tFloat mempty
      tvarsShouldEqual tDouble mempty
      tvarsShouldEqual tChar mempty
      tvarsShouldEqual tString mempty
      tvarsShouldEqual (TVar 0) (Set.singleton 0)
      tvarsShouldEqual (TCon "List" [tUnit]) mempty
      tvarsShouldEqual (TCon "List" [TVar 0]) (Set.singleton 0)
      tvarsShouldEqual (TCon "List" [TCon "List" [TVar 0]]) (Set.singleton 0)
      tvarsShouldEqual (tArr tInt tInt) mempty
      tvarsShouldEqual (tArr (TCon "List" [TVar 0]) tInt) (Set.singleton 0)
      tvarsShouldEqual (tArr (TVar 0) (TVar 0)) (Set.singleton 0)
      tvarsShouldEqual (tArr (TVar 0) (TVar 1)) (Set.fromList [0, 1])
      tvarsShouldEqual (tArr tInt (TVar 0)) (Set.singleton 0)
    describe "Expr Type" $ do
      tvarsShouldEqual (EVar (Label tChar "x")) mempty
      tvarsShouldEqual (EVar (Label (TVar 0) "x")) (Set.singleton 0)
      tvarsShouldEqual (ELit (PInt 42) :: Expr Type) mempty
      tvarsShouldEqual (ePat (EVar (Label (TVar 0) "x")) [clause [Label (TVar 1) "Cons", Label (TVar 2) "x"] (ELit (PInt 1)), clause [Label (TVar 3) "Nil"] (ELit (PInt 2))]) (Set.fromList [0, 1, 2, 3])

--

freeShouldEqual :: Expr () -> Set Name -> SpecWith ()
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
        , Set.fromList ["v"]
        )
      , ( eLet [(Label () "x", ELit (PInt 1))] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y")))
        , Set.fromList ["y"]
        )
      , ( eLet [(Label () "z", ELit (PInt 1))] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y")))
        , Set.fromList ["x", "y"]
        )
      , ( eLam [Label () "x"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y")))
        , Set.fromList ["y"]
        )
      , ( eLam [Label () "x", Label () "y"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y")))
        , mempty
        )
      , ( -- let f = lam(x) => x + y in 1
          eLet [(Label () "f", eLam [Label () "x"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y"))))] (ELit (PInt 1))
        , Set.fromList ["y"]
        )
      , ( -- let f = lam(x) => x + y in p
          eLet [(Label () "f", eLam [Label () "x"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y"))))] (EVar (Label () "p"))
        , Set.fromList ["y", "p"]
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
        ( eLet [(Label () "x", ELit (PInt 5))] (EOp2 ((), OAdd) (EVar (Label () "x")) (ELit (PInt 1)))
        , eLet [(Label 0 "x", ELit (PInt 5))] (EOp2 (1, OAdd) (EVar (Label 2 "x")) (ELit (PInt 1)))
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
        ( EIf (EOp2 ((), OEq) (ELit (PBool True)) (ELit (PBool False))) (ELit (PInt 1)) (ELit (PInt 100))
        , EIf (EOp2 (0, OEq) (ELit (PBool True)) (ELit (PBool False))) (ELit (PInt 1)) (ELit (PInt 100))
        )
      , -- match x { | Cons(x) => 1 | Nil => 2 }
        ( ePat (EVar (Label () "x")) [clause [Label () "Cons", Label () "x"] (ELit (PInt 1)), clause [Label () "Nil"] (ELit (PInt 2))]
        , ePat (EVar (Label 0 "x")) [clause [Label 1 "Cons", Label 2 "x"] (ELit (PInt 1)), clause [Label 3 "Nil"] (ELit (PInt 2))]
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
      (eLam [Label tInt "x"] (ELit (PInt 0)))
      (tInt ~> tInt)
    typeOfShouldEqual
      (eLet [(Label (TVar 2 ~> TVar 2) "id", eLam [Label (TVar 2) "x"] (EVar (Label (TVar 2) "x")))] (EIf (eApp tBool (EVar (Label (tBool ~> tBool) "id")) [ELit (PBool True)]) (eApp tInt (EVar (Label (tInt ~> tInt) "id")) [ELit (PInt 1)]) (ELit (PInt 2))))
      tInt
    typeOfShouldEqual
      (EOp2 (tInt, OAdd) (ELit (PInt 1)) (ELit (PInt 1)))
      tInt

--

inferExprShouldEqual :: Expr () -> Either String (Expr Type) -> SpecWith ()
inferExprShouldEqual input result =
  it (trimStr is) ((canonicalExpr <$> typeExpr testTypeEnv input) == result)
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
                [ELit (PInt 1)])
              (ELit (PInt 2)))
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
              (eApp tInt
                (EVar (Label (tInt ~> tInt) "id"))
                [ELit (PInt 1)])
              (ELit (PInt 2)))
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
                      (ELit (PInt 0)))
                    (ELit (PInt 1))
                    (EOp2
                      ((), OMul)
                      (EVar (Label () "n"))
                      (eApp
                        ()
                        (EVar (Label () "f"))
                        [EOp2
                          ((), OSub)
                          (EVar (Label () "n"))
                          (ELit (PInt 1))
                        ])
                    ))
              )
            ]
            (eApp ()
              (EVar (Label () "f"))
              [ELit (PInt 5)])
        , Right $ eLet
            [
              ( Label (tInt ~> tInt) "f"
              ,  eLam
                  [Label tInt "n"]
                  (EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt "n"))
                      (ELit (PInt 0)))
                    (ELit (PInt 1))
                    (EOp2
                      (tInt, OMul)
                      (EVar (Label tInt "n"))
                      (eApp
                        tInt
                        (EVar (Label (tInt ~> tInt) "f"))
                        [EOp2
                          (tInt, OSub)
                          (EVar (Label tInt "n"))
                          (ELit (PInt 1))
                        ])
                    ))
              )
            ]
            (eApp tInt
              (EVar (Label (tInt ~> tInt) "f"))
              [ELit (PInt 5)])
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
                    [ELit (PInt 1)]
                )
              ]
              (eApp
                ()
                (EVar (Label () "g"))
                [ELit (PInt 2)]))
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
                ( Label (tInt ~> tInt) "g"
                , eApp
                  (tInt ~> tInt)
                  (EVar (Label (tInt ~> tInt ~> tInt) "f"))
                  [ELit (PInt 1)]
                )
              ]
              (eApp
                tInt
                (EVar (Label (tInt ~> tInt) "g"))
                [ELit (PInt 2)]))
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
                    [ELit (PInt 1)])
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
                      (ELit (PInt 1)))
                )
              ]
              (eApp
                ()
                (EVar (Label () "f"))
                [EVar (Label () "h")])
            )
        , Right $ eLet
            [
              ( Label ((tInt ~> TVar 0) ~> TVar 0) "f"
              , eLam
                  [Label (tInt ~> TVar 0) "g"]
                  (eApp
                    (TVar 0)
                    (EVar (Label (tInt ~> TVar 0) "g"))
                    [ELit (PInt 1)])
              )
            ]
            (eLet
              [
                ( Label (tInt ~> tInt) "h"
                , eLam
                    [Label tInt "x"]
                    (EOp2
                      (tInt, OAdd)
                      (EVar (Label tInt "x"))
                      (ELit (PInt 1)))
                )
              ]
              (eApp
                tInt
                (EVar (Label ((tInt ~> tInt) ~> tInt) "f"))
                [EVar (Label (tInt ~> tInt) "h")])
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
              [ELit (PInt 3), ELit (PInt 5)])
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
              tInt
              (EVar (Label (tInt ~> tInt ~> tInt) "f"))
              [ELit (PInt 3), ELit (PInt 5)])
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
                    , clause [Label () "Nil"] (ELit (PInt 0))
                    ])
              )
            ]
            (eApp
              ()
              (EVar (Label () "f"))
              [ eApp
                  ()
                  (EVar (Label () "Cons"))
                  [ ELit (PInt 5)
                  , EVar (Label () "Nil")
                  ]
              ])
        , Right $ eLet
            [
              ( Label (TCon "List" [tInt] ~> tInt) "f"
              , eLam
                  [Label (TCon "List" [tInt]) "x"]
                  (ePat
                    (EVar (Label (TCon "List" [tInt]) "x"))
                    [ clause [Label (tInt ~> TCon "List" [tInt] ~> TCon "List" [tInt]) "Cons", Label tInt "y", Label (TCon "List" [tInt]) "ys"] (EVar (Label tInt "y"))
                    , clause [Label (TCon "List" [tInt]) "Nil"] (ELit (PInt 0))
                    ])
              )
            ]
            (eApp
              tInt
              (EVar (Label (TCon "List" [tInt] ~> tInt) "f"))
              [ eApp
                  (TCon "List" [tInt])
                  (EVar (Label (tInt ~> TCon "List" [tInt] ~> TCon "List" [tInt]) "Cons"))
                  [ ELit (PInt 5)
                  , EVar (Label (TCon "List" [tInt]) "Nil")
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
              [ELit (PInt 5)])
        , Left "Cannot unify"
        )
      , -- let x = 1 in let f = lam(y) => x + y in f(1)
        ( eLet
            [
              ( Label () "x"
              , ELit (PInt 1)
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
                [ELit (PInt 1)]))
        , Right $ eLet
            [
              ( Label tInt "x"
              , ELit (PInt 1)
              )
            ]
            (eLet
              [
                ( Label (tInt ~> tInt) "f"
                , eLam
                    [Label tInt "y"]
                    (EOp2
                      (tInt, OAdd)
                      (EVar (Label tInt "x"))
                      (EVar (Label tInt "y")))
                )
              ]
              (eApp
                tInt
                (EVar (Label (tInt ~> tInt) "f"))
                [ELit (PInt 1)]))
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
                    [ELit (PInt 100)])
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
                [ELit (PInt 1)]))
        , Right $ eLet
            [
              ( Label ((tInt ~> TVar 0) ~> TVar 0) "f"
              , eLam
                  [Label (tInt ~> TVar 0) "g"]
                  (eApp
                    (TVar 0)
                    (EVar (Label (tInt ~> TVar 0) "g"))
                    [ELit (PInt 100)])
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
                tInt
                (eApp
                  (tInt ~> tInt)
                  (EVar (Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "f"))
                  [EVar (Label (tInt ~> tInt ~> tInt) "h")])
                [ELit (PInt 1)]))
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
                        [ELit (PInt 1)])
                  )
                ]
                (eApp
                  ()
                  (EVar (Label () "g"))
                  [EVar (Label () "f"), ELit (PInt 2)]))
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
                  ( Label ((tInt ~> TVar 1) ~> TVar 1) "g"
                  , eLam
                      [Label (tInt ~> TVar 1) "$f"]
                      (eApp
                        (TVar 1)
                        (EVar (Label (tInt ~> TVar 1) "$f"))
                        [ELit (PInt 1)])
                  )
                ]
                (eApp
                  tInt
                  (EVar (Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "g"))
                  [EVar (Label (tInt ~> tInt ~> tInt) "f"), ELit (PInt 2)]))
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
                        (ELit (PInt 1))
                    ])
              )
            ,
              ( Label () "g"
              , eLam [Label () "y"] 
                  (EIf
                    (EOp2
                      ((), OEq)
                      (EVar (Label () "y"))
                      (ELit (PInt 100)))
                    (ELit (PInt 100))
                    (eApp ()
                      (EVar (Label () "f"))
                      [ EVar (Label () "y") ]))
              )
            ]
            (eApp ()
              (EVar (Label () "f"))
              [ ELit (PInt 1) ])
          , Right $ eLet
            [
              ( Label (tInt ~> tInt) "f"
              , eLam [Label tInt "x"] 
                  (eApp tInt
                    (EVar (Label (tInt ~> tInt) "g"))
                    [ EOp2
                        (tInt, OAdd)
                        (EVar (Label tInt "x"))
                        (ELit (PInt 1))
                    ])
              )
            ,
              ( Label (tInt ~> tInt) "g"
              , eLam [Label tInt "y"] 
                  (EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt "y"))
                      (ELit (PInt 100)))
                    (ELit (PInt 100))
                    (eApp tInt
                      (EVar (Label (tInt ~> tInt) "f"))
                      [ EVar (Label tInt "y") ]))
              )
            ]
            (eApp tInt
              (EVar (Label (tInt ~> tInt) "f"))
              [ ELit (PInt 1) ])
          )
--      , --
--        ( undefined
--        , undefined
--        )
      ]

--

qualifyNamesShouldEqual :: Expr () -> Expr () -> SpecWith ()
qualifyNamesShouldEqual input result =
  it (trimStr is) (qualifyNames input == result)
  where
    is = "qualifyNames (" <> show input <> ") == " <> show result

testQualifyNames :: SpecWith ()
testQualifyNames = do
  describe "qualifyNames" $ do
    mapM_ (uncurry qualifyNamesShouldEqual)
      [ -- let f = 1 ; g = 2 in let f  = 2 in f
        ( eLet [(Label () "f", ELit (PInt 1)), (Label () "g", ELit (PInt 2))] (eLet [(Label () "f", ELit (PInt 2))] (EVar (Label () "f")))
        -- let f.0 = 1 ; g.1 = 2 in let f.2  = 2 in f.2
        , eLet 
            [
              (Label () "f.0", ELit (PInt 1))
            , (Label () "g.1", ELit (PInt 2))
            ] 
            (eLet 
              [(Label () "f.2", ELit (PInt 2))] 
              (EVar (Label () "f.2"))
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
        ( (eLet [(Label () "f", eLam [Label () "x"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "y"))))] (ELit (PInt 1)), "y", "z")
        , eLet [(Label () "f", eLam [Label () "x"] (EOp2 ((), OAdd) (EVar (Label () "x")) (EVar (Label () "z"))))] (ELit (PInt 1))
        )
      ]

 
--

defMapShouldEqual :: Expr () -> DefMap Type -> SpecWith ()
defMapShouldEqual input result =
  forM_ (Map.keys result) $
    \key -> it (show key) (Right (result ! key) == ((!) <$> map <*> pure key))
  where
    map = defMap . canonicalExpr <$> typeExpr mempty input

testDefMap :: SpecWith ()
testDefMap =
  describe "defMap" $
    mapM_ (uncurry defMapShouldEqual)
      [ 
        ( eLet
            [
              ( Label () "n"
              , ELit (PInt 500)
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
                          (ELit (PInt 1))
                      ])
                )
              ,
                ( Label () "g"
                , eLam [Label () "y"] 
                    (EIf
                      (EOp2
                        ((), OEq)
                        (EVar (Label () "y"))
                        (ELit (PInt 100)))
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
                      [ ELit (PInt 1) ]
                  ])
              ))
        , Map.fromList
            [ 
              ( "$fun.0", 
                ( [Label tInt "x"] 
                , eApp tInt
                    (EVar (Label (tInt ~> tInt) "g"))
                    [ EOp2
                        (tInt, OAdd)
                        (EVar (Label tInt "x"))
                        (ELit (PInt 1))
                    ]
                ) 
              )
            , ( "$fun.1", 
                ( [Label tInt "y"]
                , EIf
                    (EOp2
                      (tBool, OEq)
                      (EVar (Label tInt "y"))
                      (ELit (PInt 100)))
                    (EVar (Label tInt "n"))
                    (eApp tInt
                      (EVar (Label (tInt ~> tInt) "f"))
                      [ EVar (Label tInt "y") ])
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
                      ( Label tInt "n"
                      , ELit (PInt 500)
                      )
                    ]
                    (eLet
                      [
                        ( Label (tInt ~> tInt) "f"
                        , EVar (Label (tInt ~> tInt) "$fun.0")
                        )
                      ,
                        ( Label (tInt ~> tInt) "g"
                        , EVar (Label (tInt ~> tInt) "$fun.1")
                        )
                      ]
                      (eLet
                        [
                          ( Label (TVar 0 ~> TVar 0) "z"
                          , EVar (Label (TVar 0 ~> TVar 0) "$fun.2") 
                          )
                        ]
                        (eApp tInt
                          (EVar (Label (tInt ~> tInt) "z"))
                          [ eApp tInt
                              (EVar (Label (tInt ~> tInt) "f"))
                              [ ELit (PInt 1) ]
                          ])
                      ))
                    ) 
              )
            ]
        )
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
              (eApp tInt
                (EVar (Label (tInt ~> tInt) "id"))
                [ELit (PInt 1)])
              (ELit (PInt 2)))
        , eLet
            [
              ( Label (tBool ~> tBool) "id.0"
              , eLam
                  [Label tBool "x"]
                  (EVar (Label tBool "x"))
              )
            , ( Label (tInt ~> tInt) "id.1"
              , eLam
                  [Label tInt "x"]
                  (EVar (Label tInt "x"))
              )
            ]
            (EIf
              (eApp tBool
                (EVar (Label (tBool ~> tBool) "id.0"))
                [ELit (PBool True)])
              (eApp tInt
                (EVar (Label (tInt ~> tInt) "id.1"))
                [ELit (PInt 1)])
              (ELit (PInt 2))))
      , ( eLet
            [
              ( Label ((tInt ~> TVar 0) ~> TVar 0) "f"
              , eLam
                  [Label (tInt ~> TVar 0) "g"]
                  (eApp
                    (TVar 0)
                    (EVar (Label (tInt ~> TVar 0) "g"))
                    [ELit (PInt 100)])
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
                tInt
                (eApp
                  (tInt ~> tInt)
                  (EVar (Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "f"))
                  [EVar (Label (tInt ~> tInt ~> tInt) "h")])
                [ELit (PInt 1)]))
        , eLet
            [
              ( Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "f.0"
              , eLam
                  [Label (tInt ~> tInt ~> tInt) "g"]
                  (eApp
                    (tInt ~> tInt)
                    (EVar (Label (tInt ~> tInt ~> tInt) "g"))
                    [ELit (PInt 100)])
              )
            ]
            (eLet
              [
                ( Label (tInt ~> tInt ~> tInt) "h.0"
                , eLam
                    [Label tInt "x", Label tInt "y"]
                    (EOp2
                      (tInt, OAdd)
                      (EVar (Label tInt "x"))
                      (EVar (Label tInt "y")))
                )
              ]
              (eApp
                tInt
                (eApp
                  (tInt ~> tInt)
                  (EVar (Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "f.0"))
                  [EVar (Label (tInt ~> tInt ~> tInt) "h.0")])
                [ELit (PInt 1)]))
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
                  ( Label ((tInt ~> TVar 1) ~> TVar 1) "g"
                  , eLam
                      [Label (tInt ~> TVar 1) "$f"]
                      (eApp
                        (TVar 1)
                        (EVar (Label (tInt ~> TVar 1) "$f"))
                        [ELit (PInt 1)])
                  )
                ]
                (eApp
                  tInt
                  (EVar (Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "g"))
                  [EVar (Label (tInt ~> tInt ~> tInt) "f"), ELit (PInt 2)]))
          , eLet
              [
                ( Label (tInt ~> tInt ~> tInt) "f.0"
                , eLam
                    [Label tInt "x", Label tInt "y"]
                    (EOp2
                      (tInt, OAdd)
                      (EVar (Label tInt "x"))
                      (EVar (Label tInt "y")))
                )
              ]
              (eLet
                [
                  ( Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "g.0"
                  , eLam
                      [Label (tInt ~> tInt ~> tInt) "$f"]
                      (eApp
                        (tInt ~> tInt)
                        (EVar (Label (tInt ~> tInt ~> tInt) "$f"))
                        [ELit (PInt 1)])
                  )
                ]
                (eApp
                  tInt
                  (EVar (Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "g.0"))
                  [EVar (Label (tInt ~> tInt ~> tInt) "f.0"), ELit (PInt 2)]))
          )
--      , ( undefined
--        , undefined
--        )
      ]

--

-- flattenAppsShouldEqual :: Expr () -> Expr () -> SpecWith ()
-- flattenAppsShouldEqual input result =
--   it (trimStr is) (flattenApps input == result)
--   where
--     is = "flattenApps (" <> show input <> ") == " <> show result
-- 
-- testFlattenApps :: SpecWith ()
-- testFlattenApps =
--   describe "flattenApps" $ do
--     mapM_ (uncurry flattenAppsShouldEqual)
--       [ ( -- (f(1))(2)
--           eApp () (eApp () (EVar (Label () "f")) [ ELit (PInt 1) ]) [ ELit (PInt 2) ]
--         , -- f(1, 2)
--           eApp () (EVar (Label () "f")) [ ELit (PInt 1), ELit (PInt 2) ]
--         )
--       ]
-- 
-- --
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
--               [ELit (PInt 1)])
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
--               [EVar (Label () "x"), ELit (PInt 1)])
--         )
--         -- let x = 1 in let f = lam(y) => x + y in f(1)
--       , ( eLet
--             [
--               ( Label () "x"
--               , ELit (PInt 1)
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
--                 [ELit (PInt 1)]))
--         -- let x = 1 in let f = lam($x, y) => $x + y in f(x, 1)
--         , eLet
--             [
--               ( Label () "x"
--               , ELit (PInt 1)
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
--                 [EVar (Label () "x"), ELit (PInt 1)]))
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
--                     [ELit (PInt 1)]
--                 )
--               ]
--               (eApp
--                 ()
--                 (EVar (Label () "g"))
--                 [ELit (PInt 2)]))
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
--                       [ELit (PInt 1)])
--                 )
--               ]
--               (eApp
--                 ()
--                 (EVar (Label () "g"))
--                 [EVar (Label () "f"), ELit (PInt 2)]))
--         )
--       , -- let n = 500 in let f = lam(x) => g(x + 1); g = lam(y) => if y == 100 then n else f(y) in f(1)
--         ( eLet
--             [
--               ( Label () "n"
--               , ELit (PInt 500)
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
--                           (ELit (PInt 1))
--                       ])
--                 )
--               ,
--                 ( Label () "g"
--                 , eLam [Label () "y"] 
--                     (EIf
--                       (EOp2
--                         ((), OEq)
--                         (EVar (Label () "y"))
--                         (ELit (PInt 100)))
--                       (EVar (Label () "n"))
--                       (eApp ()
--                         (EVar (Label () "f"))
--                         [ EVar (Label () "y") ]))
--                 )
--               ]
--               (eApp ()
--                 (EVar (Label () "f"))
--                 [ ELit (PInt 1) ]))
--         , -- let n = 500 in let f = lam($g, x) => $g(x + 1); g = lam($f, $n, y) => if y == 100 then $n else $f(y) in f(g(f, n), 1) 
--           eLet
--             [
--               ( Label () "n"
--               , ELit (PInt 500)
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
--                           (ELit (PInt 1))
--                       ])
--                 )
--               ,
--                 ( Label () "g"
--                 , eLam [Label () "y"] 
--                     (EIf
--                       (EOp2
--                         ((), OEq)
--                         (EVar (Label () "y"))
--                         (ELit (PInt 100)))
--                       (EVar (Label () "n"))
--                       (eApp ()
--                         (EVar (Label () "f"))
--                         [ EVar (Label () "y") ]))
--                 )
--               ]
--               (eApp ()
--                 (EVar (Label () "f"))
--                 [ ELit (PInt 1) ]))
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
--               ( Label (tInt ~> tInt ~> tInt) "f.0"
--               , eLam
--                   [Label tInt "x", Label tInt "y"]
--                   (EOp2
--                     (tInt, OAdd)
--                     (EVar (Label tInt "x"))
--                     (EVar (Label tInt "y")))
--               )
--             ]
--             (eLet
--               [
--                 ( Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "g.0"
--                 , eLam
--                     [Label (tInt ~> tInt ~> tInt) "$f"]
--                     (eApp
--                       (tInt ~> tInt)
--                       (EVar (Label (tInt ~> tInt ~> tInt) "$f"))
--                       [ELit (PInt 1)])
--                 )
--               ]
--               (eApp
--                 tInt
--                 (EVar (Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "g.0"))
--                 [EVar (Label (tInt ~> tInt ~> tInt) "f.0"), ELit (PInt 2)]))
--         -- let f.0 = lam(x, y) => x + y in let g.0 = lam($f, $.0) => $f(1, $.0) in g.0(f.0, 2)
--         , eLet
--             [
--               ( Label (tInt ~> tInt ~> tInt) "f.0"
--               , eLam
--                   [Label tInt "x", Label tInt "y"]
--                   (EOp2
--                     (tInt, OAdd)
--                     (EVar (Label tInt "x"))
--                     (EVar (Label tInt "y")))
--               )
--             ]
--             (eLet
--               [
--                 ( Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "g.0"
--                 , eLam
--                     [Label (tInt ~> tInt ~> tInt) "$f", Label tInt "$.0"]
--                     (eApp
--                       tInt
--                       (EVar (Label (tInt ~> tInt ~> tInt) "$f"))
--                       [ELit (PInt 1), EVar (Label tInt "$.0")])
--                 )
--               ]
--               (eApp
--                 tInt
--                 (EVar (Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "g.0"))
--                 [EVar (Label (tInt ~> tInt ~> tInt) "f.0"), ELit (PInt 2)]))
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
--               ( Label (tInt ~> tInt ~> tInt) "f.0"
--               , eLam
--                   [Label tInt "x", Label tInt "y"]
--                   (EOp2
--                     (tInt, OAdd)
--                     (EVar (Label tInt "x"))
--                     (EVar (Label tInt "y")))
--               )
--             ]
--             (eLet
--               [
--                 ( Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "g.0"
--                 , eLam
--                     [Label (tInt ~> tInt ~> tInt) "$f", Label tInt "$.0"]
--                     (eApp
--                       tInt
--                       (EVar (Label (tInt ~> tInt ~> tInt) "$f"))
--                       [ELit (PInt 1), EVar (Label tInt "$.0")])
--                 )
--               ]
--               (eApp
--                 tInt
--                 (EVar (Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "g.0"))
--                 [EVar (Label (tInt ~> tInt ~> tInt) "f.0"), ELit (PInt 2)]))
--         , [ Def "_" [] (eApp tInt (EVar (Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "$fun.1")) [EVar (Label (tInt ~> tInt ~> tInt) "$fun.0"), ELit (PInt 2)])
--           , Def "$fun.0" [Label tInt "x", Label tInt "y"] (EOp2 (tInt, OAdd) (EVar (Label tInt "x")) (EVar (Label tInt "y")))
--           , Def "$fun.1" [Label (tInt ~> tInt ~> tInt) "$f", Label tInt "$.0"] (eApp tInt (EVar (Label (tInt ~> tInt ~> tInt) "$f")) [ELit (PInt 1), EVar (Label tInt "$.0")])
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
--                 ( Label (tInt ~> tInt) "id.1"
--                 , eLam
--                     [Label tInt "x"]
--                     (EVar (Label tInt "x"))
--                 )
--               ]
--               (EIf
--                 (eApp tBool
--                   (EVar (Label (tBool ~> tBool) "id.0"))
--                   [ELit (PBool True)])
--                 (eApp tInt
--                   (EVar (Label (tInt ~> tInt) "id.1"))
--                   [ELit (PInt 1)])
--                 (ELit (PInt 2))))
--         , [ Def "_" []
--               (EIf
--                 (eApp tBool
--                   (EVar (Label (tBool ~> tBool) "$fun.0"))
--                   [ELit (PBool True)])
--                 (eApp tInt
--                   (EVar (Label (tInt ~> tInt) "$fun.1"))
--                   [ELit (PInt 1)])
--                 (ELit (PInt 2)))
--           , Def "$fun.0" [Label (TCon "Bool" []) "x"] (EVar (Label (TCon "Bool" []) "x"))
--           , Def "$fun.1" [Label (TCon "Int" []) "x"] (EVar (Label (TCon "Int" []) "x"))
--           ]
--         )
--       , ( eLet
--             [
--               ( Label (TCon "List" [tInt] ~> tInt) "f"
--               , eLam
--                   [Label (TCon "List" [tInt]) "x"]
--                   (ePat
--                     (EVar (Label (TCon "List" [tInt]) "x"))
--                     [ clause [Label (tInt ~> TCon "List" [tInt] ~> TCon "List" [tInt]) "Cons", Label tInt "y", Label (TCon "List" [tInt]) "ys"] (EVar (Label tInt "y"))
--                     , clause [Label (TCon "List" [tInt]) "Nil"] (ELit (PInt 0))
--                     ])
--               )
--             ]
--             (eApp
--               tInt
--               (EVar (Label (TCon "List" [tInt] ~> tInt) "f"))
--               [ eApp
--                   (TCon "List" [tInt])
--                   (EVar (Label (tInt ~> TCon "List" [tInt] ~> TCon "List" [tInt]) "Cons"))
--                   [ ELit (PInt 5)
--                   , EVar (Label (TCon "List" [tInt]) "Nil")
--                   ]
--             ])
--         , [ Def "_" [] (eApp tInt (EVar (Label (TCon "List" [TCon "Int" []] ~> TCon "Int" []) "$fun.0"))
--               [ eApp
--                   (TCon "List" [TCon "Int" []])
--                   (EVar (Label (tInt ~> TCon "List" [TCon "Int" []] ~> TCon "List" [TCon "Int" []]) "Cons"))
--                   [ELit (PInt 5), EVar (Label (TCon "List" [TCon "Int" []]) "Nil")]
--               ])
--           , Def "$fun.0" [Label (TCon "List" [TCon "Int" []]) "x"]
--               (ePat
--                 (EVar (Label (TCon "List" [TCon "Int" []]) "x"))
--                 [ clause [Label (tInt ~> TCon "List" [TCon "Int" []] ~> TCon "List" [TCon "Int" []]) "Cons", Label tInt "y", Label (TCon "List" [TCon "Int" []]) "ys"] (EVar (Label tInt "y"))
--                 , clause [Label (TCon "List" [TCon "Int" []]) "Nil"] (ELit (PInt 0))
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
--           [ Def "_" [] (eApp tInt (EVar (Label ((tInt ~> tInt ~> tInt) ~> tInt ~> tInt) "$fun.1")) [EVar (Label (tInt ~> tInt ~> tInt) "$fun.0"), ELit (PInt 2)])
--           , Def "$fun.0" [Label tInt "x", Label tInt "y"] (EOp2 (tInt, OAdd) (EVar (Label tInt "x")) (EVar (Label tInt "y")))
--           , Def "$fun.1" [Label (tInt ~> tInt ~> tInt) "$f", Label tInt "$.0"] (eApp tInt (EVar (Label (tInt ~> tInt ~> tInt) "$f")) [ELit (PInt 1), EVar (Label tInt "$.0")])
--           ]
--         , [ Def "_" [] (LCal (Label tInt "$fun.1") [LVar (Label (tInt ~> tInt ~> tInt) "$fun.0"), LLit (PInt 2)])
--           , Def "$fun.0" [Label tInt "x", Label tInt "y"] (LOp2 (tInt, OAdd) (LVar (Label tInt "x")) (LVar (Label tInt "y")))
--           , Def "$fun.1" [Label (tInt ~> tInt ~> tInt) "$f", Label tInt "$.0"] (LCal (Label tInt "$f") [LLit (PInt 1), LVar (Label tInt "$.0")])
--           ]
--         )
--       , (
--           [ Def "_" [] (EIf
--               (eApp tBool
--                 (EVar (Label (tBool ~> tBool) "$fun.0"))
--                 [ELit (PBool True)])
--               (eApp tInt
--                 (EVar (Label (tInt ~> tInt) "$fun.1"))
--                 [ELit (PInt 1)])
--               (ELit (PInt 2)))
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
--                       (Label tInt "$fun.1")
--                       [LLit (PInt 1)])
--                 , ([Label tBool "False"], LLit (PInt 2))
--                 ])
--           , Def "$fun.0" [Label (TCon "Bool" []) "x"] (LVar (Label (TCon "Bool" []) "x"))
--           , Def "$fun.1" [Label (TCon "Int" []) "x"] (LVar (Label (TCon "Int" []) "x"))
--           ]
--         )
--       , (
--           [ Def "_" [] (eApp tInt (EVar (Label (TCon "List" [TCon "Int" []] ~> TCon "Int" []) "$fun.0"))
--               [ eApp
--                   (TCon "List" [TCon "Int" []])
--                   (EVar (Label (tInt ~> TCon "List" [TCon "Int" []] ~> TCon "List" [TCon "Int" []]) "Cons"))
--                   [ELit (PInt 5), EVar (Label (TCon "List" [TCon "Int" []]) "Nil")]
--               ])
--           , Def "$fun.0" [Label (TCon "List" [TCon "Int" []]) "x"]
--               (ePat
--                 (EVar (Label (TCon "List" [TCon "Int" []]) "x"))
--                 [ clause [Label (tInt ~> TCon "List" [TCon "Int" []] ~> TCon "List" [TCon "Int" []]) "Cons", Label tInt "y", Label (TCon "List" [TCon "Int" []]) "ys"] (EVar (Label tInt "y"))
--                 , clause [Label (TCon "List" [TCon "Int" []]) "Nil"] (ELit (PInt 0))
--                 ])
--           ]
--         , [ Def "_" [] (LCal (Label (TCon "Int" []) "$fun.0")
--               [ LCon
--                   (Label (TCon "List" [TCon "Int" []]) "Cons")
--                   [LLit (PInt 5), LCon (Label (TCon "List" [TCon "Int" []]) "Nil") []]
--               ])
--           , Def "$fun.0" [Label (TCon "List" [TCon "Int" []]) "x"]
--               (lPat
--                 (LVar (Label (TCon "List" [TCon "Int" []]) "x"))
--                 [ ([Label (tInt ~> TCon "List" [TCon "Int" []] ~> TCon "List" [TCon "Int" []]) "Cons", Label tInt "y", Label (TCon "List" [TCon "Int" []]) "ys"], LVar (Label tInt "y"))
--                 , ([Label (TCon "List" [TCon "Int" []]) "Nil"], LLit (PInt 0))
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
--           [ Def "_" [] (LCal (Label tInt "$fun.1") [LVar (Label (tInt ~> tInt ~> tInt) "$fun.0"), LLit (PInt 2)])
--           , Def "$fun.0"
--               [Label tInt "x", Label tInt "y"]
--               (LOp2 (tInt, OAdd) (LVar (Label tInt "x")) (LVar (Label tInt "y")))
--           , Def "$fun.1"
--               [Label (tInt ~> tInt ~> tInt) "$f", Label tInt "$.0"]
--               (LCal (Label tInt "$f") [LLit (PInt 1), LVar (Label tInt "$.0")])
--           ]
--         , VPrim (PInt 3)
--         )
--       , ( [ Def "_" []
--               (lPat
--                 (LCal
--                   (Label tBool "$fun.0")
--                   [LLit (PBool True)])
--                 [ ([Label tBool "True"],
--                     LCal
--                       (Label tInt "$fun.1")
--                       [LLit (PInt 1)])
--                 , ([Label tBool "False"], LLit (PInt 2))
--                 ])
--           , Def "$fun.0"
--               [Label (TCon "Bool" []) "x"]
--               (LVar (Label (TCon "Bool" []) "x"))
--           , Def "$fun.1"
--               [Label (TCon "Int" []) "x"]
--               (LVar (Label (TCon "Int" []) "x"))
--           ]
--         , VPrim (PInt 1)
--         )
--       , ( [ Def "_" [] (LCal (Label (TCon "Int" []) "$fun.0")
--               [ LCon
--                   (Label (TCon "List" [TCon "Int" []]) "Cons")
--                   [LLit (PInt 5), LCon (Label (TCon "List" [TCon "Int" []]) "Nil") []]
--               ])
--           , Def "$fun.0" [Label (TCon "List" [TCon "Int" []]) "x"]
--               (lPat
--                 (LVar (Label (TCon "List" [TCon "Int" []]) "x"))
--                 [ ([Label (tInt ~> TCon "List" [TCon "Int" []] ~> TCon "List" [TCon "Int" []]) "Cons", Label tInt "y", Label (TCon "List" [TCon "Int" []]) "ys"], LVar (Label tInt "y"))
--                 , ([Label (TCon "List" [TCon "Int" []]) "Nil"], LLit (PInt 0))
--                 ])
--           ]
--         , VPrim (PInt 5)
--         )
--       , ( [ Def "_" [] (LCon (Label (tInt ~> TCon "Maybe" [TCon "Int" []]) "Just") [LLit (PInt 1)])
--           ]
--         , VData "Just" [VPrim (PInt 1)]
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
    testDefMap
    testMonomorphize
--    testFlattenLambdas
--    testFlattenApps
--    testconvertClosures
--    testInsertImplicitArgs
--    testFlattenLets
--    testLiftLambdas
--    testToLExpr
--    testEval



yyy = defMap . canonicalExpr <$> typeExpr mempty xxx

xxx =
  eLet
    [
      ( Label () "n"
      , ELit (PInt 500)
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
                  (ELit (PInt 1))
              ])
        )
      ,
        ( Label () "g"
        , eLam [Label () "y"] 
            (EIf
              (EOp2
                ((), OEq)
                (EVar (Label () "y"))
                (ELit (PInt 100)))
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
              [ ELit (PInt 1) ]
          ])
      ))
 
