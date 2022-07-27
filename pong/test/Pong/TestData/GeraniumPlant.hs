{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.GeraniumPlant where

import Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
import Pong.Lang

fragment1 :: ModuleDefs MonoType TypedExpr
fragment1 =
  Map.fromList
    [
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLet
              (tInt ~> tInt ~> tInt, "f")
              ( eLam
                  ()
                  [(tInt, "x")]
                  ( eLam
                      ()
                      [(tInt, "y")]
                      ( eOp2
                          oAddInt
                          (eVar (tInt, "x"))
                          (eVar (tInt, "y"))
                      )
                  )
              )
              (eLit (PInt 123))
          )
      )
    ]

fragment2 :: ModuleDefs MonoType Ast
fragment2 =
  Map.fromList
    [
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLit (PInt 123)
          )
      )
    ,
      ( (Scheme (tInt ~> tInt ~> tInt), "$lam1")
      , Function
          (fromList [(tInt, "x"), (tInt, "y")])
          ( tInt
          , eOp2
              oAddInt
              (eVar (tInt, "x"))
              (eVar (tInt, "y"))
          )
      )
    ,
      ( (Scheme (tInt ~> tInt ~> tInt), "$lam2")
      , Function
          (fromList [(tInt, "x"), (tInt, "$v0")])
          ( tInt
          , eCall
              ()
              (tInt ~> tInt ~> tInt, "$lam1")
              [eVar (tInt, "x"), eVar (tInt, "$v0")]
          )
      )
    ]

fragment3 :: ModuleDefs MonoType TypedExpr
fragment3 =
  Map.fromList
    [
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLet
              (tInt ~> tInt ~> tInt, "f")
              ( eLam
                  ()
                  [(tInt, "x"), (tInt, "y")]
                  ( eOp2
                      oAddInt
                      (eVar (tInt, "x"))
                      (eVar (tInt, "y"))
                  )
              )
              (eLit (PInt 123))
          )
      )
    ]

fragment9 :: ModuleDefs MonoType Ast
fragment9 =
  Map.fromList
    [
      ( (Scheme (tInt ~> tInt ~> tInt), "$lam1")
      , Function
          (fromList [(tInt, "x"), (tInt, "y")])
          ( tInt
          , eOp2
              oAddInt
              (eVar (tInt, "x"))
              (eVar (tInt, "y"))
          )
      )
    ,
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLit (PInt 123)
          )
      )
    ]

fragment4 :: ModuleDefs MonoType TypedExpr
fragment4 =
  Map.fromList
    [
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLet
              (tInt ~> tInt, "f")
              ( eLam
                  ()
                  [(tInt, "y")]
                  ( eOp2
                      oAddInt
                      (eVar (tInt, "x"))
                      (eVar (tInt, "y"))
                  )
              )
              (eLit (PInt 123))
          )
      )
    ]

fragment5 :: ModuleDefs MonoType Ast
fragment5 =
  Map.fromList
    [
      ( (Scheme (tInt ~> tInt ~> tInt), "$lam1")
      , Function
          (fromList [(tInt, "x"), (tInt, "y")])
          ( tInt
          , eOp2
              oAddInt
              (eVar (tInt, "x"))
              (eVar (tInt, "y"))
          )
      )
    ,
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLet
              (tInt ~> tInt, "f")
              (eCall () (tInt ~> tInt ~> tInt, "$lam1") [eVar (tInt, "x")])
              (eLit (PInt 123))
          )
      )
    ]

fragment6 :: ModuleDefs MonoType TypedExpr
fragment6 =
  Map.fromList
    [
      ( (Scheme (tInt ~> tInt ~> tInt), "foo")
      , Function
          (fromList [(tInt, "a")])
          ( tInt ~> tInt
          , eIf
              (eVar (tBool, "x"))
              (eLam () [(tInt, "a")] (eLit (PInt 1)))
              (eLam () [(tInt, "a")] (eLit (PInt 2)))
          )
      )
    ]

fragment7 :: ModuleDefs MonoType Ast
fragment7 =
  Map.fromList
    [
      ( (Scheme (tInt ~> tInt ~> tInt), "foo")
      , Function
          (fromList [(tInt, "a")])
          ( tInt ~> tInt
          , eCall () (tBool ~> tInt ~> tInt, "$if3") [eVar (tBool, "x")]
          )
      )
    ,
      ( (Scheme (tBool ~> tInt ~> tInt), "$if3")
      , Function
          (fromList [(tBool, "x"), (tInt, "$v0")])
          ( tInt
          , eIf
              (eVar (tBool, "x"))
              (eCall () (tInt ~> tInt, "$lam1") [eVar (tInt, "$v0")])
              (eCall () (tInt ~> tInt, "$lam2") [eVar (tInt, "$v0")])
          )
      )
    ,
      ( (Scheme (tInt ~> tInt), "$lam1")
      , Function
          (fromList [(tInt, "a")])
          (tInt, eLit (PInt 1))
      )
    ,
      ( (Scheme (tInt ~> tInt), "$lam2")
      , Function
          (fromList [(tInt, "a")])
          (tInt, eLit (PInt 2))
      )
    ]

fragment8 :: ModuleDefs MonoType Ast
fragment8 =
  Map.fromList
    [
      ( (Scheme (tInt ~> tInt ~> tInt), "foo")
      , Function
          (fromList [(tInt, "a"), (tInt, "$v0")])
          ( tInt
          , eCall () (tBool ~> tInt ~> tInt, "$if3") [eVar (tBool, "x"), eVar (tInt, "$v0")]
          )
      )
    ,
      ( (Scheme (tBool ~> tInt ~> tInt), "$if3")
      , Function
          (fromList [(tBool, "x"), (tInt, "$v0")])
          ( tInt
          , eIf
              (eVar (tBool, "x"))
              (eCall () (tInt ~> tInt, "$lam1") [eVar (tInt, "$v0")])
              (eCall () (tInt ~> tInt, "$lam2") [eVar (tInt, "$v0")])
          )
      )
    ,
      ( (Scheme (tInt ~> tInt), "$lam1")
      , Function
          (fromList [(tInt, "a")])
          (tInt, eLit (PInt 1))
      )
    ,
      ( (Scheme (tInt ~> tInt), "$lam2")
      , Function
          (fromList [(tInt, "a")])
          (tInt, eLit (PInt 2))
      )
    ]
