module Pong.TestData.GeraniumPlant where

import Data.List.NonEmpty (fromList, toList)
import qualified Data.Map.Strict as Map
import Pong.Data
import Pong.Lang
import Pong.Util

fragment1 :: Program MonoType TypedExpr
fragment1 =
  Program
    ( Map.fromList
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
    )

fragment2 :: Program MonoType Ast
fragment2 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                  (tInt ~> tInt ~> tInt, "f")
                  undefined
                  (eLit (PInt 123))
              )
          )
        ]
    )

fragment3 :: Program MonoType TypedExpr
fragment3 =
  Program
    ( Map.fromList
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
    )

fragment4 :: Program MonoType TypedExpr
fragment4 =
  Program
    ( Map.fromList
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
    )
