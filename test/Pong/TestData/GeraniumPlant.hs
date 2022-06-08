module Pong.TestData.GeraniumPlant where

import Pong.Data
import Pong.Lang
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (fromList, toList)
import Pong.Util

program10 :: Program MonoType TypedExpr
program10 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                          (tInt ~> tInt ~> tInt, "$var_add_1")
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


