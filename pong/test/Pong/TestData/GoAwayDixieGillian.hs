{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.GoAwayDixieGillian where

import Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
import Pong.Lang

program1 :: ModuleDefs MonoType TypedExpr
program1 =
  Map.fromList
    [
      ( (Scheme (tUnit ~> tInt), "main")
      , Function
          (fromList [(tUnit, "a")])
          ( tInt
          , eLet
              ( tRec
                  ( rExt
                      "a"
                      tInt
                      ( rExt
                          "b"
                          tBool
                          ( rExt
                              "c"
                              tInt
                              rNil
                          )
                      )
                  )
              , "r"
              )
              ( eExt
                  "a"
                  (eLit (PInt 100))
                  ( eExt
                      "b"
                      (eLit (PBool True))
                      ( eExt
                          "c"
                          (eLit (PInt 3))
                          eNil
                      )
                  )
              )
              ( eRes
                  [
                    ( tInt
                        ~> tRec (rExt "b" tBool (rExt "c" tInt rNil))
                        ~> tRec
                          ( rExt "a" tInt (rExt "b" tBool (rExt "c" tInt rNil))
                          )
                    , "a"
                    )
                  , (tInt, "x")
                  ,
                    ( tRec
                        ( rExt
                            "b"
                            tBool
                            ( rExt
                                "c"
                                tInt
                                rNil
                            )
                        )
                    , "q"
                    )
                  ]
                  ( eVar
                      ( tRec
                          ( rExt
                              "a"
                              tInt
                              ( rExt
                                  "b"
                                  tBool
                                  ( rExt
                                      "c"
                                      tInt
                                      rNil
                                  )
                              )
                          )
                      , "r"
                      )
                  )
                  ( eVar (tInt, "x")
                  )
              )
          )
      )
    ]
