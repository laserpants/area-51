{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.GoAwayDixieGillian where

import Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
import Pong.Data
import Pong.Lang

program1 :: Program MonoType TypedExpr
program1 =
  Program
    ( Map.fromList
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
                  ( eRec
                      ( rExt
                          "a"
                          (eLit (PInt 100))
                          ( rExt
                              "b"
                              (eLit (PBool True))
                              ( rExt
                                  "c"
                                  (eLit (PInt 3))
                                  rNil
                              )
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
    )
