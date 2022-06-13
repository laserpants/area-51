{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.ThePanamaHat where

import Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
import Pong.Data
import Pong.Lang
import Pong.Util

program200 :: Text
program200 =
  "def main(a : unit) : int =\
  \  let\
  \    xs =\
  \      Nil()\
  \    in\
  \      200\
  \"

-- "

program201 :: Program () SourceExpr
program201 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [((), "a")])
              ( ()
              , eLet
                  ((), "xs")
                  (eCon ((), "Nil"))
                  (eLit (PInt 200))
              )
          )
        ]
    )
