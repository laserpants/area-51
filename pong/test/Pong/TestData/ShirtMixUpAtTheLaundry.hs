{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.ShirtMixUpAtTheLaundry where

import Pong.Lang
import Pong.Util

program2zz :: Text -- { b : bool, c : bool, d : int } =\
program2zz =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : { b : int } =\
  \      field\
  \        { a = x | s } =\
  \          { a = 5, b = 6 }\
  \        in\
  \          s\
  \"

-- "

prog2zzSig :: Label Scheme
prog2zzSig = (Scheme (tUnit ~> tRec (rExt "b" tInt rNil)), "main")

program2 :: Text
program2 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \    r =\
  \      { a = 5, b = true }\
  \    in\
  \      let\
  \        q =\
  \          { c = 1 | r }\
  \        in\
  \          field\
  \            { a = x | s } =\
  \              q\
  \            in\
  \              x\
  \"

-- "

program3 :: Text
program3 =
  "\
  \module Main\
  \\r\n\
  \func main(a : unit) : int =\
  \  let\
  \    r =\
  \      { a = 5, b = true }\
  \    in\
  \      let\
  \        q =\
  \          { c = 1 | r }\
  \        in\
  \          field\
  \            { c = x | s } =\
  \              q\
  \            in\
  \              x\
  \"

-- "
