{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.ShirtMixUpAtTheLaundry where

import Pong.Util

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
