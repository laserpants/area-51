{-# LANGUAGE OverloadedStrings #-}
module Pong.TestData.ShirtMixUpAtTheLaundry where

import Pong.Data
import Pong.Lang
import Pong.Util

program2 :: Text
program2 =
  "def main(a : unit) : int =\
  \  let\
  \    r =\
  \      { a = 5, b = true }\
  \    in\
  \      let\
  \        q =\
  \          { c = 1 | r }\
  \        in\
  \          letr\
  \            { a = x | s } =\
  \              q\
  \            in\
  \              x\
  \"

-- "

program3 :: Text
program3 =
  "def main(a : unit) : int =\
  \  let\
  \    r =\
  \      { a = 5, b = true }\
  \    in\
  \      let\
  \        q =\
  \          { c = 1 | r }\
  \        in\
  \          letr\
  \            { c = x | s } =\
  \              q\
  \            in\
  \              x\
  \"

-- "
