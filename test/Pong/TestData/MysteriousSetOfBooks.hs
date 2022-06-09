{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.MysteriousSetOfBooks where

import Data.Functor
import Pong.Data
import Pong.Lang
import Pong.Util
import Pong.TestHelpers

program20 :: Text
program20 = 
  "def foo(a : unit) : int =\
  \  5\
  \\r\n\
  \def main(a : unit) : int =\
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
  \              if\
  \                x == foo(())\
  \                then\
  \                  100\
  \                else\
  \                  200\
  \"

-- "
