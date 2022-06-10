{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.MysteriousSetOfBooks where

import Data.Functor
import Pong.Data
import Pong.Lang
import Pong.TestHelpers
import Pong.Util

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

program21 :: Text
program21 =
  "def baz(a : unit) : { a : int, b : bool } =\
  \  { a = 5, b = true }\
  \\r\n\
  \def foo(a : unit) : int =\
  \  5\
  \\r\n\
  \def main(a : unit) : int =\
  \  let\
  \    r =\
  \      baz(())\
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
  \                  101\
  \                else\
  \                  200\
  \"

-- "

program22 :: Text
program22 =
  "const foo : int =\
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
  \                x == foo\
  \                then\
  \                  102\
  \                else\
  \                  200\
  \"

-- "

program23 :: Text
program23 =
  "const foo : int =\
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
  \              letr\
  \                { c = y | t } =\
  \                  s\
  \                in\
  \                  y\
  \"

-- "
