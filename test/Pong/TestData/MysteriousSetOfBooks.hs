{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.MysteriousSetOfBooks where

import Pong.Util

program20 :: Text
program20 =
  "def foo(a : unit) : int =\
  \  5\
  \\r\n\
  \def main(_ : unit) : int =\
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

program24 :: Text
program24 =
  "def main(_ : unit) : int =\
  \  let\
  \    r =\
  \      { a = 5 }\
  \    in\
  \      letr\
  \        { a = x | s } =\
  \          r\
  \        in\
  \          x\
  \"

-- "

program25 :: Text
program25 =
  "def foo(a : unit) : int =\
  \  5\
  \\r\n\
  \def main(_ : unit) : int =\
  \  let\
  \    r =\
  \      { a = false }\
  \    in\
  \      letr\
  \        { a = x | s } =\
  \          r\
  \        in\
  \          if x\
  \            then 9\
  \            else 10\
  \"

-- "

program26 :: Text
program26 =
  "def main(_ : unit) : int =\
  \  let\
  \    r =\
  \      { a = 3.14159 }\
  \    in\
  \      letr\
  \        { a = x | s } =\
  \          r\
  \        in\
  \          if x > 3.0\
  \            then 99\
  \            else 109\
  \"

-- "

program27 :: Text
program27 =
  "def main(_ : unit) : int =\
  \  let\
  \    b =\
  \      5\
  \    in\
  \      let\
  \        a =\
  \          true\
  \        in\
  \          if not a\
  \            then 1\
  \            else 2\
  \"

-- "

program28 :: Text
program28 =
  "def main(_ : unit) : int =\
  \  let\
  \    b =\
  \      5\
  \    in\
  \      let\
  \        a =\
  \          -b\
  \        in\
  \          if -5 == a\
  \            then 1\
  \            else 2\
  \"

-- "
