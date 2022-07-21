module Main where

import Control.Monad.Reader
import Teriyaki.Data
import Teriyaki.Tree

test1 :: [[Pattern ()]]
test1 =
  [ []
  ]

runtest1 :: Bool
runtest1 =
  runReader (exhaustive test1) mempty

main :: IO ()
main = print ("X" :: String) -- print kTyp
