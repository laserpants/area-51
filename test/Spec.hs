{-# LANGUAGE OverloadedStrings #-}

import Pong.EvalTests
import Pong.LangTests
import Pong.ReadTests
import Pong.TreeTests
import Pong.TypeTests
import Pong.UtilTests
import Test.Hspec

llvmEmitTests :: SpecWith ()
llvmEmitTests =
  describe "Pong.LLVM.Emit" $ do
    pure ()

utilEnvTests :: SpecWith ()
utilEnvTests =
  describe "Pong.Util.Env" $ do
    pure ()

utilPrettyTests :: SpecWith ()
utilPrettyTests =
  describe "Pong.Util.Pretty" $ do
    pure ()

main :: IO ()
main =
  hspec $ do
    evalTests
    llvmEmitTests
    langTests
    readTests
    treeTests
    typeTests
    utilTests
    utilEnvTests
    utilPrettyTests
