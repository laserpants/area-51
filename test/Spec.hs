{-# LANGUAGE OverloadedStrings #-}

import Pong.EvalTests
import Pong.LangTests
import Pong.ReadTests
import Pong.TreeTests
import Pong.TypeTests
import Pong.UtilTests
import Pong.Util.EnvTests
import Pong.Util.PrettyTests
import Pong.LLVM.EmitTests
import Test.Hspec

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
