{-# LANGUAGE OverloadedStrings #-}

-- import Pong.EvalTests
-- import Pong.LLVM.EmitTests
import Pong.LangTests
-- import Pong.ReadTests
-- import Pong.TreeTests
import Pong.TypeTests
-- import Pong.Util.EnvTests
-- import Pong.Util.PrettyTests
-- import Pong.UtilTests
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    --    evalTests
    --    llvmEmitTests
    langTests

    --    readTests
    --    treeTests
    typeTests

--    utilTests
--    utilEnvTests
--    utilPrettyTests
