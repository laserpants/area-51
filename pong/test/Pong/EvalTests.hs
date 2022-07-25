{-# LANGUAGE OverloadedStrings #-}

module Pong.EvalTests where

-- import Data.Either.Extra (fromRight)
import Pong.Data
import Pong.Eval
-- import Pong.TestData.GoAwayDixieGillian
import Pong.TestData.JackOfClubs
import Pong.TestData.MysteriousSetOfBooks
-- import Pong.TestData.MysteriousSetOfBooks
-- import Pong.TestData.ShirtMixUpAtTheLaundry
-- import Pong.TestData.TheFatalAuction
-- import Pong.TestData.ThePanamaHat
import Pong.TestHelpers
import Pong.Tree
-- import qualified Pong.Util.Env as Env
import Test.Hspec

exitSuccessValue :: Value
exitSuccessValue = PrimValue (PInt 0)

evalTests :: SpecWith ()
evalTests =
  describe "Pong.Eval" $ do
    describe "- evalModule" $ do
      -------------------------------------------------------------------------
      do
        v <- runIO $ evalModule (compileSource programoo4) mainSig
        it "1" (Just (PrimValue (PInt 1307674368000)) == (fst <$> v))
      -------------------------------------------------------------------------
      do
        v <- runIO $ evalModule (compileSource program55z) mainSig
        it "2" (Just (PrimValue (PInt 300)) == (fst <$> v))
      -------------------------------------------------------------------------
      do
        v <- runIO $ evalModule (compileSource program55zz) mainSig
        it "3" (Just (PrimValue (PInt 300)) == (fst <$> v))
      -------------------------------------------------------------------------
      do
        v <- runIO $ evalModule (compileSource program55zzz) mainSig
        it "4" (Just (PrimValue (PInt 551)) == (fst <$> v))
      -------------------------------------------------------------------------
      do
        v <- runIO $ evalModule (compileSource program55zzx) mainSig
        it "5" (Just (exitSuccessValue, "551") == v)
      -------------------------------------------------------------------------
      do
        v <- runIO $ evalModule (compileSource program55zx) mainSig
        it "5" (Just (exitSuccessValue, "300") == v)

----     -------------------------------------------------------------------------
----     do
----       v <- runIO $ evalModule (transformModule program1) mainSig
----       it "1" (Just (PrimValue (PInt 100)) == v)
----      -------------------------------------------------------------------------
----      let program :: Module MonoType Ast
----          program =
----            transformModule (fromRight emptyModule (parseAndAnnotate program2))
----       in do
----            v <- runIO $ evalModule program mainSig
----            it "2" (Just (PrimValue (PInt 5)) == v)
----      -------------------------------------------------------------------------
----      let program :: Module MonoType Ast
----          program =
----            transformModule (fromRight emptyModule (parseAndAnnotate program3))
----       in do
----            v <- runIO $ evalModule program mainSig
----            it "3" (Just (PrimValue (PInt 1)) == v)
----      -------------------------------------------------------------------------
----      let program :: Module MonoType Ast
----          program = compileSource program4
----       in do
----            v <- runIO $ evalModule program mainSig
----            it "4" (Just (PrimValue (PInt 14)) == v)
----      -------------------------------------------------------------------------
----      let program :: Module MonoType Ast
----          program = compileSource program20
----       in do
----            v <- runIO $ evalModule program mainSig
----            it "5" (Just (PrimValue (PInt 100)) == v)
----      -------------------------------------------------------------------------
----      let program :: Module MonoType Ast
----          program = compileSource program21
----       in do
----            v <- runIO $ evalModule program mainSig
----            it "6" (Just (PrimValue (PInt 101)) == v)
----      -------------------------------------------------------------------------
----      let program :: Module MonoType Ast
----          program = compileSource program22
----       in do
----            v <- runIO $ evalModule program mainSig
----            it "7" (Just (PrimValue (PInt 102)) == v)
----      -------------------------------------------------------------------------
----      let program :: Module MonoType Ast
----          program = compileSource program23
----       in do
----            v <- runIO $ evalModule program mainSig
----            it "8" (Just (PrimValue (PInt 1)) == v)
----      -------------------------------------------------------------------------
----      let program :: Module MonoType Ast
----          program = compileSource program44
----       in do
----            v <- runIO $ evalModule program mainSig
----            it "9" (Just (PrimValue (PInt 4)) == v)
----      -------------------------------------------------------------------------
----      let program :: Module MonoType Ast
----          program = compileSource program46
----       in do
----            v <- runIO $ evalModule program mainSig
----            it "10" (Just (PrimValue (PInt 5)) == v)
----      -------------------------------------------------------------------------
----      let program :: Module MonoType Ast
----          program = compileSource program48
----       in do
----            v <- runIO $ evalModule program mainSig
----            it "11" (Just (PrimValue (PInt 5)) == v)
----      -------------------------------------------------------------------------
----      do
----        v <- runIO $ evalModule program217 mainSig
----        it "12" (Just (PrimValue (PInt 401)) == v)
----      -------------------------------------------------------------------------
----      do
----        v <- runIO $ evalModule program271 mainSig
----        it "13" (Just (PrimValue (PInt 5)) == v)
----      -------------------------------------------------------------------------
----      let program :: Module MonoType Ast
----          program = compileSourceWithEnv env program212
----          env =
----            Env.fromList
----              [
----                ( "Nil"
----                , Right (Scheme (tCon "List" [tVar "a"]))
----                )
----              ,
----                ( "Cons"
----                , Right (Scheme (tVar "a" ~> tCon "List" [tVar "a"] ~> tCon "List" [tVar "a"]))
----                )
----              ]
----       in do
----            v <- runIO $ evalModule program mainSig
----            it "14" (Just (PrimValue (PInt 4)) == v)
----      -------------------------------------------------------------------------
----      do
----        v <- runIO $ evalModule program217 mainSig
----        it "15" (Just (PrimValue (PInt 401)) == v)
----      -------------------------------------------------------------------------
----      do
----        v <- runIO $ runEval mempty (eval expr305)
----        it "16" (PrimValue (PInt 401) == v)
----      -------------------------------------------------------------------------
----      do
----        v <- runIO $ runEval mempty (eval expr306)
----        it "17" (PrimValue (PInt 402) == v)
----      -------------------------------------------------------------------------
----      do
----        v <- runIO $ evalModule (compileModule program305) mainSig
----        it "18" (Just (PrimValue (PInt 402)) == v)
----      -------------------------------------------------------------------------
----      do
----        v <- runIO $ evalModule (compileSource program27) mainSig
----        it "19" (Just (PrimValue (PInt 2)) == v)
