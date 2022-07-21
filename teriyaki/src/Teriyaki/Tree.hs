{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Teriyaki.Tree where

import Control.Arrow ((<<<), (>>>))
import Control.Monad.Reader
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Teriyaki.Data
import Teriyaki.Util
import Teriyaki.Util.Env (Environment)
import qualified Teriyaki.Util.Env as Env

type ConstructorEnv =
  Environment (Set Name, Int)

constructorEnv :: [(Name, ([Name], Int))] -> ConstructorEnv
constructorEnv = Env.fromList <<< (first Set.fromList <$$>)

-------------------------------------------------------------------------------

data PatternGroup
  = ConGroup Name [Pattern ()]
  | OrPattern (Pattern ()) (Pattern ())
  | WildcardPattern

type PatternMatrix = [[Pattern ()]]

useful ::
  (MonadReader ConstructorEnv m) =>
  PatternMatrix ->
  [Pattern ()] ->
  m Bool
useful px ps =
  undefined
  where
    step1 =
      undefined

    step2 =
      undefined

    step3 [] _ = pure True -- Zero rows (0x0 matrix)
    step3 (p : _) qs
      | null p = pure False -- One or more rows but no columns
      | null qs = error "Implementation error"
    step3 qx (q : qs) =
      case patternGroups q of
        ConGroup con rs ->
          undefined
        --                let special = specialized con (getTag <$> rs)
        --                 in step3 (special qx) (head (special [q:qs]))
        WildcardPattern ->
          undefined
        OrPattern a b ->
          undefined

isComplete :: (MonadReader ConstructorEnv m) => [Name] -> m Bool
isComplete = undefined

lookupCon :: ConstructorEnv -> Name -> Set Name
lookupCon constructors con =
  undefined

--  | isTupleCon con || isRowCon con = Set.singleton con
--  | otherwise = maybe mempty fst (Env.lookup con constructors)

builtIn :: ConstructorEnv
builtIn =
  constructorEnv
    []

-- specialized :: Name -> [TInfo] -> PatternMatrix u -> PatternMatrix u
specialized = undefined

patternGroups :: Pattern () -> PatternGroup
patternGroups =
  project
    >>> \case
      _ ->
        undefined
