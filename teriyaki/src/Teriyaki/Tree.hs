{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Teriyaki.Tree where

import Control.Monad.Extra (anyM, (||^))
import Control.Monad.Reader
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Teriyaki.Data
import Teriyaki.Lang
import Teriyaki.Util
import Teriyaki.Util.Env (Environment)
import qualified Teriyaki.Util.Env as Env

type ConstructorEnv =
  Environment (Set Name, Int)

constructorEnv :: [(Name, ([Name], Int))] -> ConstructorEnv
constructorEnv = Env.fromList <<< (first Set.fromList <$$>)

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

exhaustive :: (MonadReader ConstructorEnv m) => [[Pattern t]] -> m Bool
exhaustive []        = pure False
exhaustive px@(ps:_) = not <$> useful px (pAny . getTag <$> ps)

{- ORMOLU_ENABLE -}

useful ::
  (MonadReader ConstructorEnv m) => PatternMatrix t -> [Pattern t] -> m Bool
useful px ps = stage3 (stage2 . stage1 <$$> px) (stage2 . stage1 <$> ps)
  where
    stage1 =
      id -- TODO
    stage2 =
      id -- TODO
    stage3 [] _ = pure True -- Zero rows (0x0 matrix)
    stage3 (p : _) qs
      | null p = pure False -- One or more rows but no columns
      | null qs = error "Implementation error"
    stage3 qx (q : qs) =
      case patternGroups q of
        ConGroup con rs ->
          let special = specialized con (getTag <$> rs)
           in stage3 (special qx) (head (special [q : qs]))
        WildcardPattern -> do
          let cs = headCons qx
          complete <- isComplete (fst <$> cs)
          if complete
            then
              cs
                & anyM
                  ( \(con, rs) -> do
                      let special = specialized con (getTag <$> rs)
                       in stage3 (special qx) (head (special [q : qs]))
                  )
            else stage3 (defaultMatrix qx) qs
        OrPattern p r ->
          stage3 qx (p : qs) ||^ stage3 qx (r : qs)
    stage3 _ _ = error "Implementation error"

isComplete :: (MonadReader ConstructorEnv m) => [Name] -> m Bool
isComplete [] = pure False
isComplete names@(name : _) = do
  defined <- ask
  pure (lookupCon (defined `Env.union` builtIn) name == Set.fromList names)

-- TODO: use (<>) ???

lookupCon :: ConstructorEnv -> Name -> Set Name
lookupCon constructors con =
  -- \| isTupleCon con || isRowCon con = Set.singleton con
  -- \| otherwise = maybe mempty fst (Env.lookup con constructors)
  maybe mempty fst (Env.lookup con constructors)

builtIn :: ConstructorEnv
builtIn =
  constructorEnv
    []

{- ORMOLU_DISABLE -}

specialized :: Name -> [t] -> PatternMatrix t -> PatternMatrix t
specialized name ts = (go =<<)
  where
    go [] = error "Implementation error"
    go (p : ps) =
      case project p of
        PCon    _ con rs
          | con == name         -> [rs <> ps]
          | otherwise           -> []
        --        PLit -> undefined
        PAs     _ _ q           -> go (q : ps)
        POr     _ q r           -> go (q : ps) <> go (r : ps)
        _                       -> [(pAny <$> ts) <> ps]

defaultMatrix :: PatternMatrix t -> PatternMatrix t
defaultMatrix = (go =<<)
  where
    go :: [Pattern t] -> PatternMatrix t
    go [] = error "Implementation error"
    go (p : ps) =
      case project p of
        PCon{}                  -> []
        PTup{}                  -> []
        PList{}                 -> []
        PNil{}                  -> []
        PExt{}                  -> []
        PLit{}                  -> []
        PAnn    _ q             -> go (q : ps)
        PAs     _ _ q           -> go (q : ps)
        POr     _ q r           -> go (q : ps) <> go (r : ps)
        _                       -> [ps]

patternGroups :: Pattern t -> PatternGroup t
patternGroups =
  project
    >>> \case
      PCon      _ con ps        -> ConGroup con ps
      -- TODO
      -- PTup
      -- PList
      -- PRecord
      -- PLit
      PAs       _ _ p           -> patternGroups p
      POr       _ p q           -> OrPattern p q
      _                         -> WildcardPattern

headCons :: PatternMatrix t -> [(Name, [Pattern t])]
headCons = (>>= go)
  where
    go :: [Pattern t] -> [(Name, [Pattern t])]
    go [] = error "Implementation error"
    go (p : ps) =
      case project p of
        -- PLit    _ r       -> [(prim r, [])]
        PCon    _ name rs       -> [(name, rs)]
        -- TODO
        -- PTup
        -- PList
        -- PRecord
        -- PLit
        PAs     _ _ q           -> go (q : ps)
        POr     _ q r           -> go (q : ps) <> go (r : ps)
        _                       -> []

{- ORMOLU_ENABLE -}
