{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Teriyaki.Tree where

import Control.Monad.Extra (anyM, (||^))
import Control.Monad.Reader
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import qualified Data.Text as Text
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

exhaustive :: (Row t, Tuple t, MonadReader ConstructorEnv m) => PatternMatrix t -> m Bool
exhaustive []        = pure False
exhaustive px@(ps:_) = not <$> useful px (pAny . getTag <$> ps)

{- ORMOLU_ENABLE -}

useful :: (Row t, Tuple t, MonadReader ConstructorEnv m) => PatternMatrix t -> [Pattern t] -> m Bool
useful px ps = go (preprocessRecords <$$> px) (preprocessRecords <$> ps)
  where
    go [] _ = pure True -- Zero rows (0x0 matrix)
    go (p : _) qs
      | null p = pure False -- One or more rows but no columns
      | null qs = error "Implementation error"
    go qx (q : qs) =
      case patternGroups q of
        ConGroup con rs ->
          let special = specialized con (getTag <$> rs)
           in go (special qx) (head (special [q : qs]))
        WildcardPattern -> do
          let cs = headCons qx
          complete <- isComplete (fst <$> cs)
          if complete
            then
              cs
                & anyM
                  ( \(con, rs) -> do
                      let special = specialized con (getTag <$> rs)
                       in go (special qx) (head (special [q : qs]))
                  )
            else go (defaultMatrix qx) qs
        OrPattern p r ->
          go qx (p : qs) ||^ go qx (r : qs)
    go _ _ =
      error "Implementation error"

preprocessRecords :: (Row t, Tuple t) => Pattern t -> Pattern t
preprocessRecords =
  cata $
    \case
      PRec _ a ->
        foldr2
          ( \d e ->
              pTup (tup [getTag d, getTag e]) [d, e]
          )
          leaf
          m
        where
          (m, r) = unwindRow a
          leaf =
            case project r of
              PNil _ ->
                pLit (tup []) IUnit
              PCon _ "{}" [] ->
                pLit (tup []) IUnit
              _ -> r
      p ->
        embed p

isComplete :: (MonadReader ConstructorEnv m) => [Name] -> m Bool
isComplete [] = pure False
isComplete names@(name : _) = do
  defined <- ask
  pure (lookupCon (defined <> builtIn) name == Set.fromList names)

isTupleCon :: Name -> Bool
isTupleCon con = Just True == (Text.all (== ',') <$> stripped con)
  where
    stripped = Text.stripPrefix "(" <=< Text.stripSuffix ")"

lookupCon :: ConstructorEnv -> Name -> Set Name
lookupCon constructors con
  | isTupleCon con = Set.singleton con
  | otherwise = maybe mempty fst (Env.lookup con constructors)

{- ORMOLU_DISABLE -}

builtIn :: ConstructorEnv
builtIn =
  constructorEnv
    [ ("#True"   , (["#True", "#False"], 0))
    , ("#False"  , (["#True", "#False"], 0))
    , ("#()"     , (["#()"], 0))
    , ("#Int"    , ([], 1))
    , ("#Big"    , ([], 1))
    , ("#Float"  , ([], 1))
    , ("#Double" , ([], 1))
    , ("#Char"   , ([], 1))
    , ("#String" , ([], 1))
    , ("[]"      , (["[]", "(::)"], 0))
    , ("(::)"    , (["[]", "(::)"], 2))
    ]

specialized :: (Row t) => Name -> [t] -> PatternMatrix t -> PatternMatrix t
specialized name ts = (go =<<)
  where
    go [] = error "Implementation error"
    go (p : ps) =
      case project p of
        PCon    _ con rs
          | con == name         -> [rs <> ps]
          | otherwise           -> []
        PLit    t lit           -> go (pCon t (primCon lit) [] : ps)
        PList   t elms          -> go (foldList t elms : ps)
        PTup    t elms          -> go (foldTuple t elms : ps)
        POr     _ q r           -> go (q : ps) <> go (r : ps)
        PAs     _ _ q           -> go (q : ps)
        PAnn    _ q             -> go (q : ps)
        PRec    {}              -> error "Implementation error"
        PNil    {}              -> error "Implementation error"
        PExt    {}              -> error "Implementation error"
        _                       -> [(pAny <$> ts) <> ps]

defaultMatrix :: (Row t) => PatternMatrix t -> PatternMatrix t
defaultMatrix = (go =<<)
  where
    go :: (Row t) => [Pattern t] -> PatternMatrix t
    go [] = error "Implementation error"
    go (p : ps) =
      case project p of
        PCon    {}              -> []
        PTup    {}              -> []
        PList   {}              -> []
        PLit    {}              -> []
        PRec    {}              -> error "Implementation error"
        PNil    {}              -> error "Implementation error"
        PExt    {}              -> error "Implementation error"
        POr     _ q r           -> go (q : ps) <> go (r : ps)
        PAs     _ _ q           -> go (q : ps)
        PAnn    _ q             -> go (q : ps)
        _                       -> [ps]

patternGroups :: (Row t) => Pattern t -> PatternGroup t
patternGroups =
  project
    >>> \case
      PCon      _ con ps        -> ConGroup con ps
      PTup      t elms          -> patternGroups (foldTuple t elms)
      PList     t elms          -> patternGroups (foldList t elms)
      PLit      t lit           -> patternGroups (pCon t (primCon lit) [])
      PAnn      _ p             -> patternGroups p
      PAs       _ _ p           -> patternGroups p
      POr       _ p q           -> OrPattern p q
      PRec      {}              -> error "Implementation error"
      PNil      {}              -> error "Implementation error"
      PExt      {}              -> error "Implementation error"
      _                         -> WildcardPattern

headCons :: (Row t) => PatternMatrix t -> [(Name, [Pattern t])]
headCons = (>>= go)
  where
    go [] = error "Implementation error"
    go (p : ps) =
      case project p of
        PCon    _ name rs       -> [(name, rs)]
        PLit    _ q             -> [(primCon q, [])]
        PTup    t elms          -> go (foldTuple t elms : ps)
        PList   t elms          -> go (foldList t elms : ps)
        POr     _ q r           -> go (q : ps) <> go (r : ps)
        PAs     _ _ q           -> go (q : ps)
        PAnn    _ q             -> go (q : ps)
        PRec    {}              -> error "Implementation error"
        PNil    {}              -> error "Implementation error"
        PExt    {}              -> error "Implementation error"
        _                       -> []

primCon :: Prim -> Name
primCon (IBool True)    = "#True"
primCon (IBool False)   = "#False"
primCon IUnit           = "#()"
primCon IInt{}          = "#Int"
primCon IBig{}          = "#Big"
primCon INat{}          = "#Nat"
primCon IFloat{}        = "#Float"
primCon IDouble{}       = "#Double"
primCon IChar{}         = "#Char"
primCon IString{}       = "#String"

{- ORMOLU_ENABLE -}
