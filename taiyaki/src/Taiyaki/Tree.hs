{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Taiyaki.Tree where

import Control.Monad.Extra (anyM, (||^))
import Control.Monad.Reader
import Data.Foldable (foldrM)
import Data.Functor.Foldable (ListF (..))
import qualified Data.Set.Monad as Set
import qualified Data.Text as Text
import Taiyaki.Data
import Taiyaki.Data.Cons
import Taiyaki.Lang
import Taiyaki.Util
import qualified Taiyaki.Util.Env as Env

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

exhaustive ::
  (Row t, Tuple t (), MonadReader ConstructorEnv m) =>
  PatternMatrix t ->
  m Bool
exhaustive []          = pure False
exhaustive px@(ps : _) = not <$> useful px (pAny . getTag <$> ps)

{- ORMOLU_ENABLE -}

useful ::
  (Row t, Tuple t (), MonadReader ConstructorEnv m) =>
  PatternMatrix t ->
  [Pattern t] ->
  m Bool
useful px ps = go (preprocessRecords <$$> px) (preprocessRecords <$> ps)
  where
    go [] _ = pure True -- Zero rows (0x0 matrix)
    go (p : _) qs
      | null p = pure False -- One or more rows but no columns
      | null qs = error "Implementation error"
    go qx (q : qs) =
      case patternGroups q of
        ConGroup ctor rs ->
          let special = specialized ctor (getTag <$> rs)
           in go (special qx) (head (special [q : qs]))
        WildcardPattern -> do
          let cs = headCons qx
          complete <- isComplete (fst <$> cs)
          if complete
            then
              cs
                & anyM
                  ( \(ctor, rs) -> do
                      let special = specialized ctor (getTag <$> rs)
                       in go (special qx) (head (special [q : qs]))
                  )
            else go (defaultMatrix qx) qs
        OrPattern p r ->
          go qx (p : qs) ||^ go qx (r : qs)
    go _ _ =
      error "Implementation error"

-- Translate records to tuples for better performance
preprocessRecords :: (Row t, Tuple t ()) => Pattern t -> Pattern t
preprocessRecords =
  cata
    ( \case
        PRec _ a ->
          foldr2
            ( \d e ->
                pTup (tup () [getTag d, getTag e]) [d, e]
            )
            leaf
            m
          where
            (m, r) = unpackRow a
            leaf =
              case project r of
                PNil _ ->
                  pLit (tup () []) IUnit
                PCon _ "{}" [] ->
                  pLit (tup () []) IUnit
                _ -> r
        p ->
          embed p
    )

isComplete :: (MonadReader ConstructorEnv m) => [Name] -> m Bool
isComplete [] = pure False
isComplete names@(name : _) = do
  defined <- ask
  pure (lookupCon (defined <> builtIn) name == Set.fromList names)

isTupleCon :: Name -> Bool
isTupleCon ctor = Just True == (Text.all (== ',') <$> stripped ctor)
  where
    stripped = Text.stripPrefix "(" <=< Text.stripSuffix ")"

lookupCon :: ConstructorEnv -> Name -> Set Name
lookupCon constructors ctor
  | isTupleCon ctor = Set.singleton ctor
  | otherwise = maybe mempty fst (Env.lookup ctor constructors)

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
        PCon    _ ctor rs
          | ctor == name        -> [rs <> ps]
          | otherwise           -> []
        PLit    t lit           -> go (pCon t (primCon lit) [] : ps)
        PList   t elms          -> go (rawList t elms : ps)
        PTup    t elms          -> go (rawTuple t elms : ps)
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
      PCon      _ ctor ps       -> ConGroup ctor ps
      PTup      t elms          -> patternGroups (rawTuple t elms)
      PList     t elms          -> patternGroups (rawList t elms)
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
    go :: (Row t) => [Pattern t] -> [(Name, [Pattern t])]
    go [] = error "Implementation error"
    go (p : ps) =
      case project p of
        PCon    _ name rs       -> [(name, rs)]
        PLit    _ q             -> [(primCon q, [])]
        PTup    t elms          -> go (rawTuple t elms : ps)
        PList   t elms          -> go (rawList t elms : ps)
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

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

-- Unpack tuples, lists, records, rows, and codata expressions
--
stage1 ::
  (Eq v, Eq e1, Eq p, Eq e4) =>
  Expr (Type v) e1 (Clause (Type v) p) (Clause (Type v) p) e4 ->
  Expr (Type v) e1 (Clause (Type v) p) (Clause (Type v) p) e4
stage1 =
  cata
    ( \case
        ETup  t es -> rawTuple t es
        EList t es -> rawList t es
        ERec  t r  -> con (tRec (normalizeRow u)) "#{*}" [rawRow r]
          where
            TRec u = project t
        --      ECo   t e -> undefined
        e -> embed e
    )

{- ORMOLU_ENABLE -}

--      ENil  t ->
--        undefined
--      EExt  t n e r ->
--        undefined

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

stage2 :: (Monad m) => Expr (Type v) e1 e2 e3 e4 -> m (Expr (Type v) e1 e2 e3 e4)
stage2 =
  undefined

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

data Labeled a
  = LCon a
  | LVar a

compilePatterns ::
  (Monad m, Functor e3) =>
  Expr (Type v) Name (Clause (Type v) p) e3 e4 ->
  [Clause (Type v) [Pattern (Type v)] (Expr (Type v) Name (Clause (Type v) p) e3 e4)] ->
  m (Expr (Type v) Name (Clause (Type v) p) e3 e4)
compilePatterns ex cs =
  compileMatch [ex] cs (eVar (tCon kTyp "<FAIL>") "<FAIL>")
  where
    compileMatch [] [] c =
      pure c
    compileMatch [] (Clause _ [] [Choice [] e] : _) _ =
      pure e
    compileMatch [] (Clause _ [] [Choice exs e] : qs) c =
      eIf (getTag e) (foldr1 undefined exs) e <$> compileMatch [] qs c
    compileMatch (u : us) qs c =
      case clauseGroups qs of
        [LVar eqs] ->
          compileMatch us (runSubst <$> eqs) c
          where
            runSubst (Clause t (p : ps) g) =
              let clause = Clause t ps g
               in case project p of
                    PVar t1 name ->
                      substitute name u <$> clause
                    PAs _ as (Fix (PVar t1 name)) ->
                      substitute name u . substitute as (eVar t1 name) <$> clause
                    PAs _ as (Fix (PAny t)) ->
                      substitute as u <$> clause
                    -- The remaining case is for wildcards and literal patterns
                    _ -> clause
            runSubst _ =
              error "Implementation error"
        --
        [LCon eqs@(Clause t _ [Choice _ e] : _)] -> do
          undefined
        --
        mixed ->
          foldrM (compileMatch (u : us)) c (clauses <$> mixed)
    --
    compileMatch _ _ _ =
      error "Implementation error"

    clauses :: Labeled a -> a
    clauses (LCon eqs) = eqs
    clauses (LVar eqs) = eqs

data CasePattern t = Case t Name [Name]

substitute ::
  (Functor e3) =>
  Name ->
  Expr t Name (Clause t p) e3 e4 ->
  Expr t Name (Clause t p) e3 e4 ->
  Expr t Name (Clause t p) e3 e4
substitute name subst =
  para
    ( \case
        ELam t pat e1 -> eLam t pat e1'
          where
            e1'
              | name == pat = fst e1
              | otherwise = snd e1
        EPat t ex eqs ->
          ePat t (snd ex) (substEq <$> eqs)
          where
            substEq eq@(Clause _ ps _)
              -- \| name `elem` (pats =<< ps) = undefined -- fst <$> eq
              | otherwise = snd <$> eq
            pats (PCon _ _ ps) = ps
        expr ->
          snd <$> expr & \case
            EVar t var
              | name == var -> subst
              | otherwise -> eVar t var
            e -> embed e
    )

{- ORMOLU_DISABLE -}

clauseGroups ::
  [Clause t [Pattern t] (Expr t e1 e2 e3 e4)] ->
  [Labeled [Clause t [Pattern t] (Expr t e1 e2 e3 e4)]]
clauseGroups = cata alg . (labeledClause <$>)
  where
    alg Nil                            = []
    alg (Cons (LCon e) (LCon es : ts)) = LCon (e : es) : ts
    alg (Cons (LVar e) (LVar es : ts)) = LVar (e : es) : ts
    alg (Cons (LCon e) ts)             = LCon [e] : ts
    alg (Cons (LVar e) ts)             = LVar [e] : ts

labeledClause ::
  Clause t [Pattern t] (Expr t e1 e2 e3 e4) ->
  Labeled (Clause t [Pattern t] (Expr t e1 e2 e3 e4))
labeledClause eq@(Clause _ (p : _) _) = p
    & cata
      ( \case
          PCon{}    -> LCon eq
          PVar{}    -> LVar eq
          PAs _ _ q -> q
      )

{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}

deriving instance Show a =>
  Show (Labeled a)

deriving instance Eq a =>
  Eq (Labeled a)

deriving instance Ord a =>
  Ord (Labeled a)
