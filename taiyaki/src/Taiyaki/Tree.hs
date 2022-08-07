{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Taiyaki.Tree where

import Control.Monad.Extra (anyM, (||^))
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (foldrM)
import Data.Functor.Classes (Eq1)
import Data.Functor.Foldable (ListF (..))
import Data.List.Extra (groupSortOn)
import qualified Data.Set.Monad as Set
import qualified Data.Text as Text
import Taiyaki.Data
import Taiyaki.Data.Cons
import Taiyaki.Lang
import Taiyaki.Util
import qualified Taiyaki.Util.Env as Env

{- ORMOLU_DISABLE -}

class TypeTag t where
  tcon :: Name -> t
  bool :: t
  tarr :: t -> t -> t
  trec :: t -> t
  ttup :: [t] -> t
  rmap :: (t -> t) -> t -> t

instance TypeTag (Type v) where
  tcon = tCon kTyp
  bool = tBool
  tarr = tArr
  trec = tRec
  ttup = tup ()
  rmap f =
    project
      >>> \case
        TRec t -> tRec (f t)

instance TypeTag () where
  tcon _   = ()
  bool     = ()
  tarr _ _ = ()
  trec _   = ()
  ttup _   = ()
  rmap _ _ = ()

{- ORMOLU_ENABLE -}

infixr 1 `tarr`

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
        PCon _ ctor rs
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
defaultMatrix = (>>= go)
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

-- =============
-- == Stage 1 ==
-- =============

-- Unpack tuples, lists, records, rows, and codata expressions
--
class Desugars e where
  desugar :: e -> e

instance Desugars e => Desugars [e] where
  desugar = fmap desugar

instance Desugars e => Desugars (Choice e) where
  desugar =
    \case
      Choice es e ->
        Choice (desugar es) (desugar e)

instance
  ( TypeTag t
  , Row t
  , Eq t
  , Desugars (Choice e)
  ) =>
  Desugars (Clause t [Pattern t] e)
  where
  desugar =
    \case
      Clause t ps cs ->
        Clause t (desugar ps) (desugar cs)

{- ORMOLU_DISABLE -}

instance (TypeTag t, Row t, Eq t) => Desugars (Binding t) where
  desugar =
    \case
      BPat t p    -> BPat t (desugar p)
      BFun t n ps -> BFun t n (desugar ps)

instance Desugars () where
  desugar _ = ()

instance (TypeTag t, Row t, Eq t) => Desugars (Pattern t) where
  desugar =
    cata
      ( \case
          PTup  t ps -> rawTuple t ps
          PList t ps -> rawList t ps
          PRec  t r  -> con (rmap normalizeRow t) "#Record" [rawRow r]
          p          -> embed p
      )

instance
  ( TypeTag t
  , Con (Expr t e1 e2 e3 e4) t
  , Functor e2
  , Functor e3
  , Row t
  , Eq t
  , Eq e1
  , Eq1 e2
  , Eq1 e3
  , Eq e4
  , Desugars e1
  , Desugars (e2 (Expr t e1 e2 e3 e4))
  , Desugars (e3 (Expr t e1 e2 e3 e4))
  , Desugars e4
  ) =>
  Desugars (Expr t e1 e2 e3 e4)
  where
  desugar =
    cata
      ( \case
          ETup  t es       -> rawTuple t es
          EList t es       -> rawList t es
          ERec  t r        -> con (rmap normalizeRow t) "#Record" [rawRow r]
          EPat  t a1 a2    -> ePat t a1 (desugar a2)
          ELet  t a1 a2 a3 -> eLet t (desugar a1) a2 a3
          EFun  t a1       -> eFun t (desugar a1)
          ELam  t a1 a2    -> eLam t (desugar a1) a2
          e -> embed e
      )

{- ORMOLU_ENABLE -}

translateFun ::
  (TypeTag t) =>
  [Clause t [Pattern t] (Expr t [Pattern t] (Clause t [Pattern t]) Void1 e4)] ->
  Expr t [Pattern t] (Clause t [Pattern t]) Void1 e4
translateFun cs@(Clause _ ps (Choice _ e : _) : _) =
  eLam
    (foldr tarr t ts)
    [pVar (getTag p) v | (p, v) <- vars]
    (ePat t expr (clause <$> cs))
  where
    t = getTag e
    ts = getTag <$> ps
    vars =
      ps `zip` ["$v" <> showt n | n <- [1 .. length ps]]
    expr =
      case [eVar (getTag p) v | (p, v) <- vars] of
        [e] -> e
        es -> tup (ttup ts) es
    clause (Clause t qs cs) = Clause t rs cs
      where
        rs
          | length qs > 1 = [pTup (ttup ts) qs]
          | otherwise = qs

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- =============
-- == Stage 2 ==
-- =============

{- ORMOLU_DISABLE -}

stage2 ::
  (TypeTag t, Monad m) =>
  Expr t [Pattern t] (Clause t [Pattern t]) Void1 (Binding t) ->
  m (Expr t Name (Clause t [Pattern t]) Void1 Void)
stage2 =
  cata
    ( \case
        ELet t bind expr1 expr2 ->
          translateLet t bind <$> expr1 <*> expr2
        ELam t a1 expr ->
          translateLam t a1 <$> expr
        EPat  t a1 a2    -> ePat t <$> a1 <*> traverse sequence a2
        EVar  t a1       -> pure (eVar t a1)
        ECon  t a1       -> pure (eCon t a1)
        ELit  t a1       -> pure (eLit t a1)
        EApp  t a1 a2    -> eApp t <$> a1 <*> sequence a2
        EIf   t a1 a2 a3 -> eIf t <$> a1 <*> a2 <*> a3
        EFix  t a1 a2 a3 -> eFix t a1 <$> a2 <*> a3
        EOp1  t a1 a2    -> eOp1 t a1 <$> a2
        EOp2  t a1 a2 a3 -> eOp2 t a1 <$> a2 <*> a3
        ETup  t a1       -> eTup t <$> sequence a1
        EList t a1       -> eList t <$> sequence a1
        ERec  t a1       -> eRec t <$> a1
        ENil  t          -> pure (eNil t)
        EExt  t a1 a2 a3 -> eExt t a1 <$> a2 <*> a3
        ESub  t          -> pure (eSub t)
        ECo   t a1       -> eCo t <$> a1
        EAnn  t a1       -> eAnn t <$> a1
    )

{- ORMOLU_ENABLE -}

translateLet ::
  (TypeTag t, Functor e3) =>
  t ->
  Binding t ->
  Expr t Name (Clause t [Pattern t]) e3 Void ->
  Expr t Name (Clause t [Pattern t]) e3 Void ->
  Expr t Name (Clause t [Pattern t]) e3 Void
translateLet t bind e1 e2 =
  case bind of
    BPat _ (Fix (PVar _ var)) ->
      eFix t var e1 e2
    BPat _ pat ->
      ePat
        (getTag e2)
        e1
        [ Clause (getTag e2) [pat] [Choice [] e2]
        ]
    BFun t1 f ps ->
      eFix t f (translateLam t1 ps e1) e2

translateLam ::
  (TypeTag t, Functor e3) =>
  t ->
  [Pattern t] ->
  Expr t Name (Clause t [Pattern t]) e3 e4 ->
  Expr t Name (Clause t [Pattern t]) e3 e4
translateLam t [] e = e
translateLam t (p : ps) expr = do
  case project p of
    PVar _ var ->
      eLam t var e
    _ ->
      eLam
        t
        "$p"
        ( ePat
            (getTag e)
            (eVar (getTag p) "$p")
            [Clause (getTag e) [p] [Choice [] e]]
        )
  where
    e = translateLam (foldr tarr (getTag expr) (getTag <$> ps)) ps expr

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

expandClause :: Clause t p a -> [Clause t p a]
expandClause (Clause t ps cs) = [Clause t ps [c] | c <- cs]

andExprs ::
  (TypeTag t, Functor e3) =>
  Expr t Name (CaseClause t) e3 e4 ->
  Expr t Name (CaseClause t) e3 e4 ->
  Expr t Name (CaseClause t) e3 e4
andExprs = eOp2 bool (OAnd (bool `tarr` bool `tarr` bool))

compilePatterns ::
  (TypeTag t, MonadState Int m, Functor e3) =>
  Expr t Name (CaseClause t) e3 e4 ->
  [Clause t [Pattern t] (Expr t Name (CaseClause t) e3 e4)] ->
  m (Expr t Name (CaseClause t) e3 e4)
compilePatterns ex cs =
  compileMatch [ex] (expandClause =<< cs) (eVar (tcon "<FAIL>") "<FAIL>")
  where
    compileMatch [] [] c =
      pure c
    compileMatch [] (Clause _ [] [Choice [] e] : _) _ =
      pure e
    compileMatch [] (Clause _ [] [Choice exs e] : qs) c =
      eIf (getTag e) (foldr1 andExprs exs) e <$> compileMatch [] qs c
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
          qs' <- traverse (toSimpleMatch t u us c) (consGroups u eqs)
          let rs = [Case (getTag u) "$_" [] c | not (isError c)]
          pure
            ( case qs' <> rs of
                [] -> c
                qs'' -> ePat (getTag e) u qs''
            )
          where
            isError =
              cata
                ( \case
                    EVar _ "<FAIL>" ->
                      True
                    _ ->
                      False
                )
        --
        mixed ->
          foldrM (compileMatch (u : us)) c (clauses <$> mixed)
    --
    compileMatch _ _ _ =
      error "Implementation error"

    clauses :: Labeled a -> a
    clauses (LCon eqs) = eqs
    clauses (LVar eqs) = eqs

    toSimpleMatch t u us c ConsGroup{..} = do
      (_, vars, pats) <- patternInfo (const id) consPatterns
      expr <- compileMatch (vars <> us) consClauses c
      pure
        ( Case
            (foldr tarr (getTag u) (getTag <$> consPatterns))
            consName
            pats
            expr
        )

uniqueName :: (MonadState Int m) => Name -> m Name
uniqueName prefix = do
  n <- getAndModify succ
  pure (prefix <> showt n)

patternInfo ::
  (MonadState Int m, Functor e2, Functor e3) =>
  (t -> Name -> a) ->
  [Pattern t] ->
  m ([(Name, t)], [Expr t e1 e2 e3 e4], [a])
patternInfo con pats = do
  vars <- replicateM (length pats) (uniqueName "$p")
  let ps = zip (getTag <$> pats) vars
  pure
    ( swap <$> ps
    , (fmap . uncurry) eVar ps
    , (fmap . uncurry) con ps
    )

{- ORMOLU_DISABLE -}

consGroups ::
  (Functor e3) =>
  Expr t Name (CaseClause t) e3 e4 ->
  [Clause t [Pattern t] (Expr t Name (CaseClause t) e3 e4)] ->
  [ConsGroup t (Expr t Name (CaseClause t) e3 e4)]
consGroups u cs =
  concatMap go (groupSortOn fst (info u <$> cs))
  where
    go all@((con, (t, ps, _)) : _) =
      [ ConsGroup
          { consName     = con
          , consType     = t
          , consPatterns = ps
          , consClauses  = thd3 . snd <$> all
          }
      ]
    info u (Clause t (p : qs) [Choice exs e]) =
      case project p of
        PCon _ con ps -> (con, (t, ps, Clause t (ps <> qs) [Choice exs e]))
        PAs  _ as  q  -> info u (Clause t (q:qs) [Choice exs (substitute as u e)])

{- ORMOLU_ENABLE -}

substitute ::
  (Functor e3) =>
  Name ->
  Expr t Name (CaseClause t) e3 e4 ->
  Expr t Name (CaseClause t) e3 e4 ->
  Expr t Name (CaseClause t) e3 e4
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
            substEq eq@(Case _ _ ps _)
              | name `elem` ps = fst <$> eq
              | otherwise = snd <$> eq
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

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- =============
-- == Stage 3 ==
-- =============

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- =============
-- == Stage 4 ==
-- =============

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- =============
-- == Postpr. ==
-- =============
