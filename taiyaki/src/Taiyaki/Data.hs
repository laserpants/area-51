{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Taiyaki.Data where

import Taiyaki.Util
import Taiyaki.Util.Env (Environment)

-------------------------------------------------------------------------------

data KindF a
  = KTyp
  | KRow
  | KArr a a

type Kind = Fix KindF

-------------------------------------------------------------------------------

newtype MonoIndex = MonoIndex Int

-- newtype PolyIndex = PolyIndex Int

-------------------------------------------------------------------------------

data TypeF v a
  = TUnit
  | TBool
  | TInt
  | TBig
  | TNat
  | TFloat
  | TDouble
  | TChar
  | TString
  | TVoid
  | TList a
  | TVar Kind v
  | TCon Kind Name
  | TApp Kind a a
  | TArr a a
  | TRec a
  | TNil
  | TExt Name a a

type Type v = Fix (TypeF v)

-------------------------------------------------------------------------------

type MonoType = Type MonoIndex

-- newtype Generic = Generic (Type PolyIndex)

-------------------------------------------------------------------------------

-- | A standalone type class constraint
data Predicate a = InClass Name a

-- data Scheme
--  = Forall [Kind] [Predicate Generic] Generic

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

data Prim
  = IUnit
  | IBool    Bool
  | IInt     Int
  | IBig     Integer
  | INat     Integer
  | IFloat   Float
  | IDouble  Double
  | IChar    Char
  | IString  Text

{- ORMOLU_ENABLE -}

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

data PatternF t a
  = PVar  t Name
  | PLit  t Prim
  | PAs   t Name a
  | POr   t a a
  | PAny  t
  | PCon  t Name [a]
  | PTup  t [a]
  | PList t [a]
  | PRec  t a
  | PNil  t
  | PExt  t Name a a
  | PAnn  t a

{- ORMOLU_ENABLE -}

type Pattern t = Fix (PatternF t)

-------------------------------------------------------------------------------

data Binding t
  = BPat t (Pattern t)
  | BFun t Name [Pattern t]

-------------------------------------------------------------------------------

-- | A pattern clause choice is a (possibly empty) list of pattern guards, and
-- a target expression.
data Choice a
  = Choice [a] a

-- | A pattern match clause consists of a list of patterns, and one or more
-- choices.
data Clause t p a
  = Clause t p [Choice a]

data CaseClause t a
  = Case t Name [Name] a

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

data Op1 t
  = ONot t
  | ONeg t

-------------------------------------------------------------------------------

data Op2 t
  = OEq   t
  | ONEq  t
  | OLt   t
  | OGt   t
  | OLtE  t
  | OGtE  t
  | OAdd  t
  | OSub  t
  | OMul  t
  | ODiv  t
  | OPow  t
  | OMod  t
  | OOr   t
  | OAnd  t
  | OLArr t
  | ORarr t
  | OFPip t
  | OBPip t
  | ODot  t
  | OGet  t

-------------------------------------------------------------------------------

data ExprF t e1 e2 e3 e4 a
  = EVar  t Name
  | ECon  t Name
  | ELit  t Prim
  | EApp  t a [a]
  | ELam  t e1 a
  | EIf   t a a a
  | EPat  t a [e2 a]
  | ELet  t e4 a a
  | EFix  t Name a a
  | EFun  t [e3 a]
  | EOp1  t (Op1 t) a
  | EOp2  t (Op2 t) a a
  | ETup  t [a]
  | EList t [a]
  | ERec  t a
  | ENil  t
  | EExt  t Name a a
  | ESub  t
  | ECo   t a
  | EAnn  t a

{- ORMOLU_ENABLE -}

type Expr t e1 e2 e3 e4 = Fix (ExprF t e1 e2 e3 e4)

-------------------------------------------------------------------------------

type ProgExpr t = Expr t [Pattern t] (Clause t [Pattern t]) (Clause t [Pattern t]) (Binding t)

newtype Ast t = Ast (ProgExpr t)

-------------------------------------------------------------------------------

type FieldSet a = Map Name [a]

-------------------------------------------------------------------------------

data Assoc
  = ALeft
  | ARight
  | ANone

-------------------------------------------------------------------------------

type PatternMatrix t = [[Pattern t]]

data PatternGroup t
  = ConGroup Name [Pattern t]
  | OrPattern (Pattern t) (Pattern t)
  | WildcardPattern

data Labeled a
  = LCon a
  | LVar a

{- ORMOLU_DISABLE -}

data ConsGroup t a = ConsGroup
  { consGroupName     :: Name
  , consGroupType     :: t
  , consGroupPatterns :: [Pattern t]
  , consGroupClauses  :: [Clause t [Pattern t] a]
  }

data ClassInfo t = ClassInfo
  { classInfoSuperClasses :: [Name]
  , classInfoParameter    :: t
  , classInfoInterface    :: [(Name, t)]
  }

data ClassInstance t = ClassInstance
  { classInstancePredicates :: [Predicate t]
  , classInstanceSignature  :: t
  , classInstanceMethods    :: [(Name, ProgExpr t)]
  }

{- ORMOLU_ENABLE -}

-------------------------------------------------------------------------------

type ConstructorEnv =
  Environment (Set Name, Int)

type ClassEnv t =
  Environment (ClassInfo t, [ClassInstance t])

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

-- Kind
deriving instance (Show a) =>
  Show (KindF a)

deriving instance (Eq a) =>
  Eq (KindF a)

deriving instance (Ord a) =>
  Ord (KindF a)

deriving instance (Data a) =>
  Data (KindF a)

deriving instance (Typeable a) =>
  Typeable (KindF a)

deriveShow1 ''KindF

deriveEq1 ''KindF

deriveOrd1 ''KindF

deriving instance Functor KindF

deriving instance Foldable KindF

deriving instance Traversable KindF

-- Type
deriving instance (Show v, Show a) =>
  Show (TypeF v a)

deriving instance (Eq v, Eq a) =>
  Eq (TypeF v a)

deriving instance (Ord v, Ord a) =>
  Ord (TypeF v a)

deriving instance (Data v, Data a) =>
  Data (TypeF v a)

deriving instance (Typeable v, Typeable a) =>
  Typeable (TypeF v a)

deriveShow1 ''TypeF

deriveEq1 ''TypeF

deriveOrd1 ''TypeF

deriving instance Functor (TypeF v)

deriving instance Foldable (TypeF v)

deriving instance Traversable (TypeF v)

-- Predicate
deriving instance (Show t) =>
  Show (Predicate t)

deriving instance (Eq t) =>
  Eq (Predicate t)

deriving instance (Ord t) =>
  Ord (Predicate t)

deriving instance (Data t) =>
  Data (Predicate t)

deriving instance (Typeable t) =>
  Typeable (Predicate t)

-- Prim
deriving instance Show Prim

deriving instance Eq Prim

deriving instance Ord Prim

deriving instance Data Prim

deriving instance Typeable Prim

-- Pattern
deriving instance (Show t, Show a) =>
  Show (PatternF t a)

deriving instance (Eq t, Eq a) =>
  Eq (PatternF t a)

deriving instance (Ord t, Ord a) =>
  Ord (PatternF t a)

deriving instance (Data t, Data a) =>
  Data (PatternF t a)

deriving instance (Typeable t, Typeable a) =>
  Typeable (PatternF t a)

deriveShow1 ''PatternF

deriveEq1 ''PatternF

deriveOrd1 ''PatternF

deriving instance Functor (PatternF t)

deriving instance Foldable (PatternF t)

deriving instance Traversable (PatternF t)

-- Binding
deriving instance (Show t) =>
  Show (Binding t)

deriving instance (Eq t) =>
  Eq (Binding t)

deriving instance (Ord t) =>
  Ord (Binding t)

deriving instance (Data t) =>
  Data (Binding t)

deriving instance (Typeable t) =>
  Typeable (Binding t)

-- Choice
deriving instance (Show a) =>
  Show (Choice a)

deriving instance (Eq a) =>
  Eq (Choice a)

deriving instance (Ord a) =>
  Ord (Choice a)

deriving instance (Data a) =>
  Data (Choice a)

deriving instance (Typeable a) =>
  Typeable (Choice a)

deriveShow1 ''Choice

deriveEq1 ''Choice

deriveOrd1 ''Choice

deriving instance Functor Choice

deriving instance Foldable Choice

deriving instance Traversable Choice

-- Clause
deriving instance (Show t, Show p, Show a) =>
  Show (Clause t p a)

deriving instance (Eq t, Eq p, Eq a) =>
  Eq (Clause t p a)

deriving instance (Ord t, Ord p, Ord a) =>
  Ord (Clause t p a)

deriving instance (Data t, Data p, Data a) =>
  Data (Clause t p a)

deriving instance (Typeable t, Typeable p, Typeable a) =>
  Typeable (Clause t p a)

deriveShow1 ''Clause

deriveEq1 ''Clause

deriveOrd1 ''Clause

deriving instance Functor (Clause t p)

deriving instance Foldable (Clause t p)

deriving instance Traversable (Clause t p)

-- CaseClause
deriving instance (Show t, Show a) => Show (CaseClause t a)

deriving instance (Eq t, Eq a) => Eq (CaseClause t a)

deriving instance (Ord t, Ord a) => Ord (CaseClause t a)

deriving instance (Data t, Data a) => Data (CaseClause t a)

deriving instance (Typeable t, Typeable a) => Typeable (CaseClause t a)

deriveShow1 ''CaseClause

deriveEq1 ''CaseClause

deriveOrd1 ''CaseClause

deriving instance Functor (CaseClause t)

deriving instance Foldable (CaseClause t)

deriving instance Traversable (CaseClause t)

-- Op1
deriving instance (Show t) =>
  Show (Op1 t)

deriving instance (Eq t) =>
  Eq (Op1 t)

deriving instance (Ord t) =>
  Ord (Op1 t)

deriving instance (Data t) =>
  Data (Op1 t)

deriving instance (Typeable t) =>
  Typeable (Op1 t)

-- Op2
deriving instance (Show t) =>
  Show (Op2 t)

deriving instance (Eq t) =>
  Eq (Op2 t)

deriving instance (Ord t) =>
  Ord (Op2 t)

deriving instance (Data t) =>
  Data (Op2 t)

deriving instance (Typeable t) =>
  Typeable (Op2 t)

-- Expr
deriving instance (Show t, Show e1, Show (e2 a), Show (e3 a), Show e4, Show a) =>
  Show (ExprF t e1 e2 e3 e4 a)

deriving instance (Eq t, Eq e1, Eq (e2 a), Eq (e3 a), Eq e4, Eq a) =>
  Eq (ExprF t e1 e2 e3 e4 a)

deriving instance (Ord t, Ord e1, Ord (e2 a), Ord (e3 a), Ord e4, Ord a) =>
  Ord (ExprF t e1 e2 e3 e4 a)

deriving instance (Typeable e2, Typeable e3, Data t, Data e1, Data (e2 a), Data (e3 a), Data e4, Data a) =>
  Data (ExprF t e1 e2 e3 e4 a)

deriving instance (Typeable t, Typeable e1, Typeable e4, Typeable a) =>
  Typeable (ExprF t e1 e2 e3 e4 a)

deriveShow1 ''ExprF

deriveEq1 ''ExprF

deriveOrd1 ''ExprF

deriving instance (Functor e2, Functor e3) =>
  Functor (ExprF t e1 e2 e3 e4)

deriving instance (Foldable e2, Foldable e3) =>
  Foldable (ExprF t e1 e2 e3 e4)

deriving instance (Traversable e2, Traversable e3) =>
  Traversable (ExprF t e1 e2 e3 e4)

-- Assoc
deriving instance Show Assoc

deriving instance Eq Assoc

deriving instance Ord Assoc

deriving instance Data Assoc

deriving instance Typeable Assoc

-- PatternGroup
deriving instance (Show t) =>
  Show (PatternGroup t)

deriving instance (Eq t) =>
  Eq (PatternGroup t)

deriving instance (Ord t) =>
  Ord (PatternGroup t)

deriving instance (Data t) =>
  Data (PatternGroup t)

deriving instance (Typeable t) =>
  Typeable (PatternGroup t)

-- Labeled
deriving instance (Show a) =>
  Show (Labeled a)

deriving instance (Eq a) =>
  Eq (Labeled a)

deriving instance (Ord a) =>
  Ord (Labeled a)

deriving instance (Data a) =>
  Data (Labeled a)

deriving instance (Typeable a) =>
  Typeable (Labeled a)

deriveShow1 ''Labeled

deriveEq1 ''Labeled

deriveOrd1 ''Labeled

deriving instance Functor Labeled

deriving instance Foldable Labeled

deriving instance Traversable Labeled

-- ConsGroup
deriving instance (Show t, Show a) =>
  Show (ConsGroup t a)

deriving instance (Eq t, Eq a) =>
  Eq (ConsGroup t a)

deriving instance (Ord t, Ord a) =>
  Ord (ConsGroup t a)

deriving instance (Data t, Data a) =>
  Data (ConsGroup t a)

deriving instance (Typeable t, Typeable a) =>
  Typeable (ConsGroup t a)

deriveShow1 ''ConsGroup

deriveEq1 ''ConsGroup

deriveOrd1 ''ConsGroup

deriving instance Functor (ConsGroup t)

deriving instance Foldable (ConsGroup t)

deriving instance Traversable (ConsGroup t)

-- ClassInfo
deriving instance (Show t) =>
  Show (ClassInfo t)

deriving instance (Eq t) =>
  Eq (ClassInfo t)

deriving instance (Ord t) =>
  Ord (ClassInfo t)

-- ClassInstance
deriving instance (Show t) =>
  Show (ClassInstance t)

deriving instance (Eq t) =>
  Eq (ClassInstance t)

deriving instance (Ord t) =>
  Ord (ClassInstance t)
