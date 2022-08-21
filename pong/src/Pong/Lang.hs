{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pong.Lang
  ( module Pong.Data
  , module Pong.Data.Cons
  , Free
  , argTypes
  , arity
  , boundVars
  , foldRow
  , foldRow1
  , foldType
  , foldType1
  , forEachDef
  , free
  , freeIndex
  , freeVars
  , hasHeadE
  , hasHeadT
  , insertArgs
  , mapTypes
  , normalizeRows
  , renameDef
  , restrictRow
  , returnType
  , rowEq
  , substituteVar
  , toMonoType
  , toScheme
  , typeOf
  , untag
  , unwindRow
  , unwindType
  )
where

import Control.Monad.State
import Data.List (nub)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Pong.Data
import Pong.Data.Cons
import Pong.Util
  ( Map
  , Name
  , Void
  , cata
  , embed
  , first
  , para
  , project
  , swap
  , varSequence
  , withoutLabels
  , (!)
  , (<&>)
  , (<<<)
  , (>>>)
  )
import Pong.Util.Env (Environment (..))
import qualified Pong.Util.Env as Env

normalizeRows :: Type v -> Type v
normalizeRows =
  cata
    ( \case
        t@RExt{} -> uncurry (flip foldRow) (unwindRow (embed t))
        t -> embed t
    )

{-# INLINE foldRow #-}
foldRow :: Type v -> FieldSet (Type v) -> Type v
foldRow = Map.foldrWithKey (flip . foldr . rExt)

{-# INLINE foldRow1 #-}
foldRow1 :: FieldSet (Type v) -> Type v
foldRow1 = foldRow rNil

unwindRow :: Type v -> (FieldSet (Type v), Type v)
unwindRow ty = (toMap fields, leaf)
  where
    toMap = foldr (uncurry (Map.insertWith (<>))) mempty
    fields =
      (`para` ty)
        ( \case
            RExt label (t, _) (_, rest) ->
              (label, [t]) : rest
            _ ->
              []
        )
    leaf =
      (`cata` ty)
        ( \case
            RExt _ _ r -> r
            t -> embed t
        )

restrictRow :: Name -> Type v -> (Type v, Type v)
restrictRow field row =
  ( e
  , normalizeRows
      ( foldRow k $
          case es of
            [] -> Map.delete field m
            _ -> Map.insert field es m
      )
  )
  where
    Just (e : es) = Map.lookup field m
    (m, k) = unwindRow row

{-# INLINE rowEq #-}
rowEq :: (Eq v) => Type v -> Type v -> Bool
rowEq t1 t2 = normalizeRows t1 == normalizeRows t2

class Free f where
  freeIn :: f -> [Int]

instance Free Scheme where
  freeIn _ = []

instance Free Void where
  freeIn _ = []

instance Free () where
  freeIn _ = []

instance (Free a) => Free [a] where
  freeIn = concatMap freeIn

instance (Free a) => Free (Either e a) where
  freeIn = concatMap freeIn

instance (Free t, Free a) => Free (Module t a) where
  freeIn (Module _ defs) = freeIn defs

{- ORMOLU_DISABLE -}

instance Free MonoType where
  freeIn =
    cata
      ( \case
          TCon _ ts    -> join ts
          TArr t1 t2   -> t1 <> t2
          TVar n       -> [n]
          TRec row     -> row
          RExt _ t1 t2 -> t1 <> t2
          _            -> []
      )

{- ORMOLU_ENABLE -}

instance (Free t, Free a) => Free (Definition t a) where
  freeIn =
    \case
      Function as (t, a) ->
        (freeIn . fst =<< toList as) <> freeIn t <> freeIn a
      Constant (t, a) ->
        freeIn t <> freeIn a
      Extern ts t ->
        freeIn ts <> freeIn t
      _ ->
        []

instance (Free t, Free a) => Free (ModuleDefs t a) where
  freeIn = freeIn . Map.elems

instance (Free a) => Free (Environment a) where
  freeIn env = freeIn =<< Env.elems env

{- ORMOLU_DISABLE -}

instance (Free t, Free a0, Free a2) => Free (Expr t a0 a1 a2) where
  freeIn =
    cata
      ( \case
          EVar (t, _)       -> freeIn t
          ECon (t, _)       -> freeIn t
          ELit{}            -> []
          EIf e1 e2 e3      -> e1 <> e2 <> e3
          ELet (t, _) e1 e2 -> freeIn t <> e1 <> e2
          EApp t fun as     -> freeIn t <> fun <> concat as
          ELam _ args e1    -> freeIn (fst <$> args) <> e1
          ECall a (t, _) as -> freeIn a <> freeIn t <> concat as
          EOp1 (t, _) e1    -> freeIn t <> e1
          EOp2 (t, _) e1 e2 -> freeIn t <> e1 <> e2
          EPat e1 cs        -> e1 <> (freeIn . fst =<< fst =<< cs) <> (snd =<< cs)
          ENil              -> []
          EExt _ e1 e2      -> e1 <> e2
          ERes fs e1 e2     -> freeIn (fst <$> fs) <> e1 <> e2
      )

{- ORMOLU_ENABLE -}

{-# INLINE free #-}
free :: (Free a) => a -> [Int]
free = nub . freeIn

class Typed t where
  typeOf :: t -> MonoType

instance Typed MonoType where
  typeOf = id

instance Typed Void where
  typeOf _ = tCon "Void" []

{- ORMOLU_DISABLE -}

instance Typed Prim where
  typeOf =
    \case
      PBool{}   -> tBool
      PInt{}    -> tInt
      PFloat{}  -> tFloat
      PDouble{} -> tDouble
      PChar{}   -> tChar
      PString{} -> tString
      PUnit     -> tUnit

{- ORMOLU_DISABLE -}

instance (Typed t, Typed a0) => Typed (Expr t a0 a1 a2) where
  typeOf =
    cata
      ( \case
          EVar (t, _)       -> typeOf t
          ECon (t, _)       -> typeOf t
          ELit lit          -> typeOf lit
          EIf  _ _ e3       -> e3
          ELet _ _ e3       -> e3
          EApp t _ _        -> typeOf t
          ELam _ args expr  -> foldType expr (typeOf . fst <$> args)
          ECall _ (t, _) as -> foldType1 (drop (length as) (unwindType t))
          EOp1 (t, _) _     -> returnType t
          EOp2 (t, _) _ _   -> returnType t
          EPat _ []         -> error "Empty case statement"
          EPat _ cs         -> head (snd <$> cs)
          ENil              -> tRec rNil
          ERes _ _ e        -> e
          EExt n e1 e2 -> 
            case project e2 of
              TRec r -> tRec (rExt n e1 r)
              _ -> error "Implementation error"
      )

{- ORMOLU_ENABLE -}

instance (Typed t) => Typed (Definition t a) where
  typeOf =
    \case
      Function args (t, _) ->
        foldType (typeOf t) (typeOf . fst <$> toList args)
      Constant (t, _) ->
        typeOf t
      Extern ts t ->
        foldType t ts
      _ ->
        error "Not implemented"

{-# INLINE arity #-}
arity :: (Typed t) => t -> Int
arity = pred <<< length <<< unwindType

freeIndex :: (Free t) => [t] -> Int
freeIndex ts =
  case free =<< ts of
    [] -> 0
    vs -> succ (maximum vs)

hasHeadT :: HeadT -> Type v -> Bool
hasHeadT con =
  project >>> \case
    TArr{}
      | ArrT == con -> True
    TVar{}
      | VarT == con -> True
    TRec{}
      | RecT == con -> True
    _ -> False

hasHeadE :: HeadE -> Expr t a0 a1 a2 -> Bool
hasHeadE con =
  project >>> \case
    EVar{}
      | VarE == con -> True
    ELit{}
      | LitE == con -> True
    ELam{}
      | LamE == con -> True
    _ -> False

unwindType :: (Typed t) => t -> [MonoType]
unwindType =
  typeOf
    >>> para
      ( \case
          TArr (t, _) (_, u) -> t : u
          t -> [embed (fst <$> t)]
      )

{-# INLINE returnType #-}
returnType :: (Typed t) => t -> MonoType
returnType = last <<< unwindType

{-# INLINE argTypes #-}
argTypes :: (Typed t) => t -> [MonoType]
argTypes = init <<< unwindType

class FreeVars f t | f -> t where
  freeVarsIn :: f -> Set (Label t)

instance FreeVars (Label Void) MonoType where
  freeVarsIn _ = mempty

instance FreeVars (Label ()) () where
  freeVarsIn = Set.singleton

instance FreeVars (Label MonoType) MonoType where
  freeVarsIn = Set.singleton

{- ORMOLU_DISABLE -}

instance
  (Ord t, FreeVars (Label t) t, FreeVars (Label a0) t) =>
  FreeVars (Expr t a0 a1 a2) t
  where
  freeVarsIn =
    cata
      ( \case
          EVar var            -> freeVarsIn var
          ECon con            -> freeVarsIn con
          ELit _              -> mempty
          EIf e1 e2 e3        -> e1 <> e2 <> e3
          ELet var e1 e2      -> (e1 <> e2) \\\ [var]
          EApp _ fun args     -> fun <> Set.unions args
          ELam _ args expr    -> expr \\\ args
          ECall _ fun args    -> Set.insert fun (Set.unions args)
          EOp1 _ e1           -> e1
          EOp2 _ e1 e2        -> e1 <> e2
          ENil                -> mempty
          EExt _ e1 e2        -> e1 <> e2
          ERes (_ : vs) e1 e2 -> (e1 <> e2) \\\ vs
          EPat e1 cs ->
            e1 <>
              Set.unions (cs <&> \(_ : vs, expr) -> expr \\\ vs)
          _ ->
            error "Implementation error"
      )

{- ORMOLU_ENABLE -}

instance
  (Ord t, FreeVars (Label t) t, FreeVars (Label a0) t) =>
  FreeVars (ModuleDefs t (Expr t a0 a1 a2)) t
  where
  freeVarsIn p = Set.unions (go . snd <$> Map.toList p)
    where
      go =
        \case
          Function args (_, expr) ->
            freeVarsIn expr \\\ toList args
          Constant (_, expr) ->
            freeVarsIn expr
          _ ->
            mempty

(\\\) :: (Ord t) => Set (Label t) -> [Label t] -> Set (Label t)
(\\\) = flip (overSet . withoutLabels . fmap snd)
  where
    overSet f = Set.fromList . f . Set.toList

infix 5 \\\

{-# INLINE freeVars #-}
freeVars :: (FreeVars f t) => f -> [Label t]
freeVars = Set.toList . Set.filter ((/= "{{data}}") . snd) . freeVarsIn

{- ORMOLU_DISABLE -}

toMonoType :: Map Name Int -> Type Name -> MonoType
toMonoType vs =
  cata
    ( \case
        TVar s        -> tVar (vs ! s)
        TUnit         -> tUnit
        TBool         -> tBool
        TInt          -> tInt
        TFloat        -> tFloat
        TDouble       -> tDouble
        TChar         -> tChar
        TString       -> tString
        TCon con ts   -> tCon con ts
        TArr t1 t2    -> tArr t1 t2
        TRec row      -> tRec row
        RNil          -> rNil
        RExt n t1 t2  -> rExt n t1 t2
    )

toScheme :: Name -> [Int] -> MonoType -> Scheme
toScheme prefix vars =
  Scheme 
    <<< cata
      ( \case
         TVar n       -> tVar (names ! n)
         TUnit        -> tUnit
         TBool        -> tBool
         TInt         -> tInt
         TFloat       -> tFloat
         TDouble      -> tDouble
         TChar        -> tChar
         TString      -> tString
         TCon con ts  -> tCon con ts
         TArr t1 t2   -> tArr t1 t2
         TRec row     -> tRec row
         RNil         -> rNil
         RExt n t1 t2 -> rExt n t1 t2
      )
 where
   names =
     Map.fromList (varSequence prefix vars)

mapTypes :: (s -> t) -> Expr s s a1 a2 -> Expr t t a1 a2
mapTypes f =
 cata
   ( \case
       EVar (t, v)          -> eVar (f t, v)
       ECon (t, c)          -> eCon (f t, c)
       ELit lit             -> eLit lit
       EIf e1 e2 e3         -> eIf e1 e2 e3
       ELet (t, a) e1 e2    -> eLet (f t, a) e1 e2
       EApp t fun as        -> eApp (f t) fun as
       ELam a args e1       -> eLam a (fmap (first f) args) e1
       ECall a (t, fun) as  -> eCall a (f t, fun) as
       EOp1 (t, op1) e1     -> eOp1 (f t, op1) e1
       EOp2 (t, op2) e1 e2  -> eOp2 (f t, op2) e1 e2
       EPat e1 cs           -> ePat e1 ((first . fmap . first) f <$> cs)
       ENil                 -> eNil
       EExt n e1 e2         -> eExt n e1 e2
       ERes fs e1 e2        -> eRes (first f <$> fs) e1 e2
   )

untag :: (Eq t) => Expr t t a1 a2 -> [t]
untag =
  nub
    <<< cata
      ( \case
          EVar (t, _)       -> [t]
          ECon (t, _)       -> [t]
          ELit _            -> []
          EIf e1 e2 e3      -> e1 <> e2 <> e3
          ELet (t, _) e1 e2 -> [t] <> e1 <> e2
          ELam _ args e1    -> (fst <$> args) <> e1
          EApp t fun as     -> [t] <> fun <> concat as
          ECall _ (t, _) as -> [t] <> concat as
          EOp1 (t, _) e1    -> [t] <> e1
          EOp2 (t, _) e1 e2 -> [t] <> e1 <> e2
          EPat e1 cs        -> e1 <> concat (fmap fst . fst <$> cs)
          ENil              -> []
          EExt _ e1 e2      -> e1 <> e2
          ERes fs e1 e2     -> (fst <$> fs) <> e1 <> e2
      )

boundVars :: Type Name -> Set Name
boundVars =
 cata
   ( \case
       TCon _ ts    -> Set.unions ts
       TArr t1 t2   -> t1 <> t2
       TVar s       -> Set.singleton s
       TRec row     -> row
       RExt _ t1 t2 -> t1 <> t2
       _            -> mempty
   )

{- ORMOLU_ENABLE -}

substituteVar :: Name -> Name -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
substituteVar from to =
  para
    ( \case
        ELet (t, var) (e1, _) (e2, _)
          | var == from -> eLet (t, var) e1 e2
        ERes r@[_, (_, var), _] (e1, _) (e2, _)
          | var == from -> eRes r e1 e2
        EPat (_, e1) cs ->
          ePat
            e1
            ( cs <&> \(ps, (l, r)) ->
                ( ps
                , if from `elem` (snd <$> ps) then l else r
                )
            )
        EVar (t, v)
          | v == from -> eVar (t, to)
        ECon (t, c)
          | c == from -> eCon (t, to)
        ECall a (t, fun) args
          | fun == from -> eCall a (t, to) (snd <$> args)
        e ->
          embed (snd <$> e)
    )

{-# INLINE foldType #-}
foldType :: Type v -> [Type v] -> Type v
foldType = foldr tArr

{-# INLINE foldType1 #-}
foldType1 :: [Type v] -> Type v
foldType1 = foldr1 tArr

{-# INLINE insertArgs #-}
insertArgs :: [Label t] -> Environment t -> Environment t
insertArgs = Env.inserts . (swap <$>)

renameDef :: Name -> Name -> ModuleDefs t a -> ModuleDefs t a
renameDef from to = Map.mapKeys rename
  where
    rename (t, defn) = (t, if defn == from then to else defn)

{-# INLINE forEachDef #-}
forEachDef ::
  (Monad m) =>
  ModuleDefs t a ->
  (Label Scheme -> Definition t a -> m b) ->
  m [b]
forEachDef p = forM (Map.toList p) . uncurry
