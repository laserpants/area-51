{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pong.Lang where

import Control.Monad.State
import Control.Newtype.Generics (over, unpack)
import Data.Char (isUpper)
import Data.Foldable (foldrM)
import Data.List (nub)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple (swap)
import Data.Tuple.Extra (first, second)
import Pong.Data
import Pong.Util
  ( Map
  , Name
  , Void
  , cata
  , embed
  , embed1
  , embed2
  , embed3
  , para
  , project
  , varSequence
  , (!)
  , (<$$>)
  , (<&>)
  , (<<<)
  , (>>>)
  )
import Pong.Util.Env (Environment (..))
import qualified Pong.Util.Env as Env

mapRow :: (e -> f) -> Row e v -> Row f v
mapRow f =
  cata $
    \case
      RNil -> rNil
      RVar v -> rVar v
      RExt name el row -> rExt name (f el) row

mapRowM :: (Monad m) => (e -> m f) -> Row e v -> m (Row f v)
mapRowM f =
  cata $
    \case
      RNil -> pure rNil
      RVar v -> pure (rVar v)
      RExt name el row -> rExt name <$> f el <*> row

bimapRow :: (e -> f) -> (v -> w) -> Row e v -> Row f w
bimapRow f g =
  cata $
    \case
      RNil -> rNil
      RVar v -> rVar (g v)
      RExt name el row -> rExt name (f el) row

normalizeRow :: Row e v -> Row e v
normalizeRow = uncurry (flip foldRow) . unwindRow

noramlizeTypeRows :: Type v -> Type v
noramlizeTypeRows =
  cata $
    \case
      TRec r -> tRec (normalizeRow r)
      t -> embed t

{-# INLINE foldRow #-}
foldRow :: Row e v -> Map Name [e] -> Row e v
foldRow = Map.foldrWithKey (flip . foldr . rExt)

{-# INLINE foldRow1 #-}
foldRow1 :: Map Name [e] -> Row e v
foldRow1 = foldRow rNil

unwindRow :: Row e v -> (Map Name [e], Row e v)
unwindRow row = (toMap fields, leaf)
  where
    toMap = foldr (uncurry (Map.insertWith (<>))) mempty
    fields =
      (`para` row)
        ( \case
            RExt label ty (_, rest) -> (label, [ty]) : rest
            _ -> []
        )
    leaf =
      (`cata` row)
        ( \case
            RExt _ _ r -> r
            r -> embed r
        )

restrictRow :: Name -> Row e v -> (e, Row e v)
restrictRow name row =
  ( e
  , normalizeRow
      ( foldRow k $
          case es of
            [] -> Map.delete name m
            _ -> Map.insert name es m
      )
  )
  where
    Just (e : es) = Map.lookup name m
    (m, k) = unwindRow row

rowEq :: (Eq e, Eq v) => Row e v -> Row e v -> Bool
rowEq r1 r2 = normalizeRow r1 == normalizeRow r2

typeEq :: (Eq v) => Type v -> Type v -> Bool
typeEq t1 t2 = noramlizeTypeRows t1 == noramlizeTypeRows t2

class FreeIn f where
  freeIn :: f -> [Int]

instance FreeIn Scheme where
  freeIn _ = []

instance FreeIn Void where
  freeIn _ = []

instance FreeIn () where
  freeIn _ = []

instance FreeIn MonoType where
  freeIn =
    cata
      ( \case
          TVar n -> [n]
          TCon _ ts -> join ts
          TArr t1 t2 -> t1 <> t2
          TRec row -> freeIn row
          _ -> []
      )

instance FreeIn (Row MonoType Int) where
  freeIn =
    cata
      ( \case
          RVar v -> [v]
          RExt _ t r -> freeIn t <> r
          _ -> []
      )

instance (FreeIn a) => FreeIn [a] where
  freeIn = concatMap freeIn

instance (FreeIn a) => FreeIn (Either e a) where
  freeIn = concatMap freeIn

instance (FreeIn t) => FreeIn (Constructor t) where
  freeIn (Constructor _ fs) = freeIn fs

instance (FreeIn t, FreeIn a) => FreeIn (Definition t a) where
  freeIn =
    \case
      Function as (t, a) ->
        (freeIn . fst =<< toList as) <> freeIn t <> freeIn a
      Constant (t, a) ->
        freeIn t <> freeIn a
      Extern ts t ->
        freeIn ts <> freeIn t
      Data _ cs ->
        freeIn cs

instance (FreeIn t, FreeIn a) => FreeIn (Program t a) where
  freeIn (Program p) = freeIn (Map.elems p)

instance (FreeIn a) => FreeIn (Environment a) where
  freeIn env = freeIn =<< Env.elems env

instance (FreeIn t, FreeIn a0, FreeIn a2) => FreeIn (Expr t a0 a1 a2) where
  freeIn =
    cata
      ( \case
          EVar (t, _) -> freeIn t
          ECon (t, _) -> freeIn t
          ELit{} -> []
          EIf e1 e2 e3 -> e1 <> e2 <> e3
          ELet (t, _) e1 e2 -> freeIn t <> e1 <> e2
          ELam _ args e1 -> freeIn (fst <$> args) <> e1
          EApp t fun as -> freeIn t <> fun <> concat as
          ECall a (t, _) as -> freeIn a <> freeIn t <> concat as
          EOp1 (t, _) e1 -> freeIn t <> e1
          EOp2 (t, _) e1 e2 -> freeIn t <> e1 <> e2
          EPat e1 cs -> e1 <> (freeInClause =<< cs)
          ERec row -> freeInRow row
          ERes fs e1 e2 -> freeIn (fst <$> fs) <> e1 <> e2
      )
    where
      freeInClause (ls, e) =
        (freeIn . fst =<< ls) <> e
      freeInRow =
        cata
          ( \case
              RNil -> []
              RVar (t, _) -> freeIn t
              RExt _ el row -> freeIn el <> row
          )

free :: (FreeIn a) => a -> [Int]
free = nub . freeIn

class Typed t where
  typeOf :: t -> MonoType

instance Typed MonoType where
  typeOf = id

instance Typed Void where
  typeOf _ = tCon "Void" []

instance Typed Prim where
  typeOf =
    \case
      PBool{} -> tBool
      PInt{} -> tInt
      PFloat{} -> tFloat
      PDouble{} -> tDouble
      PChar{} -> tChar
      PString{} -> tString
      PUnit -> tUnit

instance (Typed t, Typed a0) => Typed (Row (Expr t a0 a1 a2) (Label t)) where
  typeOf =
    cata
      ( \case
          RNil -> tRec rNil
          RVar (t, _) -> typeOf t
          RExt name el r ->
            let TRec row = project r
             in tRec (rExt name (typeOf el) row)
      )

instance (Typed t, Typed a0) => Typed (Expr t a0 a1 a2) where
  typeOf =
    cata
      ( \case
          EVar (t, _) -> typeOf t
          ECon (t, _) -> typeOf t
          ELit lit -> typeOf lit
          EIf _ _ e3 -> e3
          ELet _ _ e3 -> e3
          ELam _ args expr -> foldType expr (typeOf . fst <$> args)
          EApp t _ _ -> typeOf t
          ECall _ (t, _) as -> foldType1 (drop (length as) (unwindType t))
          EOp1 (t, _) _ -> returnType t
          EOp2 (t, _) _ _ -> returnType t
          EPat _ [] -> error "Empty case statement"
          EPat _ cs -> head (snd <$> cs)
          ERec r -> typeOf r
          ERes _ _ e -> e
      )

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
        error "TODO"

{-# INLINE arity #-}
arity :: (Typed t) => t -> Int
arity = pred <<< length <<< unwindType

freeIndex :: (FreeIn t) => [t] -> Int
freeIndex ts =
  case free =<< ts of
    [] -> 0
    vs -> succ (maximum vs)

isConT :: ConT -> Type v -> Bool
isConT con =
  project >>> \case
    TArr{}
      | ArrT == con -> True
    TVar{}
      | VarT == con -> True
    TRec{}
      | RecT == con -> True
    _ -> False

isConE :: ConE -> Expr t a0 a1 a2 -> Bool
isConE con =
  project >>> \case
    EVar{}
      | VarE == con -> True
    ELit{}
      | LitE == con -> True
    ELam{}
      | LamE == con -> True
    ERec{}
      | RecE == con -> True
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

freeVars :: (Eq t, Ord t) => Expr t a0 a1 a2 -> [Label t]
freeVars =
  Set.toList
    <<< cata
      ( \case
          EVar v
            | isUpper (Text.head (snd v)) -> mempty
            | otherwise -> Set.singleton v
          ECon _ -> mempty
          ELit _ -> mempty
          EIf e1 e2 e3 -> e1 <> e2 <> e3
          ELet bind e1 e2 -> Set.delete bind (e1 <> e2)
          ELam _ args expr -> expr \\ Set.fromList args
          EApp _ fun args -> fun <> Set.unions args
          ECall _ fun args
            | isUpper (Text.head (snd fun)) -> Set.unions args
            | otherwise -> Set.insert fun (Set.unions args)
          EOp1 _ e1 -> e1
          EOp2 _ e1 e2 -> e1 <> e2
          EPat e1 cs ->
            e1
              <> Set.unions (cs <&> \(_ : vs, expr) -> expr \\ Set.fromList vs)
          ERec row ->
            (`cata` row)
              ( \case
                  RNil -> mempty
                  RVar v -> Set.singleton v
                  RExt _ el r -> Set.fromList (freeVars el) <> r
              )
          ERes (_ : vs) e1 e2 -> e1 <> e2 \\ Set.fromList vs
          _ -> error "Implementation error"
      )

toMonoType :: Map Name Int -> Type Name -> MonoType
toMonoType vs =
  cata
    ( \case
        TVar s -> tVar (vs ! s)
        TUnit -> tUnit
        TBool -> tBool
        TInt -> tInt
        TFloat -> tFloat
        TDouble -> tDouble
        TChar -> tChar
        TString -> tString
        TCon con ts -> tCon con ts
        TArr t1 t2 -> tArr t1 t2
        TRec row -> tRec (bimapRow (toMonoType vs) (vs !) row)
    )

toScheme :: Name -> [Int] -> MonoType -> Scheme
toScheme prefix vars = Scheme <<< go
  where
    names =
      Map.fromList (varSequence prefix vars)
    go =
      cata
        ( \case
            TVar n -> tVar (names ! n)
            TUnit -> tUnit
            TBool -> tBool
            TInt -> tInt
            TFloat -> tFloat
            TDouble -> tDouble
            TChar -> tChar
            TString -> tString
            TCon con ts -> tCon con ts
            TArr t1 t2 -> tArr t1 t2
            TRec row -> tRec (bimapRow go (names !) row)
        )

mapTypes :: (s -> t) -> Expr s s a1 a2 -> Expr t t a1 a2
mapTypes f =
  cata
    ( \case
        EVar (t, v) -> eVar (f t, v)
        ECon (t, c) -> eCon (f t, c)
        ELit lit -> eLit lit
        EIf e1 e2 e3 -> eIf e1 e2 e3
        ELet (t, a) e1 e2 -> eLet (f t, a) e1 e2
        ELam a args e1 -> eLam a (fmap (first f) args) e1
        EApp t fun as -> eApp (f t) fun as
        ECall a (t, fun) as -> eCall_ a (f t, fun) as
        EOp1 (t, op1) e1 -> eOp1 (f t, op1) e1
        EOp2 (t, op2) e1 e2 -> eOp2 (f t, op2) e1 e2
        EPat e1 cs -> ePat e1 ((first . fmap . first) f <$> cs)
        ERec r -> eRec (mapRowTypes r)
        ERes fs e1 e2 -> eRes (first f <$> fs) e1 e2
    )
  where
    mapRowTypes =
      cata
        ( \case
            RNil -> rNil
            RVar (t, v) -> rVar (f t, v)
            RExt name el row -> rExt name (mapTypes f el) row
        )

boundVars :: Type Name -> Set Name
boundVars =
  cata
    ( \case
        TVar s -> Set.singleton s
        TCon _ ts -> Set.unions ts
        TArr t1 t2 -> Set.union t1 t2
        TRec row ->
          (`cata` row)
            ( \case
                RExt _ r a -> Set.union (boundVars r) a
                _ -> mempty
            )
        _ -> mempty
    )

{-# INLINE foldType #-}
foldType :: Type v -> [Type v] -> Type v
foldType = foldr tArr

{-# INLINE foldType1 #-}
foldType1 :: [Type v] -> Type v
foldType1 = foldr1 tArr

{-# INLINE insertArgs #-}
insertArgs :: [(t, Name)] -> Environment t -> Environment t
insertArgs = Env.inserts . (swap <$>)

{-# INLINE emptyProgram #-}
emptyProgram :: (Ord t) => Program t a
emptyProgram = Program mempty

{-# INLINE modifyProgram #-}
modifyProgram ::
  (MonadState (r, Program t a) m) =>
  (Map (Label Scheme) (Definition t a) -> Map (Label Scheme) (Definition t a)) ->
  m ()
modifyProgram = modify . second . over Program

{-# INLINE insertDef #-}
insertDef ::
  (MonadState (r, Program t a) m) =>
  Label Scheme ->
  Definition t a ->
  m ()
insertDef = modifyProgram <$$> Map.insert

{-# INLINE forEachDef #-}
forEachDef ::
  (Monad m) =>
  Program t a ->
  (Label Scheme -> Definition t a -> m b) ->
  m [b]
forEachDef (Program p) = forM (Map.toList p) . uncurry

{-# INLINE foldDefsM #-}
foldDefsM ::
  (Monad m) =>
  ((Label Scheme, Definition t a) -> r -> m r) ->
  r ->
  Program t a ->
  m r
foldDefsM f a = foldrM f a . Map.toList . unpack

{-# INLINE programMap #-}
programMap ::
  (Label Scheme -> Definition t1 a1 -> Definition t2 a2) ->
  Program t1 a1 ->
  Program t2 a2
programMap = over Program . Map.mapWithKey

{-# INLINE programFor #-}
programFor ::
  Program t1 a1 ->
  (Label Scheme -> Definition t1 a1 -> Definition t2 a2) ->
  Program t2 a2
programFor = flip programMap

programMapM ::
  (Monad m) =>
  (Label Scheme -> Definition t1 a1 -> m (Definition t2 a2)) ->
  Program t1 a1 ->
  m (Program t2 a2)
programMapM f = Program <$$> Map.traverseWithKey f . unpack

{-# INLINE programForM #-}
programForM ::
  (Monad m) =>
  Program t1 a1 ->
  (Label Scheme -> Definition t1 a1 -> m (Definition t2 a2)) ->
  m (Program t2 a2)
programForM = flip programMapM

{-# INLINE tUnit #-}
tUnit :: Type v
tUnit = embed TUnit

{-# INLINE tBool #-}
tBool :: Type v
tBool = embed TBool

{-# INLINE tInt #-}
tInt :: Type v
tInt = embed TInt

{-# INLINE tFloat #-}
tFloat :: Type v
tFloat = embed TFloat

{-# INLINE tDouble #-}
tDouble :: Type v
tDouble = embed TDouble

{-# INLINE tArr #-}
tArr :: Type v -> Type v -> Type v
tArr = embed2 TArr

infixr 1 `tArr`

{-# INLINE (~>) #-}
(~>) :: Type v -> Type v -> Type v
(~>) = tArr

infixr 1 ~>

{-# INLINE tCon #-}
tCon :: Name -> [Type v] -> Type v
tCon = embed2 TCon

{-# INLINE tVar #-}
tVar :: v -> Type v
tVar = embed1 TVar

{-# INLINE tRec #-}
tRec :: Row (Type v) v -> Type v
tRec = embed1 TRec

{-# INLINE tChar #-}
tChar :: Type v
tChar = embed TChar

{-# INLINE tString #-}
tString :: Type v
tString = embed TString

{-# INLINE rNil #-}
rNil :: Row e v
rNil = embed RNil

{-# INLINE rVar #-}
rVar :: v -> Row e v
rVar = embed1 RVar

{-# INLINE rExt #-}
rExt :: Name -> e -> Row e v -> Row e v
rExt = embed3 RExt

{-# INLINE eOp1 #-}
eOp1 :: (t, Op1) -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eOp1 = embed2 EOp1

{-# INLINE eOp2 #-}
eOp2 :: (t, Op2) -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eOp2 = embed3 EOp2

{-# INLINE eVar #-}
eVar :: Label t -> Expr t a0 a1 a2
eVar = embed1 EVar

{-# INLINE eCon #-}
eCon :: Label a0 -> Expr t a0 a1 a2
eCon = embed1 ECon

{-# INLINE eLit #-}
eLit :: Prim -> Expr t a0 a1 a2
eLit = embed1 ELit

{-# INLINE eIf #-}
eIf :: Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eIf = embed3 EIf

{-# INLINE eLet #-}
eLet :: Label t -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eLet = embed3 ELet

{-# INLINE eLam #-}
eLam :: a1 -> [Label t] -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eLam = embed3 ELam

{-# INLINE eApp #-}
eApp :: t1 -> Expr t0 t1 a1 a2 -> [Expr t0 t1 a1 a2] -> Expr t0 t1 a1 a2
eApp = embed3 EApp

{-# INLINE eCall #-}
eCall :: Label t -> [Expr t a0 a1 ()] -> Expr t a0 a1 ()
eCall = embed3 ECall ()

{-# INLINE eCall_ #-}
eCall_ :: a2 -> Label t -> [Expr t a0 a1 a2] -> Expr t a0 a1 a2
eCall_ = embed3 ECall

{-# INLINE ePat #-}
ePat :: Expr t a0 a1 a2 -> [Clause t (Expr t a0 a1 a2)] -> Expr t a0 a1 a2
ePat = embed2 EPat

{-# INLINE eRec #-}
eRec :: Row (Expr t a0 a1 a2) (Label t) -> Expr t a0 a1 a2
eRec = embed1 ERec

{-# INLINE eRes #-}
eRes :: [Label t] -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eRes = embed3 ERes

{-# INLINE oAddInt #-}
oAddInt :: (Type v, Op2)
oAddInt = (tInt ~> tInt ~> tInt, OAdd)

{-# INLINE oSubInt #-}
oSubInt :: (Type v, Op2)
oSubInt = (tInt ~> tInt ~> tInt, OSub)

{-# INLINE oMulInt #-}
oMulInt :: (Type v, Op2)
oMulInt = (tInt ~> tInt ~> tInt, OMul)

{-# INLINE oEqInt #-}
oEqInt :: (Type v, Op2)
oEqInt = (tInt ~> tInt ~> tBool, OEq)

{-# INLINE oNEqInt #-}
oNEqInt :: (Type v, Op2)
oNEqInt = (tInt ~> tInt ~> tBool, ONEq)

{-# INLINE oLtInt #-}
oLtInt :: (Type v, Op2)
oLtInt = (tInt ~> tInt ~> tBool, OLt)

{-# INLINE oGtInt #-}
oGtInt :: (Type v, Op2)
oGtInt = (tInt ~> tInt ~> tBool, OGt)

{-# INLINE oLtEInt #-}
oLtEInt :: (Type v, Op2)
oLtEInt = (tInt ~> tInt ~> tBool, OLtE)

{-# INLINE oGtEInt #-}
oGtEInt :: (Type v, Op2)
oGtEInt = (tInt ~> tInt ~> tBool, OGtE)
