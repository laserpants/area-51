{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Pong.Lang where

import Control.Monad.State
import Control.Newtype.Generics (over, unpack)
import Data.Char (isUpper)
import Data.List (nub)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple (swap)
import Data.Tuple.Extra (first, second)
import Pong.Data
import Pong.Util (Map, Name, Void, cata, embed, embed1, embed2, embed3, embed4, para, project, varSequence, without, (!), (<$$>), (<&>), (<<<), (>>>))
import Pong.Util.Env (Environment (..))
import qualified Pong.Util.Env as Env

mapRow :: (e -> f) -> Row e r -> Row f r
mapRow f =
  cata $
    \case
      RNil -> rNil
      RVar v -> rVar v
      RExt name elem row -> rExt name (f elem) row

mapRowM :: (Monad m) => (e -> m f) -> Row e r -> m (Row f r)
mapRowM f =
  cata $
    \case
      RNil -> pure rNil
      RVar v -> pure (rVar v)
      RExt name elem row -> rExt name <$> f elem <*> row

normalizeRow :: Row e r -> Row e r
normalizeRow = uncurry (flip foldRow) . unwindRow

normalizeTypeRows :: Type v s -> Type v s
normalizeTypeRows =
  cata $
    \case
      TRow r -> tRow (normalizeRow r)
      t -> embed t

{-# INLINE foldRow #-}
foldRow :: Row e r -> Map Name [e] -> Row e r
foldRow = Map.foldrWithKey (flip . foldr . rExt)

{-# INLINE foldRow1 #-}
foldRow1 :: Map Name [e] -> Row e r
foldRow1 = foldRow rNil

unwindRow :: Row e r -> (Map Name [e], Row e r)
unwindRow row = (toMap fields, leaf)
  where
    toMap = foldr (uncurry (Map.insertWith (<>))) mempty
    paraRow = (`para` row)
    fields =
      paraRow
        ( \case
            RExt label ty (_, rest) -> (label, [ty]) : rest
            _ -> []
        )
    cataRow = (`cata` row)
    leaf =
      cataRow
        ( \case
            RExt _ _ r -> r
            r -> embed r
        )

restrictRow :: Name -> Row e r -> (e, Row e r)
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

class FreeIn f where
  free :: f -> [Int]

instance FreeIn Scheme where
  free _ = []

instance FreeIn (Type Int s) where
  free =
    nub
      <<< cata
        ( \case
            TVar n -> [n]
            TCon _ ts -> join ts
            TArr t1 t2 -> t1 <> t2
            TRow row -> free row
            _ -> []
        )

instance FreeIn (Row (Type Int s) Int) where
  free =
    nub
      <<< cata
        ( \case
            RVar v -> [v]
            RExt _ t r -> free t <> r
            _ -> []
        )

instance (Foldable f, Functor f, FreeIn a) => FreeIn (f a) where
  free = concatMap free

class Typed t where
  typeOf :: t -> MonoType

instance (Typed t) => FreeIn (Expr t t a1 a2) where
  free = free . typeOf

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
          RNil -> tRow rNil
          RVar (t, _) -> typeOf t
          RExt name elem r ->
            let TRow row = project r
             in tRow (rExt name (typeOf elem) row)
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
          EApp t fun as -> typeOf t
          ECall _ (t, _) as -> foldType1 (drop (length as) (unwindType t))
          EOp1 (t, _) _ -> returnType t
          EOp2 (t, _) _ _ -> returnType t
          ECase _ [] -> error "Empty case statement"
          ECase _ cs -> head (snd <$> cs)
          ERow r -> typeOf r
          EField _ _ e -> e
      )

instance (Typed t) => Typed (Definition t a) where
  typeOf =
    \case
      Function args (t, _) ->
        foldType (typeOf t) (typeOf . fst <$> toList args)
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

isConT :: ConT -> Type v s -> Bool
isConT con =
  project >>> \case
    TArr{}
      | ArrT == con -> True
    TVar{}
      | VarT == con -> True
    TRow{}
      | RowT == con -> True
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
    ERow{}
      | RowE == con -> True
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
          ECase e1 cs ->
            e1
              <> Set.unions (cs <&> \(_ : vs, expr) -> expr \\ Set.fromList vs)
          ERow row ->
            (`cata` row)
              ( \case
                  RNil -> mempty
                  RVar v -> Set.singleton v
                  RExt _ elem r -> Set.fromList (freeVars elem) <> r
              )
          EField (_ : vs) e1 e2 -> e1 <> e2 \\ Set.fromList vs
      )

toMonoType :: Map Name MonoType -> Type Void Name -> MonoType
toMonoType vs =
  cata
    ( \case
        TGen s ->
          case Map.lookup s vs of
            Nothing -> tVar 0 -- error "Implementation error"
            -- Nothing -> error "Implementation error"
            Just t -> t
        TUnit -> tUnit
        TBool -> tBool
        TInt -> tInt
        TFloat -> tFloat
        TDouble -> tDouble
        TChar -> tChar
        TString -> tString
        TCon con ts -> tCon con ts
        TArr t1 t2 -> tArr t1 t2
        TRow row -> tRow (mapRow (toMonoType vs) row)
    )

toScheme :: Name -> [Int] -> MonoType -> Scheme
toScheme prefix vars = Scheme <<< go
  where
    names =
      Map.fromList (varSequence prefix vars)
    go =
      cata
        ( \case
            TVar n -> tGen (names ! n)
            TUnit -> tUnit
            TBool -> tBool
            TInt -> tInt
            TFloat -> tFloat
            TDouble -> tDouble
            TChar -> tChar
            TString -> tString
            TCon con ts -> tCon con ts
            TArr t1 t2 -> tArr t1 t2
            TRow row -> tRow (mapRow go row)
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
        ECase e1 cs -> eCase e1 ((first . fmap . first) f <$> cs)
        ERow r -> eRow (mapRowTypes r)
        EField fs e1 e2 -> eField (first f <$> fs) e1 e2
    )
  where
    mapRowTypes =
      cata
        ( \case
            RNil -> rNil
            RVar (t, v) -> rVar (f t, v)
            RExt name elem row -> rExt name (mapTypes f elem) row
        )

boundVars :: Type v Name -> Set Name
boundVars =
  cata
    ( \case
        TGen s -> Set.singleton s
        TCon _ ts -> Set.unions ts
        TArr t1 t2 -> Set.union t1 t2
        TRow row ->
          (`cata` row)
            ( \case
                RExt _ r a -> Set.union (boundVars r) a
                _ -> mempty
            )
        _ -> mempty
    )

{-# INLINE foldType #-}
foldType :: Type v s -> [Type v s] -> Type v s
foldType = foldr tArr

{-# INLINE foldType1 #-}
foldType1 :: [Type v s] -> Type v s
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

insertDef ::
  (MonadState (r, Program t a) m) =>
  Label Scheme ->
  Definition t a ->
  m ()
insertDef = modifyProgram <$$> Map.insert

{-# INLINE tUnit #-}
tUnit :: Type v s
tUnit = embed TUnit

{-# INLINE tBool #-}
tBool :: Type v s
tBool = embed TBool

{-# INLINE tInt #-}
tInt :: Type v s
tInt = embed TInt

{-# INLINE tFloat #-}
tFloat :: Type v s
tFloat = embed TFloat

{-# INLINE tDouble #-}
tDouble :: Type v s
tDouble = embed TDouble

{-# INLINE tArr #-}
tArr :: Type v s -> Type v s -> Type v s
tArr = embed2 TArr

infixr 1 `tArr`

{-# INLINE (~>) #-}
(~>) = tArr

infixr 1 ~>

{-# INLINE tCon #-}
tCon :: Name -> [Type v s] -> Type v s
tCon = embed2 TCon

{-# INLINE tVar #-}
tVar :: v -> Type v s
tVar = embed1 TVar

{-# INLINE tRow #-}
tRow :: Row (Type v s) Int -> Type v s
tRow = embed1 TRow

{-# INLINE tChar #-}
tChar :: Type v s
tChar = embed TChar

{-# INLINE tString #-}
tString :: Type v s
tString = embed TString

{-# INLINE tGen #-}
tGen :: s -> Type v s
tGen = embed1 TGen

{-# INLINE rNil #-}
rNil :: Row e r
rNil = embed RNil

{-# INLINE rVar #-}
rVar :: r -> Row e r
rVar = embed1 RVar

{-# INLINE rExt #-}
rExt :: Name -> e -> Row e r -> Row e r
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

{-# INLINE eCase #-}
eCase :: Expr t a0 a1 a2 -> [([Label t], Expr t a0 a1 a2)] -> Expr t a0 a1 a2
eCase = embed2 ECase

{-# INLINE eRow #-}
eRow :: Row (Expr t a0 a1 a2) (Label t) -> Expr t a0 a1 a2
eRow = embed1 ERow

eField :: [Label t] -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eField = embed3 EField

{-# INLINE oAddInt #-}
oAddInt :: (Type v s, Op2)
oAddInt = (tInt ~> tInt ~> tInt, OAdd)

{-# INLINE oSubInt #-}
oSubInt :: (Type v s, Op2)
oSubInt = (tInt ~> tInt ~> tInt, OSub)

{-# INLINE oMulInt #-}
oMulInt :: (Type v s, Op2)
oMulInt = (tInt ~> tInt ~> tInt, OMul)

{-# INLINE oEqInt #-}
oEqInt :: (Type v s, Op2)
oEqInt = (tInt ~> tInt ~> tBool, OEq)

{-# INLINE oNEqInt #-}
oNEqInt :: (Type v s, Op2)
oNEqInt = (tInt ~> tInt ~> tBool, ONEq)

{-# INLINE oLtInt #-}
oLtInt :: (Type v s, Op2)
oLtInt = (tInt ~> tInt ~> tBool, OLt)

{-# INLINE oGtInt #-}
oGtInt :: (Type v s, Op2)
oGtInt = (tInt ~> tInt ~> tBool, OGt)

{-# INLINE oLtEInt #-}
oLtEInt :: (Type v s, Op2)
oLtEInt = (tInt ~> tInt ~> tBool, OLtE)

{-# INLINE oGtEInt #-}
oGtEInt :: (Type v s, Op2)
oGtEInt = (tInt ~> tInt ~> tBool, OGtE)
