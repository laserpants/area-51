{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Pong.Lang where

import Control.Monad.State
import Control.Newtype.Generics
import Data.Char (isUpper)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple (swap)
import Data.Tuple.Extra (first, second)
import Pong.Data
import Pong.Util (Map, Name, Void, cata, embed, embed1, embed2, embed3, embed4, para, project, without, (!), (<#>), (<$$>), (<<<), (>>>))
import Pong.Util.Env (Environment (..))
import qualified Pong.Util.Env as Env

mapRow :: (e -> f) -> Row e r -> Row f r
mapRow f =
  cata $ \case
    RNil -> rNil
    RVar v -> rVar v
    RExt name elem row -> rExt name (f elem) row

mapRowM :: (Monad m) => (e -> m f) -> Row e r -> m (Row f r)
mapRowM f =
  cata $ \case
    RNil -> pure rNil
    RVar v -> pure (rVar v)
    RExt name elem row -> rExt name <$> f elem <*> row

normalizeRow :: Row e r -> Row e r
normalizeRow = uncurry (flip foldRow) . unwindRow

normalizeTypeRows :: Type v q -> Type v q
normalizeTypeRows =
  cata $ \case
    TRow r -> tRow (normalizeRow r)
    t -> embed t

{-# INLINE foldRow #-}
foldRow :: Row e r -> Map Name [e] -> Row e r
foldRow = Map.foldrWithKey (flip . foldr . rExt)

{-# INLINE foldRow1 #-}
foldRow1 :: Map Name [e] -> Row e r
foldRow1 = foldRow rNil

unwindRow :: Row e r -> (Map Name [e], Row e r)
unwindRow row = (foldr (uncurry (Map.insertWith (<>))) mempty fields, leaf)
 where
  fields =
    (`para` row) $ \case
      RExt label ty (_, rest) -> (label, [ty]) : rest
      _ -> []
  leaf =
    (`cata` row) $ \case
      RExt _ _ r -> r
      t -> embed t

splitRow :: Name -> Row e r -> (e, Row e r)
splitRow name row =
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

instance FreeIn (Type Int g) where
  free =
    Set.toList
      <<< cata
        ( \case
            TVar n -> Set.singleton n
            TCon _ ts -> Set.unions ts
            TArr t1 t2 -> t1 <> t2
            TRow r -> Set.fromList (free r)
            _ -> mempty
        )

instance FreeIn (Row (Type Int g) Int) where
  free =
    Set.toList
      <<< cata
        ( \case
            RVar v -> Set.singleton v
            RExt _ expr r -> Set.fromList (free expr) <> r
            _ -> mempty
        )

instance (FreeIn f) => FreeIn [f] where
  free = concatMap free

instance (Typed t) => FreeIn (Expr t t a1 a2) where
  free = free . typeOf

instance (FreeIn e) => FreeIn (Environment e) where
  free =
    \case
      Environment env -> free (Map.elems env)

class Typed t where
  typeOf :: t -> Type Int g

instance Typed Void where
  typeOf _ = tCon "Void" []

instance (Typed g) => Typed (Type Int g) where
  typeOf =
    cata $ \case
      TUnit -> tUnit
      TBool -> tBool
      TInt -> tInt
      TFloat -> tFloat
      TDouble -> tDouble
      TChar -> tChar
      TString -> tString
      TCon con ts -> tCon con ts
      TArr t1 t2 -> tArr t1 t2
      TVar v -> tVar v
      TGen g -> typeOf g
      TRow row ->
        tRow
          ( (`cata` row) $ \case
              RNil -> rNil
              RVar v -> rVar v
              RExt name elem r -> rExt name (typeOf elem) r
          )

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
    cata $ \case
      RNil -> tRow rNil
      RVar (t, _) -> typeOf t
      RExt name elem r ->
        let TRow row = project r
         in tRow (rExt name (typeOf elem) row)

instance (Typed t, Typed a0) => Typed (Expr t a0 a1 a2) where
  typeOf =
    cata $ \case
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

instance (Typed t) => Typed (Definition t a) where
  typeOf =
    \case
      Function args (t, _) -> foldType (typeOf t) (typeOf . fst <$> toList args)

arity :: (Typed t) => t -> Int
arity = pred <<< length <<< unwindType

freeIndex :: (FreeIn t) => [t] -> Int
freeIndex ts =
  case free =<< ts of
    [] -> 0
    vs -> succ (maximum vs)

isConT :: ConT -> Type v q -> Bool
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

unwindType :: (Typed t) => t -> [Type Int g]
unwindType =
  typeOf
    >>> para
      ( \case
          TArr (t, _) (_, u) -> t : u
          t -> [embed (fst <$> t)]
      )

{-# INLINE returnType #-}
returnType :: (Typed t) => t -> Type Int g
returnType = last <<< unwindType

{-# INLINE argTypes #-}
argTypes :: (Typed t) => t -> [Type Int g]
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
              <> Set.unions (cs <#> \(_ : vs, expr) -> expr \\ Set.fromList vs)
          ERow row ->
            (`cata` row) $ \case
              RNil -> mempty
              RVar v -> Set.singleton v
              RExt _ elem r -> Set.fromList (freeVars elem) <> r
          EField (_ : vs) e1 e2 -> e1 <> e2 \\ Set.fromList vs
      )

toMonoType :: Map Name MonoType -> Type Int Name -> MonoType
toMonoType vs =
  cata $ \case
    TGen n ->
      case Map.lookup n vs of
        Nothing -> error "Implementation error"
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
    TVar v -> tVar v
    TRow row -> tRow (mapRow (toMonoType vs) row)

mapTypes :: (s -> t) -> Expr s s a1 a2 -> Expr t t a1 a2
mapTypes f =
  cata $ \case
    EVar (t, v) -> eVar (f t, v)
    ECon (t, c) -> eCon (f t, c)
    ELit lit -> eLit lit
    EIf e1 e2 e3 -> eIf e1 e2 e3
    ELet (t, a) e1 e2 -> eLet (f t, a) e1 e2
    ELam a args e1 -> eLam a (fmap (first f) args) e1
    EApp t fun as -> eApp (f t) fun as
    ECall a (t, fun) as -> embed3 ECall a (f t, fun) as
    EOp1 (t, op1) e1 -> eOp1 (f t, op1) e1
    EOp2 (t, op2) e1 e2 -> eOp2 (f t, op2) e1 e2
    ECase e1 cs -> eCase e1 ((first . fmap . first) f <$> cs)
    ERow r -> eRow (mapRowTypes r)
    EField fs e1 e2 -> eField (first f <$> fs) e1 e2
 where
  mapRowTypes =
    cata $ \case
      RNil -> rNil
      RVar (t, v) -> rVar (f t, v)
      RExt name elem row -> rExt name (mapTypes f elem) row

{-# INLINE foldType #-}
foldType :: Type v q -> [Type v q] -> Type v q
foldType = foldr tArr

{-# INLINE foldType1 #-}
foldType1 :: [Type v q] -> Type v q
foldType1 = foldr1 tArr

{-# INLINE insertArgs #-}
insertArgs :: [(t, Name)] -> Environment t -> Environment t
insertArgs = Env.inserts . (swap <$>)

{-# INLINE emptyProgram #-}
emptyProgram :: Program t a
emptyProgram = Program mempty

--programTypeEnv :: (Typed t) => Program t a -> Environment MonoType
--programTypeEnv p = Env.fromList (typeOf <$$> Map.toList (unpack p))
--
-- modifyM :: (MonadState s m) => (s -> m s) -> m ()
-- modifyM f = get >>= f >>= put

modifyProgram ::
  (MonadState (s, Program t a) m) =>
  (Map Name (Definition t a) -> Map Name (Definition t a)) ->
  m ()
modifyProgram = modify . second . over Program

---- modifyProgramM ::
----      (MonadState (Int, Program a) m)
----   => (Map Name (Definition Type a) -> m (Map Name (Definition Type a)))
----   -> m ()
---- modifyProgramM f = do
----   (n, Program p) <- get
----   x <- f p
----   put (n, Program x)
----   --p <- Control.Newtype.Generics.unpack <$> get
----   --let zzz = modify f p
----   -- modify . over Program

insertDef :: (MonadState (s, Program t a) m) => Name -> Definition t a -> m ()
insertDef = modifyProgram <$$> Map.insert

---- --updateDef ::
---- --     (MonadState (Program a) m)
---- --  => (Definition Type a -> Maybe (Definition Type a))
---- --  -> Name
---- --  -> m ()
---- --updateDef = modifyProgram <$$> Map.update
----
---- --forEachDef ::
---- --     (MonadState (Program a) m)
---- --  => (Definition Type a -> m (Definition Type a))
---- --  -> m ()
---- forEachDef run = do
----   Program defs <- gets snd
----   forM_ (Map.keys defs) $ \key -> do
----     Program defs <- gets snd
----     def <- run (defs ! key)
----     insertDef key def
--
--forEachDefX (Program defs) run = do
--  --Program defs <- get
--  forM_ (Map.keys defs) $ \key -> do
--    def <- run (defs ! key)
--    insertDef key def
--
---- --lookupDef :: (MonadState (Int, Program a) m) => Name -> m (Definition Type a)
---- lookupDef key = do
----   Program defs <- gets snd
----   case defs !? key of
----     Just z -> pure z
----     Nothing ->
----       error (show defs)
----   --traceShowM defs
----   --pure (defs ! key)

{-# INLINE tUnit #-}
tUnit :: Type v q
tUnit = embed TUnit

{-# INLINE tBool #-}
tBool :: Type v q
tBool = embed TBool

{-# INLINE tInt #-}
tInt :: Type v q
tInt = embed TInt

{-# INLINE tFloat #-}
tFloat :: Type v q
tFloat = embed TFloat

{-# INLINE tDouble #-}
tDouble :: Type v q
tDouble = embed TDouble

{-# INLINE tArr #-}
tArr :: Type v q -> Type v q -> Type v q
tArr = embed2 TArr

infixr 1 `tArr`

{-# INLINE (~>) #-}
(~>) = tArr

infixr 1 ~>

{-# INLINE tCon #-}
tCon :: Name -> [Type v q] -> Type v q
tCon = embed2 TCon

{-# INLINE tVar #-}
tVar :: v -> Type v q
tVar = embed1 TVar

{-# INLINE tRow #-}
tRow :: Row (Type v q) Int -> Type v q
tRow = embed1 TRow

{-# INLINE tChar #-}
tChar :: (Type v q)
tChar = embed TChar

{-# INLINE tString #-}
tString :: (Type v q)
tString = embed TString

{-# INLINE tGen #-}
tGen :: q -> Type v q
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

{-# INLINE eCase #-}
eCase :: Expr t a0 a1 a2 -> [([Label t], Expr t a0 a1 a2)] -> Expr t a0 a1 a2
eCase = embed2 ECase

{-# INLINE eRow #-}
eRow :: Row (Expr t a0 a1 a2) (Label t) -> Expr t a0 a1 a2
eRow = embed1 ERow

eField :: [Label t] -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eField = embed3 EField

{-# INLINE oAddInt #-}
oAddInt :: (Type v q, Op2)
oAddInt = (tInt ~> tInt ~> tInt, OAdd)

{-# INLINE oSubInt #-}
oSubInt :: (Type v q, Op2)
oSubInt = (tInt ~> tInt ~> tInt, OSub)

{-# INLINE oMulInt #-}
oMulInt :: (Type v q, Op2)
oMulInt = (tInt ~> tInt ~> tInt, OMul)

{-# INLINE oEqInt #-}
oEqInt :: (Type v q, Op2)
oEqInt = (tInt ~> tInt ~> tBool, OEq)

{-# INLINE oLtInt #-}
oLtInt :: (Type v q, Op2)
oLtInt = (tInt ~> tInt ~> tBool, OLt)

{-# INLINE oGtInt #-}
oGtInt :: (Type v q, Op2)
oGtInt = (tInt ~> tInt ~> tBool, OGt)

{-# INLINE oLtEInt #-}
oLtEInt :: (Type v q, Op2)
oLtEInt = (tInt ~> tInt ~> tBool, OLtE)

{-# INLINE oGtEInt #-}
oGtEInt :: (Type v q, Op2)
oGtEInt = (tInt ~> tInt ~> tBool, OGtE)
