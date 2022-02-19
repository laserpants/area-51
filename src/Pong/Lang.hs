{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Pong.Lang
  where
--  ( module Pong.Data
--  , module Pong.Util
--  , FreeIn(..)
--  , Typed
--  , HasArity
--  , annotate
--  , unwindType
--  , insertArgs
--  , typeOf
--  , foldType
--  , isCon
--  , isTCon
--  , arity
--  , returnTypeOf
--  , argTypes
--  , funArgs
--  , constructors
--  , insertDefinition
--  , emptyProgram
----  , printProgram
--  , tUnit
--  , tBool
--  , tInt32
--  , tInt64
--  , tFloat
--  , tDouble
--  , tVar
--  , tArr
--  , tData
----  , tOpaque
--  , (~>)
--  , var
--  , lit
--  , if_
--  , lam
--  , let_
--  , app
--  , call_
--  , op2
--  , case_
--  ) where
--
--import Data.Map.Strict (Map)
--import Data.Tuple (swap)
--import Data.Tuple.Extra (first, dupe)
--import Data.Void
import Data.List (nub)
import Pong.Data
import Pong.Util
--import TextShow
--import qualified Data.Map.Strict as Map
import qualified Pong.Util.Env as Env
import Data.Tuple (swap)
import qualified Data.Map.Strict as Map

class FreeIn t a where
  free :: a -> [Label t]

instance (Eq t) => FreeIn t Type where
  free = nub . cata (\case
    TVar t -> undefined
    _ -> [])

instance FreeIn t a => FreeIn t [a] where
  free = (free =<<)

instance (Eq t) => FreeIn t (Row (Expr t a0 a1 a2) (Label t)) where
  free =
    cata $ \case
      RNil -> []
      RVar v -> [v]
      RExt _ expr r -> free expr <> r

instance (Eq t) => FreeIn t (Expr t a0 a1 a2) where
  free =
    cata $ \case
      EVar v -> [v]
      ECon c -> [c]
      ELit lit -> []
      EIf e1 e2 e3 -> e1 <> e2 <> e3
      ELet bind e1 e2 -> (e1 <> e2) `without` [bind]
      ELam _ args expr -> expr `without` args
      EApp _ fun args -> fun <> concat args
      ECall _ fun args -> fun : concat args
      EOp2 op e1 e2 -> e1 <> e2
      ECase e1 cs -> e1 <> (cs >>= free . uncurry Clause)
      ERow row -> free row

instance (Eq t) => FreeIn t (Clause (Label t)) where
  free =
    \case
      Clause (_:vs) expr -> expr `without` vs

instance (FreeIn t e) => FreeIn t (Environment e) where
  free = 
    \case
      Env env -> free (Map.elems env)

--instance (FreeIn t a) => FreeIn t (Signature s (Type, a)) where
--  free = free . snd . body
--
--instance (FreeIn t a) => FreeIn t (Map Name (Signature s (Type, a))) where
--  free = concatMap free . Map.elems

class Typed a where
  typeOf :: a -> Type

instance Typed Type where
  typeOf = id

instance Typed Literal where
  typeOf =
    \case
      LBool {} -> tBool
      LInt32 {} -> tInt32
      LInt64 {} -> tInt64
      LFloat {} -> tFloat
      LDouble {} -> tDouble
--      LChar {} -> tChar
--      LString {} -> tString
      LUnit -> tUnit

instance Typed Op2 where
  typeOf =
    \case
      OEqInt32 -> tInt32 ~> tInt32 ~> tBool
      OAddInt32 -> tInt32 ~> tInt32 ~> tInt32
      OSubInt32 -> tInt32 ~> tInt32 ~> tInt32
      OMulInt32 -> tInt32 ~> tInt32 ~> tInt32
      OAddFloat -> tFloat ~> tFloat ~> tFloat
      OMulFloat -> tFloat ~> tFloat ~> tFloat
      OSubFloat -> tFloat ~> tFloat ~> tFloat
      ODivFloat -> tFloat ~> tFloat ~> tFloat
      OAddDouble -> tDouble ~> tDouble ~> tDouble
      OMulDouble -> tDouble ~> tDouble ~> tDouble
      OSubDouble -> tDouble ~> tDouble ~> tDouble
      ODivDouble -> tDouble ~> tDouble ~> tDouble

instance (Typed t) => Typed (Row (Expr t a0 a1 a2) (Label t)) where
  typeOf =
    cata $ \case
      RNil -> tRow rNil
      RVar (t, _) -> typeOf t
      RExt name expr r ->
        let TRow row = project r 
         in tRow (rExt name (typeOf expr) row)

instance (Typed t, Typed (Row (Expr t a0 a1 a2) (Label t))) => Typed (Expr t a0 a1 a2) where
  typeOf =
    cata $ \case
      EVar (t, _) -> typeOf t
      ECon (t, _) -> typeOf t
      ELit lit -> typeOf lit
      EIf _ _ e3 -> e3
      ELam _ args expr -> foldType expr (typeOf . fst <$> args)
      EApp _ fun as -> tapp fun as
      ECall _ (t, _) as -> tapp t as
      EOp2 op _ _ -> returnType op
      ECase _ [] -> error "Empty case statement"
      ECase _ cs -> head (snd <$> cs)
      ERow r -> typeOf r
    where
      tapp t as = foldType1 (drop (length as) (unwindType t))

--instance (Typed t) => Typed (Expr t a0 a1 a2 a3) where
--  typeOf =
--    cata $ \case
--      EVar (t, _) -> typeOf t
--      ELit lit -> typeOf lit
--      EIf _ _ e3 -> e3
--      ELam _ args expr -> foldType expr (typeOf . fst <$> args)
--      ELet _ _ _ e2 -> e2
--      EApp _ fun as -> tapp fun as
--      ECall _ (t, _) as -> tapp t as
--      EOp2 op _ _ -> returnTypeOf op
--      ECase _ [] -> error "Empty case statement"
--      ECase _ cs -> head (snd <$> cs)
--    where
--      tapp t as = foldType1 (drop (length as) (unwindType t))
--
--instance Typed (Definition a) where
--  typeOf =
--    \case
--      Function Signature {..} -> foldType (fst body) (fst <$> arguments)
--      External Signature {..} -> foldType body arguments
--      Constant lit -> typeOf lit
--      Data name _ -> tData name

class HasArity a where
  arity :: a -> Int

instance HasArity Type where
  arity = pred <<< length <<< unwindType

instance (Typed t) => HasArity (Expr t a0 a1 a2) where
  arity = arity . typeOf

--instance HasArity (Definition a) where
--  arity =
--    \case
--      Function Signature {..} -> length arguments
--      External Signature {..} -> length arguments
--      _ -> 0

isTCon :: TCon -> Type -> Bool
isTCon con =
  project >>> \case
    TArr {}
      | ArrT == con -> True
    TVar {}
      | VarT == con -> True
    _ -> False

isCon :: Con -> Expr t a0 a1 a2 -> Bool
isCon con =
  project >>> \case
    EVar {}
      | VarE == con -> True
    ELit {}
      | LitE == con -> True
    ELam {}
      | LamE == con -> True
    _ -> False

--annotate :: (Typed a) => a -> (Type, a)
--annotate = first typeOf . dupe

unwindType :: (Typed t) => t -> [Type]
unwindType =
  typeOf >>> 
    para (\case
      TArr (t, _) (_, u) -> t : u
      t -> [embed (fst <$> t)])

{-# INLINE returnType #-}
returnType :: (Typed t) => t -> Type
returnType = last <<< unwindType 

{-# INLINE argTypes #-}
argTypes :: (Typed t) => t -> [Type]
argTypes = init <<< unwindType

--funArgs :: Definition a -> [Label Type]
--funArgs =
--  \case
--    Function Signature {..} -> arguments
--    External Signature {..} -> showt <$$> zip arguments [0 :: Int ..]
--    _ -> []
--
--constructors :: Definition a -> [Constructor]
--constructors =
--  \case
--    Data _ cs -> cs
--    _ -> []

{-# INLINE foldType #-}
foldType :: Type -> [Type] -> Type
foldType = foldr tArr

{-# INLINE foldType1 #-}
foldType1 :: [Type] -> Type
foldType1 = foldr1 tArr

{-# INLINE insertArgs #-}
insertArgs :: [(Type, Name)] -> Environment Type -> Environment Type
insertArgs = Env.inserts . (swap <$>)

--{-# INLINE emptyProgram #-}
--emptyProgram :: Program
--emptyProgram = Program 0 mempty
--
--insertDefinition :: Name -> Definition Ast -> Program -> Program
--insertDefinition name def =
--  \case
--    Program {definitions, ..} ->
--      Program {definitions = Map.insert name def definitions, ..}
--
----printProgram :: Program -> IO ()
----printProgram Program {..} = sequence_ (Map.mapWithKey (curry print) definitions)

--{-# INLINE tVar #-}
--tVar :: Int -> Type
--tVar = embed1 TVar
--
--{-# INLINE tArr #-}
--tArr :: Type -> Type -> Type
--tArr = embed2 TArr
--
--infixr 1 `tArr`
--
--{-# INLINE (~>) #-}
--(~>) = tArr
--
--infixr 1 ~>
--
--{-# INLINE tData #-}
--tData :: Name -> Type
--tData = embed1 TData
--
----{-# INLINE tOpaque #-}
----tOpaque :: Type
----tOpaque = embed TOpaque
--
--{-# INLINE var #-}
--var :: Label t -> Expr t a0 a1 a2 a3
--var = embed1 EVar
--
--{-# INLINE lit #-}
--lit :: Literal -> Expr t a0 a1 a2 a3
--lit = embed1 ELit
--
--{-# INLINE if_ #-}
--if_ :: Expr t a0 a1 a2 a3 -> Expr t a0 a1 a2 a3 -> Expr t a0 a1 a2 a3 -> Expr t a0 a1 a2 a3
--if_ = embed3 EIf
--
--{-# INLINE lam #-}
--lam :: [Label t] -> Expr t a0 () a2 a3 -> Expr t a0 () a2 a3
--lam = embed3 ELam () 
--
--{-# INLINE let_ #-}
--let_ :: Label t -> Expr t () a1 a2 a3 -> Expr t () a1 a2 a3 -> Expr t () a1 a2 a3
--let_ = embed4 ELet ()
--
--{-# INLINE app #-}
--app :: Expr t a0 a1 () a3 -> [Expr t a0 a1 () a3] -> Expr t a0 a1 () a3
--app = embed3 EApp ()
--
--{-# INLINE call_ #-}
--call_ :: Label t -> [Expr t a0 a1 a2 ()] -> Expr t a0 a1 a2 ()
--call_ = embed3 ECall ()
--
--{-# INLINE op2 #-}
--op2 :: Op2 -> Expr t a0 a1 a2 a3 -> Expr t a0 a1 a2 a3 -> Expr t a0 a1 a2 a3
--op2 = embed3 EOp2
--
--{-# INLINE case_ #-}
--case_ :: Expr t a0 a1 a2 a3 -> [([Label t], Expr t a0 a1 a2 a3)] -> Expr t a0 a1 a2 a3
--case_ = embed2 ECase

{-# INLINE tUnit #-}
tUnit :: Type
tUnit = embed TUnit

{-# INLINE tBool #-}
tBool :: Type
tBool = embed TBool

{-# INLINE tInt32 #-}
tInt32 :: Type
tInt32 = embed TInt32

{-# INLINE tInt64 #-}
tInt64 :: Type
tInt64 = embed TInt64

{-# INLINE tFloat #-}
tFloat :: Type
tFloat = embed TFloat

{-# INLINE tDouble #-}
tDouble :: Type
tDouble = embed TDouble

{-# INLINE tArr #-}
tArr :: Type -> Type -> Type
tArr = embed2 TArr

{-# INLINE tCon #-}
tCon :: Name -> [Type] -> Type
tCon = embed2 TCon

infixr 1 `tArr`

{-# INLINE (~>) #-}
(~>) = tArr

infixr 1 ~>

{-# INLINE tVar #-}
tVar :: Int -> Type
tVar = embed1 TVar

{-# INLINE tGen #-}
tGen :: Int -> Type
tGen = embed1 TGen

{-# INLINE tRow #-}
tRow :: Row Type Name -> Type
tRow = embed1 TRow

{-# INLINE rNil #-}
rNil :: Row r v
rNil = embed RNil

{-# INLINE rVar #-}
rVar :: v -> Row r v
rVar = embed1 RVar

{-# INLINE rExt #-}
rExt :: Name -> r -> Row r v -> Row r v
rExt = embed3 RExt

{-# INLINE eOp2 #-}
eOp2 :: Op2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eOp2 = embed3 EOp2

{-# INLINE eVar #-}
eVar :: Label t -> Expr t a0 a1 a2
eVar = embed1 EVar

{-# INLINE eCon #-}
eCon :: Label t -> Expr t a0 a1 a2
eCon = embed1 ECon

{-# INLINE eLit #-}
eLit :: Literal -> Expr t a0 a1 a2 
eLit = embed1 ELit 

{-# INLINE eIf #-}
eIf :: Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 
eIf = embed3 EIf

{-# INLINE eLet #-}
eLet :: Label t -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 
eLet = embed3 ELet 

{-# INLINE eLam #-}
eLam :: [Label t] -> Expr t () a1 a2 -> Expr t () a1 a2
eLam = embed3 ELam ()

{-# INLINE eApp #-}
eApp :: Expr t a0 () a2 -> [Expr t a0 () a2] -> Expr t a0 () a2
eApp = embed3 EApp ()

{-# INLINE eCall #-}
eCall :: Label t -> [Expr t a0 a1 ()] -> Expr t a0 a1 ()
eCall = embed3 ECall ()

{-# INLINE eCase #-}
eCase :: Expr t a0 a1 a2 -> [([Label t], Expr t a0 a1 a2)] -> Expr t a0 a1 a2 
eCase = embed2 ECase

{-# INLINE eRow #-}
eRow :: Row (Expr t a0 a1 a2) (Label t) -> Expr t a0 a1 a2 
eRow = embed1 ERow
