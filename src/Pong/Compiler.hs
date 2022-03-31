{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pong.Compiler where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Control.Newtype.Generics as N
import Data.Function ((&))
import Data.List (partition, nub)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty, (!!), fromList, toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (first, second, swap)
import Data.Void (Void)
import Debug.Trace
import Pong.Data
import Pong.Eval
import Pong.Lang
import Pong.TypeChecker (Substitution, apply, unify, runTypeChecker')
import Pong.Util
import Prelude hiding ((!!))
import TextShow (showt)

-- from:
--   lam(a) => lam(b) => b
--
-- to: 
--   lamb[a, b] => b
combineLambdas :: Expr t a0 a1 a2 -> Expr t a0 a1 a2
combineLambdas =
  cata $ \case
    ELam t xs (Fix (ELam _ ys expr)) -> eLam t (xs <> ys) expr
    e -> embed e

-- from:
--   (g(x))(y)
--
-- to: 
--   g(x, y)
combineApps :: Expr t Type a1 a2 -> Expr t Type a1 a2
combineApps =
  cata $ \case
    EApp _ (Fix (EApp t expr xs)) ys ->
      let t1 = foldType1 (drop (length ys) (unwindType t))
       in eApp t1 expr (xs <> ys)
    e -> embed e

-- from:
--   foo = lam(a) => lam(b) => b
--   baz(x) = lam(a) => lam(b) => b
--
-- to: 
--   foo(a, b) = b
--   baz(x, a, b) = b
hoistTopLambdas ::
     Definition (Label t) (Expr t a0 () a2)
  -> Definition (Label t) (Expr t a0 () a2)
hoistTopLambdas =
  \case
    Function args (t, expr)
      | isCon LamE expr -> combine t (toList args) expr
    Constant (t, expr)
      | isCon LamE expr -> combine t [] expr
    def -> def
  where
    combine t as =
      project . combineLambdas >>> \case
        ELam _ bs expr -> Function (fromList (as <> bs)) (returnType t, expr)
        _ -> error "Implementation error (1)"

-- from:
--   plus(x, y) = x + y
--   foo(x) = plus(x)      
--
-- to: 
--   plus(x, y) = x + y
--   foo(x, v_0) = plus(x, v_0)
--
fillParams ::
     Definition (Label Type) (Expr Type Type () Void)
  -> Definition (Label Type) (Expr Type Type () Void)
fillParams = hoistTopLambdas <<< fmap (convertClosuresT <<< fillExprParams <<< marshall)

marshall :: TypedExpr -> TypedExpr
marshall =
  cata $ \case
    ELam t args (Fix (ELam _ args' e1)) ->
      eLam t (args <> args') e1
    EApp t fun args ->
      case project fun of
        EIf e1 e2 e3 ->
          eIf e1 (app e2 args) (app e3 args)
        ECase e1 cs -> 
          eCase e1 ((`app` args) <$$> cs)
        EField fs e1 e2 ->
          eField fs e1 (app e2 args)
        ELet e1 e2 e3 ->
          eLet e1 e2 (app e3 args)
        EApp _ e1 args' ->
          app e1 (args' <> args)
        _ -> eApp t fun args
    e ->
      embed e
  where
    app e args = eApp (foldType1 (drop (length args) (unwindType e))) e args 

--
-- from:
--   let
--     g =
--      f(1)
--   in
--     g(2)
--
-- to:
--   let
--     g =
--       (\v_0 -> f(1, v_0))
--   in
--     g(2)
--
fillExprParams :: TypedExpr -> TypedExpr
fillExprParams =
  para $ \case
    EVar (t, fun)
      | arity t > 0 ->
        let extra = argTypes t `zip` names
         in eLam () extra (eApp (returnType t) (eVar (t, fun)) (extra <#> eVar))
    EApp t fun@(expr, _) args
      | arity t > 0 ->
        let extra = drop (length args) (argTypes expr) `zip` names
            t1 = foldType1 (drop (length extra) (unwindType t))
         in eLam
              ()
              extra
              (eApp t1 (varCon fun) ((args <#> varCon) <> (extra <#> eVar)))
      | otherwise -> eApp t (varCon fun) (args <#> varCon)
    e -> embed (e <#> snd)
  where
    names = [".v" <> showt n | n <- [0 :: Int ..]]
    varCon :: (TypedExpr, TypedExpr) -> TypedExpr
    varCon (e1, e2) =
      case project e1 of
        EVar v -> eVar v
        _ -> e2

--
-- from:
--   lam(x) => x + h
--
-- to:
--   (lam[h, x] => x + h)(h)
--
convertClosures :: Expr Type () () a2 -> Expr Type () () a2
convertClosures =
  cata $ \case
    ELam _ args expr -> do
      let extra = freeVars expr `without` args
          lambda = eLam () (extra <> args) expr
      case extra of
        [] -> lambda
        _ -> eApp () lambda (eVar <$> extra)
    expr -> embed expr

convertClosuresT :: TypedExpr -> TypedExpr
convertClosuresT =
  cata $ \case
    ELam _ args expr -> do
      let extra = freeVars expr `without` args
          lambda = eLam () (extra <> args) expr
      case extra of
        [] -> lambda
        _ -> eApp (appType (length extra) lambda) lambda (eVar <$> extra)
    expr -> embed expr
  where
    appType n t = foldType1 (drop n (unwindType t))

--
-- from:
--   let x = y in x + z
--
-- to:
--   y + z
--
replaceVarLets :: Expr Type a0 a1 a2 -> (Expr Type a0 a1 a2, [(Name, Name)])
replaceVarLets input = (substMany subs e, subs)
  where
    (e, subs) =
      input &
      (runWriter <<<
       cata
         (\case
            ELet (t, name) expr body ->
              expr >>=
              (project >>> \case
                 EVar (_, var) -> do
                   tell [(name, var)]
                   body
                 _ -> eLet (t, name) <$> expr <*> body)
            e -> embed <$> sequence e))

varSubst :: Name -> Name -> Expr Type a0 a1 a2 -> Expr Type a0 a1 a2
varSubst from to =
  cata $ \case
    EVar (t, v)
      | v == from -> eVar (t, to)
    e -> embed e

substMany :: [(Name, Name)] -> Expr Type a0 a1 a2 -> Expr Type a0 a1 a2
substMany subs e = foldr (uncurry varSubst) e subs

liftLambdas ::
     (MonadState (Int, Program PreAst) m) => Expr Type Type () Void -> m PreAst
--liftLambdas input = do
liftLambdas = do
  --(expr, subs) <- replaceVarLets <$> fun input
  --modifyProgram (substMany subs <$$>)
  --pure expr
  --fun input
  --where
  --  fun =
      cata $ \case
        ELam _ args expr -> do
          name <- uniqueName ".f"
          body <- expr
          let t = typeOf body
          insertDef name (Function (fromList args) (t, body))
          pure (eVar (foldType t (args <#> fst), name))
        EVar v -> pure (eVar v)
        ECon c -> pure (eCon c)
        ELit l -> pure (eLit l)
        EIf a1 a2 a3 -> eIf <$> a1 <*> a2 <*> a3
        ELet a1 a2 a3 -> eLet a1 <$> a2 <*> a3
        EOp2 op a1 a2 -> eOp2 op <$> a1 <*> a2
        ECase a1 a2 -> eCase <$> a1 <*> traverse sequence a2
        EApp t expr args -> eApp t <$> expr <*> sequence args
        ERow row -> eRow <$> mapRowM liftLambdas row
        EField a1 a2 a3 -> eField a1 <$> a2 <*> a3

uniqueName :: (MonadState (Int, Program a) m) => Name -> m Name
uniqueName prefix = do
  n <- gets fst
  modify (first succ)
  pure (prefix <> showt n)

alignCallSigns :: (MonadState (Int, Program PreAst) m) => PreAst -> m PreAst
alignCallSigns =
  cata $ \case
    EApp t fun args -> do
      f <- fun
      as <- sequence args
      case project f of
        EVar (t1, var) -> do
          --traceShowM "**** 1"
          --traceShowM var
          --def <- lookupDef var -- (if "g" == var then ".f0" else if "f" == var then ".f1" else var)
          Program defs <- gets snd
          case defs !? var of
            Nothing -> do
              pure (eApp t f as)
            Just def -> do
              let tdef = typeOf def
              case runTypeChecker' (leastFree [tdef, t1]) mempty (unify tdef t1) of
                Right sub
                  | sub /= mempty -> do
                    name <- uniqueName ".g"
                    insertDef name =<< preprocess (fmap xx123 (apply sub def))
                    pure (eApp t (eVar (t1, name)) as)
                _ -> pure (eApp t f as)
        _ -> pure (eApp t f as)
    e -> embed <$> sequence e

xx123 :: Expr Type Type Void Void -> Expr Type Type () Void
xx123 = cata $ \case
--  ELam _ a1 a2 -> eLam () a1 a2
        EVar v -> eVar v
        ECon c -> eCon c
        ELit l -> eLit l
        EIf a1 a2 a3 -> eIf a1 a2 a3
        ELet a1 a2 a3 -> eLet a1 a2 a3
        EApp a1 a2 a3 -> eApp a1 a2 a3
        EOp2 op a1 a2 -> eOp2 op a1 a2
        ECase a1 a2 -> eCase a1 a2
        ERow row -> eRow (mapRow xx123 row)
        EField a1 a2 a3 -> eField a1 a2 a3


preprocess ::
     (MonadState (Int, Program PreAst) m)
  => Definition (Label Type) (Expr Type Type () Void)
  -> m (Definition (Label Type) PreAst)
preprocess = traverse liftLambdas <<< fillParams
--  traverse liftLambdas (fillParams (convert <$> def))
--  where
--    convert =
--      cata $ \case
--        EVar v -> eVar v
--        ECon c -> eCon c
--        ELit l -> eLit l
--        EIf a1 a2 a3 -> eIf a1 a2 a3
--        ELam _ a1 a2 -> eLam () a1 a2
--        ELet a1 a2 a3 -> eLet a1 a2 a3
--        EApp a1 a2 a3 -> eApp a1 a2 a3
--        EOp2 op a1 a2 -> eOp2 op a1 a2
--        ECase a1 a2 -> eCase a1 a2
--        ERow row -> eRow (mapRow convert row)
--        EField a1 a2 a3 -> eField a1 a2 a3

replaceFunArgs :: (MonadState (Int, Program PreAst) m) => PreAst -> m PreAst
replaceFunArgs =
  cata $ \case
    EApp t fun args -> do
      f <- fun
      as <- sequence args
      Program defs <- gets snd
      --let zzz1 (_, e) = isTCon ArrT (typeOf e) && undefined -- v `elem` defs
      --let (as1, as2) = partition zzz1 (zip [0 ..] as)
      let (as1, as2) = partition (isTCon ArrT . typeOf . snd) (zip [0 ..] as)
      case project f of
        EVar (t1, var)
          | not (null as1) -> do
            --traceShowM "**** 2"
            --traceShowM var
            --def <- lookupDef var
            def <- lookupDef var -- (if "g" == var then ".f0" else if "f" == var then ".f1" else var)
            let Function ps (t2, e) = def
            name <- uniqueName ".h"
            let getVar i =
                  let EVar v = project (as List.!! i)
                   in snd v
                subs = [(snd (ps !! i), getVar i) | i <- fst <$> as1]
                def1 =
                  Function
                    (fromList [ps !! i | i <- fst <$> as2])
                    (t2, substMany subs e)
            insertDef name =<< undefined -- preprocess def1
            pure (eApp t (eVar (typeOf def1, name)) (snd <$> as2))
        _ -> pure (eApp t f as)
    e -> embed <$> sequence e

convertFunApps :: PreAst -> Ast
convertFunApps =
  combineApps >>>
  cata
    (\case
       EVar a -> eVar a
       ECon con -> eCall con []
       ELit a -> eLit a
       EIf a1 a2 a3 -> eIf a1 a2 a3
       ELet a1 a2 a3 -> eLet a1 a2 a3
       EOp2 op a1 a2 -> eOp2 op a1 a2
       ECase a1 a2 -> eCase a1 a2
       EApp _ expr args -> do
         case project expr of
           EVar var -> eCall var args
           ECall _ fun as1 -> do
             eCall fun (as1 <> args)
           e -> error "Implementation error (2)"
       ERow row -> eRow (mapRow convertFunApps row)
       EField a1 a2 a3 -> eField a1 a2 a3)


--
-- let
--   z =
--     (if x > 3 
--       then f
--       else g)(5)
--   in
--     z(3)
--
--
-- lam(x) => plus(x)
--
--
-- (let x = 3 in f)(5)
--
-- (lam(v_0) => let x = 3 in f(v_0))(5)
--
--
--     (if x > 3 
--       then f
--       else g)
--
--     (lam(v_0) => if x > 3 
--       then f(v_0)
--       else g(v_0))

---------------------------------------------------------------------

--one123 :: TypedExpr -> TypedExpr
--one123 te =
--  fez (typeOf te) te
--
--  cata $ \case
----    EVar (t, var) | arity t > 0 ->
----      undefined
----    ECon (t, con) | arity t > 0 ->
----      undefined
--    EIf e1 e2 e3 | arity e3 > 0 ->
--      fez (typeOf e3) (eIf e1 e2 e3)
--    ELet t e1 e2 | arity e2 > 0 ->
--      fez (typeOf e2) (eLet t e1 e2) 
----    EApp t fun as | arity t > 0 ->
----      undefined
----    ELam _ args e1 | arity e1 > 0 ->
----      undefined
----    ECase e1 ((_, e):_) | arity e > 0 ->
----      undefined
----    EField fs e1 e2 | arity e2 > 0 ->
----      undefined
--    e ->
--      embed e

fez :: TypedExpr -> TypedExpr
fez = 
  cata $ \case
--    EApp t (Fix (EVar (t1, v))) as | arity t > 0 ->
--      eLam () (args t) (eApp (returnType t) (eVar (t1, v)) (as <> (args t <#> eVar)))
    EApp t fun as | arity t > 0 ->
      eLam () (args t) (eApp (returnType t) fun (as <> (args t <#> eVar)))
--    EVar (t, fun) | arity t > 0 ->
--      eLam () (args t) (eApp (returnType t) (eVar (t, fun)) (args t <#> eVar))
    e ->
      embed e
  where
    args e = zip (argTypes e) [".v" <> showt m | m <- [0 :: Int .. ]]

--app e = eApp (returnType e) e (args e <#> eVar)

--    ELet t e1 e2 | arity e2 > 0 ->
--      eLam () (args e2) (eLet t e1 (app e2))
--    EIf e1 e2 e3 | arity e3 > 0 ->
--      eLam () (args e3) (eIf e1 (app e2) (app e3))

--fez e
--  | arity t > 0 = eLam () args (boo e)
--  | otherwise = e
--    where
--      t = typeOf e
--      args = zip (argTypes t) names
--
--      names = [".v" <> showt m | m <- [0 :: Int .. ]]
--
--      appt n t = foldType1 (drop n (unwindType t))
--
--      boo :: TypedExpr -> TypedExpr
--      boo = para $ \case
--        --EVar (t, var) ->
--        --  eApp (appt (length args) t) (eVar (t, var)) (args <#> eVar)
--
--        --ECon (t, con) ->
--        --  eApp undefined undefined undefined -- (appt (length args) t) (eVar (t, var)) (args <#> eVar)
--
--        EApp t0 xx args0 -> do
--          eApp (appt (length args) t0) undefined ((args0 <#> fst) <> (args <#> eVar))
--
--        ELet t0 (_, e1) (_, e2) ->
--          eLet t0 e1 e2 
--
--        EIf (e1, _) (_, e2) (_, e3) ->
--          eIf e1 e2 e3
--
--        e -> eApp (appt (length args) t) (embed (fst <$> e)) (args <#> eVar)

        --e ->
        --  eApp (foldType1 (drop (length args) (unwindType (embed e)))) (embed e) (args <#> eVar)

-- let x = 3 in f
--
-- let x = 3 in lam(v_0) => f(v_0)
--
-- lam(v_0) => let x = 3 in f(v_0)
test001 =
  fez (eLet (tInt, "x") (eLit (PInt 3)) (eVar (tInt ~> tInt, "f")))
  ==
    eLam () [(tInt, ".v0")] (eLet (tInt, "x") (eLit (PInt 3)) (eApp tInt (eVar (tInt ~> tInt, "f")) [eVar (tInt, ".v0")]))
    
-- if x then f else g
-- lam(v_0) => if x then f(v_0) else g(v_0)
test002 =
  fez (eIf (eVar (tBool, "x")) (eVar (tInt ~> tInt, "f")) (eVar (tInt ~> tInt, "g")))
  ==
    eLam () [(tInt, ".v0")] (eIf (eVar (tBool, "x")) (eApp tInt (eVar (tInt ~> tInt, "f")) [eVar (tInt, ".v0")]) (eApp tInt (eVar (tInt ~> tInt, "g")) [eVar (tInt, ".v0")]))


-- f(3)
-- lam(v_0) => f(3, v_0)
test003 =
  fez (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "f")) [eLit (PInt 3)])
  ==
  eLam () [(tInt, ".v0")] (eApp tInt (eVar (tInt ~> tInt ~> tInt, "f")) [eLit (PInt 3), eVar (tInt, ".v0")])


-- f
-- lam(v_0) => f(v_0)
test004 =
  fez (eVar (tInt ~> tInt, "f")) 
  ==
  eLam () [(tInt, ".v0")] (eApp tInt (eVar (tInt ~> tInt, "f")) [eVar (tInt, ".v0")])


-------------------------------)
-------------------------------)
-------------------------------)

--  let
--    h =
--      lam(r) =>
--        r(4)
--    in
--      let 
--        f =
--          lam[x, y] => x + y
--        in
--          let 
--            q =
--              (if z > 5 then f else g)(5)
--            in
--              q(3) + h(q)
--


--  $f1(r) = r(4)          
--  $f2(x, y) = x + y      
--  $f3(z, f, a0, a1) = if z > 5 then f(a0, a1) else g(a0, a1)
--                         
--
--  let h = [$f1]
--    in
--      let f = [$f2]
--        in
--          let q = [$f3(z, f, 5)]
--            in
--              [$f3(z, f, 5, 3)] + [$f1(q)]
--








--  let 
--    f =
--      lam[x, y] => x + y
--    in
--      let 
--        q =
--          (if z > 5 then f else g)(5)
--        in
--          q(3)
--
testop1 :: TypedExpr
testop1 =
  eLet
    (tInt ~> tInt ~> tInt, "f")
    (eLam () [(tInt, "x"), (tInt, "y")] (eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))))
    (eLet
        (tInt ~> tInt, "q") 
        (eApp (tInt ~> tInt)
          (eIf (eOp2 oGtInt (eVar (tInt, "z")) (eLit (PInt 5))) (eVar (tInt ~> tInt ~> tInt, "f")) (eVar (tInt ~> tInt ~> tInt, "g")))
          [eLit (PInt 5)])
        (eApp tInt (eVar (tInt ~> tInt, "q")) [eLit (PInt 3)]))

--  let 
--    f =
--      lam(x) =>
--        lam(y) => 
--          x + y
--    in
--      let 
--        q =
--          (if z > 5 then f else g)(5)
--        in
--          q(3)
--
testop1_0 :: TypedExpr
testop1_0 =
  eLet
    (tInt ~> tInt ~> tInt, "f")
    (eLam () [(tInt, "x")] 
        (eLam () [(tInt, "y")] 
            (eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y")))))
    (eLet
        (tInt ~> tInt, "q") 
        (eApp (tInt ~> tInt)
          (eIf (eOp2 oGtInt (eVar (tInt, "z")) (eLit (PInt 5))) (eVar (tInt ~> tInt ~> tInt, "f")) (eVar (tInt ~> tInt ~> tInt, "g")))
          [eLit (PInt 5)])
        (eApp tInt (eVar (tInt ~> tInt, "q")) [eLit (PInt 3)]))


--  let 
--    f =
--      lam[x, y] => x + y
--    in
--      let 
--        q =
--          $f1(z, f, 5)
--        in
--          q(3)
--
testop1_1 :: TypedExpr
testop1_1 =
  eLet
    (tInt ~> tInt ~> tInt, "f")
    (eLam () [(tInt, "x"), (tInt, "y")] (eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))))
    (eLet
        (tInt ~> tInt, "q") 
        (eApp (tInt ~> tInt)
          (eVar (tInt ~> (tInt ~> tInt ~> tInt) ~> tInt ~> tInt ~> tInt, "$f1"))
          [eVar (tInt, "z"), eVar (tInt ~> tInt ~> tInt, "f"), eLit (PInt 5)])
        (eApp tInt (eVar (tInt ~> tInt, "q")) [eLit (PInt 3)]))


-- --  let 
-- --    f =
-- --      lam[x, y] => x + y
-- --    in
-- --      let 
-- --        q =
-- --          $f1(z, f, 5)
-- --        in
-- --          q(3)
-- --
-- testop1_1_1 :: TypedExpr
-- testop1_1_1 =
--   eLet
--     (tInt ~> tInt ~> tInt, "f")
--     (eLam () [(tInt, "x"), (tInt, "y")] (eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))))
--     (eLet
--         (tInt ~> tInt, "q") 
--         (eApp (tInt ~> tInt)
--           (eVar (tInt ~> (tInt ~> tInt ~> tInt) ~> tInt ~> tInt ~> tInt, "$f1"))
--           [eVar (tInt, "z"), eVar (tInt ~> tInt ~> tInt, "f"), eLit (PInt 5)])
--         (eApp tInt (eVar (tInt ~> tInt, "q")) [eLit (PInt 3)]))


--
--  $f1[z : int, f : int ~> int ~> int, $v1 : int, $v2 : int] = 
--    if z > 5 
--      then 
--        f($v1, $v2) 
--      else 
--        g($v1, $v2)
--
testop1_2 :: Program TypedExpr
testop1_2 = Program (Map.fromList [
    ( "$f1" 
    , Function (fromList [(tInt, "z"), (tInt ~> tInt ~> tInt, "f"), (tInt, "$v1"), (tInt, "$v2")])
        (tInt, eIf (eOp2 oGtInt (eVar (tInt, "z")) (eLit (PInt 5))) 
            (eApp tInt (eVar (tInt ~> tInt ~> tInt, "f")) [eVar (tInt, "$v1"), eVar (tInt, "$v2")])
            (eApp tInt (eVar (tInt ~> tInt ~> tInt, "g")) [eVar (tInt, "$v1"), eVar (tInt, "$v2")])
        )
    )
    ])


--  let 
--    q =
--      (lam(a, b) => a)(5)
--    in
--      q(3)
--
testop2 :: TypedExpr
testop2 =
  eLet
      (tInt ~> tInt, "q") 
      (eApp (tInt ~> tInt)
        (eLam () [(tInt, "a"), (tInt, "b")] (eVar (tInt, "a")))
        [eLit (PInt 5)])
      (eApp tInt (eVar (tInt ~> tInt, "q")) [eLit (PInt 3)])

--  let 
--    q =
--      $f1(5)
--    in
--      q(3)
--
testop2_1 :: TypedExpr
testop2_1 =
  eLet
      (tInt ~> tInt, "q") 
      (eApp (tInt ~> tInt)
        (eVar (tInt ~> tInt ~> tInt, "$f1"))
        [eLit (PInt 5)])
      (eApp tInt (eVar (tInt ~> tInt, "q")) [eLit (PInt 3)])

--
-- f1[a, b] = a
--
testop2_2 :: Program TypedExpr
testop2_2 = Program (Map.fromList [
    ( "$f1" 
    , Function (fromList [(tInt, "a"), (tInt, "b")]) (tInt, eVar (tInt, "a")))
    ])


--  let 
--    q =
--      (lam(a, b) => x)(5)
--    in
--      q(3)
--
testop3 :: TypedExpr
testop3 =
  eLet
      (tInt ~> tInt, "q") 
      (eApp (tInt ~> tInt)
        (eLam () [(tInt, "a"), (tInt, "b")] (eVar (tInt, "x")))
        [eLit (PInt 5)])
      (eApp tInt (eVar (tInt ~> tInt, "q")) [eLit (PInt 3)])


-- $f1(x, a, b) = x
--
--  let 
--    q =
--      $f1(x, 5)
--    in
--      q(3)
--
testop3_1 :: TypedExpr
testop3_1 =
  eLet
      (tInt ~> tInt, "q") 
      (eApp (tInt ~> tInt)
        (eVar (tInt ~> tInt ~> tInt ~> tInt, "$f1"))
        [eVar (tInt, "x"), eLit (PInt 5)])
      (eApp tInt (eVar (tInt ~> tInt, "q")) [eLit (PInt 3)])


-- $f1(x, a, b) = x
-- $f2(x, v0) = $f1(x, 5, v0)
--
--  let 
--    q = 
--      $f2(x)
--    in
--      q(3)
--
testop3_1_1 :: TypedExpr
testop3_1_1 =
  undefined
--  eLet
--      (tInt ~> tInt, "q") 
--      (eApp (tInt ~> tInt)
--        (eVar (tInt ~> tInt ~> tInt ~> tInt, "$f1"))
--        [eVar (tInt, "x"), eLit (PInt 5)])
--      (eApp tInt (eVar (tInt ~> tInt, "q")) [eLit (PInt 3)])

--
-- f1[x, a, b] = x
--
testop3_2 :: Program TypedExpr
testop3_2 = Program (Map.fromList [
    ( "$f1" 
    , Function (fromList [(tInt, "x"), (tInt, "a"), (tInt, "b")]) (tInt, eVar (tInt, "x")))
    ])


--  let 
--    q =
--      lam(a, b) => a
--    in
--      let
--        r =
--          q(3)
--        in
--          r(4)
--
testop4 :: TypedExpr
testop4 =
  eLet
    (tInt ~> tInt ~> tInt, "q") 
      (eLam () [(tInt, "a"), (tInt, "b")] (eVar (tInt, "a")))
      (eLet 
        (tInt ~> tInt, "r") 
        (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "q")) [eLit (PInt 3)])
        (eApp tInt (eVar (tInt ~> tInt, "r")) [eLit (PInt 4)]))


--  $f1(a, b) = a
--  $f2(q, v0) = q(3, v0)
--
--  let 
--    q = 
--      $f1
--    in
--      let
--        r =
--          $f2(q)
--        in
--          r(4)
--
testop4_1 :: TypedExpr
testop4_1 =
  eLet
    (tInt ~> tInt ~> tInt, "q") 
      (eLam () [(tInt, "a"), (tInt, "b")] (eVar (tInt, "a")))
      (eLet 
        (tInt ~> tInt, "r") 
        (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "q")) [eLit (PInt 3)])
        (eApp tInt (eVar (tInt ~> tInt, "r")) [eLit (PInt 4)]))






foo :: [Label Type] -> Type -> TypedExpr -> Definition (Label Type) TypedExpr
foo vs t = project >>> \case 
  ELam _ args e1 ->
    Function (fromList (vs <> args)) (r, e1)
  EIf e1 e2 e3 ->
    Function (fromList (vs <> zip ts names)) (r, eIf e1 (eApp r e2 args1) (eApp r e3 args1))
  where
    r = returnType t
    ts = argTypes t
    args1 = eVar <$> zip ts names
    names = ["$v" <> showt m | m <- [1 :: Int .. ]]

op1 :: (MonadState (Int, Program TypedExpr) m) => [Label Type] -> TypedExpr -> m TypedExpr
op1 ns =
  cata $ \case
    ELam t xs expr ->
      expr >>= (project >>> \case 
        ELam _ ys e2 -> pure (eLam t (xs <> ys) e2)
        e1 -> pure (eLam t xs (embed e1)))
    EApp t fun args | arity t > 0 -> do
      f <- fun
      let t0 = typeOf f
      name <- uniqueName "$f"
      let fs = freeVars f `without` ((t0, name):ns)
          def = foo fs t0 f
      insertDef name def
      as <- sequence args
      pure (eApp t (eVar (typeOf def, name)) ((eVar <$> fs) <> as))
    e ->
      embed <$> sequence e

ns = [(tInt ~> tInt ~> tInt, "g")]

tst1 = evalState (op1 ns testop1) (1, emptyProgram) == testop1_1

tst2 = snd (execState (op1 ns testop1) (1, emptyProgram)) == testop1_2

tst3 = evalState (op1 ns testop1_0) (1, emptyProgram) == testop1_1

tst4 = (snd <$> runState (op1 ns testop2) (1, emptyProgram)) == (testop2_1, testop2_2)

tst5 = (snd <$> runState (op1 ns testop3) (1, emptyProgram)) == (testop3_1, testop3_2)

tstall = tst1 && tst2 && tst3 && tst4 && tst5


def1 = Function (fromList [(tInt, "z"), (tInt ~> tInt ~> tInt, "f"), (tInt, "$v1"), (tInt, "$v2")]) (tInt, eLit (PInt 3)) :: Definition (Label Type) TypedExpr
tdef = tInt ~> (tInt ~> tInt ~> tInt) ~> tInt ~> tInt ~> tInt

-- >>>

appa :: [Expr Type Void () ()] -> Expr Type Void () () -> Expr Type Void () ()
appa as = 
  cata $ \case
    EVar v -> eCall undefined undefined
    ECon c -> eCall undefined undefined
    EIf a1 a2 a3 -> eIf a1 (appa as a2) (appa as a3)
    ELet a1 a2 a3 -> undefined -- eLet a1 a2 a3
    EOp2 op a1 a2 -> eOp2 op a1 a2
    ECase a1 a2 -> eCase a1 (second (appa as) <$> a2)
--    EApp _ expr args -> undefined
    EField a1 a2 a3 -> undefined -- eField a1 a2 a3)
    --
    ELit l -> undefined
    ERow row -> undefined -- eRow (mapRow (appa as) row)

beebop :: (MonadState (Int, Program (Expr Type Void () ())) m) => TypedExpr -> m (Expr Type Void () ())
beebop =
  cata $ \case
    ELam t args e1 -> do
      e <- e1
      case project e of
        ELam _ ys e2 -> pure (eLam t (args <> ys) e2)
        _ -> do
          let f = eLam () args e
          let t0 = typeOf f
          name <- uniqueName "$f"
          let fs = freeVars f `without` ((t0, name):ns)
              def = Function (fromList (fs <> args)) (typeOf e, e)

          insertDef name def
          pure (eCall (typeOf def, name) [])

    EApp t fun args -> do
      f <- fun
      case project f of
        ECall _ g as1 -> do
          as <- sequence args
          pure (eCall g (as1 <> as))
        EVar v -> do
          as <- sequence args
          pure (eCall v as)
        EIf e1 e2 e3 -> do
          let f = eIf e1 e2 e3
              t0 = typeOf f
              r = returnType t0
              ts = argTypes t0
              args1 = eVar <$> zip ts names
              names = ["$v" <> showt m | m <- [1 :: Int .. ]]
          name <- uniqueName "$f"
          let fs = freeVars f `without` ((t0, name):ns)
              def = Function (fromList (fs <> zip ts names)) (r, eIf e1 (app args1 e2) (app args1 e3))
              --def = Function (fromList (fs <> zip ts names)) (r, eIf e1 (eApp r e2 args1) (eApp r e3 args1))
          insertDef name def
          pure (eCall (typeOf def, name) [])
        e ->
          undefined -- pure (eCall (tInt, "zz") [])
        --_ -> do
        --  let t0 = typeOf f
        --  name <- uniqueName "$f"
        --  let fs = freeVars f `without` ((t0, name):ns)
        --      def = foo2 fs t0 f
        --  insertDef name def
        --  as <- sequence args
        --  pure (eCall (typeOf def, name) ((eVar <$> fs) <> as))
    EVar v -> pure (eVar v)
--    ECon c -> pure (eCon c)
    ECon c -> pure (eCall c [])
    ELit l -> pure (eLit l)
    EIf a1 a2 a3 -> eIf <$> a1 <*> a2 <*> a3
    ELet a1 a2 a3 -> eLet a1 <$> a2 <*> a3
    EOp2 op a1 a2 -> eOp2 op <$> a1 <*> a2
    ECase a1 a2 -> eCase <$> a1 <*> traverse sequence a2
    ERow row -> eRow <$> mapRowM beebop row
    EField a1 a2 a3 -> eField a1 <$> a2 <*> a3

app args = 
  project >>> \case
    ECall _ f as -> do
      eCall f (args <> as)

--foo3 :: (MonadState (Int, Program (Expr Type Type () ())) m) => [Label Type] -> Type -> Expr Type Type () () -> m (Definition (Label Type) (Expr Type Type () ()))
--foo3 vs t = 
--  cata $ \case 
--    ELam _ args e1 -> do
--      e <- e1
--      pure (Function undefine (r, e))
----      pure (Function (fromList (vs <> args)) (r, e1))
----    EIf e1 e2 e3 ->
----      Function (fromList (vs <> zip ts names)) 
----        (r, eIf e1 (eApp r e2 args1) (eApp r e3 args1))
--    expr -> do
--      e <- sequence expr
--      error (show e)
--    where
--      r = undefined -- returnType t
----      ts = argTypes t
----      args1 = eVar <$> zip ts names
----      names = ["$v" <> showt m | m <- [1 :: Int .. ]]

foo2 :: [Label Type] -> Type -> Expr Type Void () () -> Definition (Label Type) (Expr Type Void () ())
foo2 vs t = undefined

--foo2 :: [Label Type] -> Type -> Expr Type Type () () -> Definition (Label Type) (Expr Type Type () ())
--foo2 vs t = project >>> \case 
--  ELam _ args e1 ->
--    Function (fromList (vs <> args)) (r, e1)
--  EIf e1 e2 e3 ->
--    Function (fromList (vs <> zip ts names)) (r, eIf e1 (eApp r e2 args1) (eApp r e3 args1))
--  e ->
--    error (show e)
--  where
--    r = returnType t
--    ts = argTypes t
--    args1 = eVar <$> zip ts names
--    names = ["$v" <> showt m | m <- [1 :: Int .. ]]

beebop2 :: (MonadState (Int, Program (Expr Type Type () ())) m, MonadReader [(Name, Name)] m) => [Label Type] -> Expr Type Type () () -> m (Expr Type Type () ())
beebop2 x =
  cata $ \case
    ECall _ f as ->
      undefined
    e -> 
      embed <$> sequence e



--foo2 :: [Label Type] -> Type -> TypedExpr -> Definition (Label Type) TypedExpr
--foo2 vs t = project >>> \case 
--  ELam _ args e1 ->
--    Function (fromList (vs <> args)) (r, e1)
--  EIf e1 e2 e3 ->
--    Function (fromList (vs <> zip ts names)) (r, eIf e1 (eApp r e2 args1) (eApp r e3 args1))
--  where
--    r = returnType t
--    ts = argTypes t
--    args1 = eVar <$> zip ts names
--    names = ["$v" <> showt m | m <- [1 :: Int .. ]]


--  let
--    h =
--      lam(r) =>
--        r(4)
--    in
--      h(lam(x) => x + 1)
exp0 = 
  eLet
    ((tInt ~> tInt) ~> tInt, "h")
    (eLam () [(tInt ~> tInt, "r")]
      (eApp tInt (eVar (tInt ~> tInt, "r")) [eLit (PInt 4)]))
    (eApp tInt
      (eVar ((tInt ~> tInt) ~> tInt, "h"))
      [eLam () [(tInt, "x")] (eOp2 oAddInt (eVar (tInt, "x")) (eLit (PInt 1)))])

--  $lam1(r) = r(4)
--  $lam2(x) = x + 1
--
--  let
--    h = [$lam1]
--    in
--      h([$lam2])
exp0_ = 
  ( eLet
    ((tInt ~> tInt) ~> tInt, "h")
    (eCall ((tInt ~> tInt) ~> tInt, "$lam1") [])
    (eCall ((tInt ~> tInt) ~> tInt, "h") [eCall (tInt ~> tInt, "$lam2") []])
  , Program (Map.fromList 
      [ ( "$lam1" , Function (fromList [(tInt ~> tInt, "r")]) (tInt, eCall (tInt ~> tInt, "r") [eLit (PInt 4)]) )
      , ( "$lam2" , Function (fromList [(tInt, "x")]) (tInt, eOp2 oAddInt (eVar (tInt, "x")) (eLit (PInt 1))) )
      ])
  )


--  let 
--    f =
--      lam[x, y] => (z + x) + y
--    in
--      f
exp00 =
  eLet
    (tInt ~> tInt ~> tInt, "f")
    (eLam () [(tInt, "x"), (tInt, "y")] (eOp2 oAddInt (eOp2 oAddInt (eVar (tInt, "z")) (eVar (tInt, "x"))) (eVar (tInt, "y"))))
    (eVar (tInt ~> tInt ~> tInt, "f"))


--  $lam1(z, x, y) = 
--    (z + x) + y
--
--  let 
--    f =
--      [$lam1(z)]
--    in
--      f
exp00_ =
  ( eLet
    (tInt ~> tInt ~> tInt, "f")
    (eCall (tInt ~> tInt ~> tInt ~> tInt, "$lam1") [eVar (tInt, "z")])
    (eVar (tInt ~> tInt ~> tInt, "f"))
  , Program (Map.fromList 
      [ ( "$lam1" 
        , Function (fromList [(tInt, "z"), (tInt, "x"), (tInt, "y")]) (tInt, eOp2 oAddInt (eOp2 oAddInt (eVar (tInt, "z")) (eVar (tInt, "x"))) (eVar (tInt, "y"))) )
      ])
  )

-- 
--  (if z > 5 then f else f)(5)
--
expx0 = 
  eApp (tInt ~> tInt)
    (eIf (eOp2 oGtInt (eVar (tInt, "z")) (eLit (PInt 5))) (eVar (tInt ~> tInt ~> tInt, "f")) (eVar (tInt ~> tInt ~> tInt, "f"))) 
    [eLit (PInt 5)]



-- 
--  $if1(z, f, $v1, $v2) = if z > 5 then f($v1, $v2) else f($v1, $v2)
--
--  ($if1)(z, f, 5)
--
expx0_ = 
  ( eCall (tInt ~> (tInt ~> tInt ~> tInt) ~> tInt ~> tInt ~> tInt, "$if1")
    [eVar (tInt, "z"), eVar (tInt ~> tInt ~> tInt, "f"), eLit (PInt 5)]
  , Program (Map.fromList 
      [ ( "$if1" 
        , Function (fromList [(tInt, "z"), (tInt ~> tInt ~> tInt, "f"), (tInt, "$v1"), (tInt, "$v2")]) 
        ( tInt, eIf (eOp2 oGtInt (eVar (tInt, "z")) (eLit (PInt 5))) 
              (eCall (tInt ~> tInt ~> tInt, "f") [eVar (tInt, "$v1"), eVar (tInt, "$v2")]) 
              (eCall (tInt ~> tInt ~> tInt, "f") [eVar (tInt, "$v1"), eVar (tInt, "$v2")]))
         )
      ])
  )


-- 
--  (if z > 5 then f else g)(5)
--
expx01 = 
  eApp (tInt ~> tInt)
    (eIf (eOp2 oGtInt (eVar (tInt, "z")) (eLit (PInt 5))) (eVar (tInt ~> tInt ~> tInt, "f")) (eVar (tInt ~> tInt ~> tInt, "g"))) 
    [eLit (PInt 5)]


expx01_ = 
  ( eCall (tInt ~> (tInt ~> tInt ~> tInt) ~> tInt ~> tInt ~> tInt, "$if1")
    [eVar (tInt, "z"), eVar (tInt ~> tInt ~> tInt, "f"), eLit (PInt 5)]
  , Program (Map.fromList 
      [ ( "$if1" 
        , Function (fromList [(tInt, "z"), (tInt ~> tInt ~> tInt, "f"), (tInt, "$v1"), (tInt, "$v2")]) 
          ( tInt, eIf (eOp2 oGtInt (eVar (tInt, "z")) (eLit (PInt 5))) 
                (eCall (tInt ~> tInt ~> tInt, "f") [eVar (tInt, "$v1"), eVar (tInt, "$v2")]) 
                (eCall (tInt ~> tInt ~> tInt, "g") [eVar (tInt, "$v1"), eVar (tInt, "$v2")]))
           )
        , ( "g", Function (fromList [(tInt, "x"), (tInt, "y")]) (tInt, eLit (PInt 1))
          )
      ])
  )




-- 
--  (lam(x) => f)(5)
--
expx8 = 
  eApp (tInt ~> tInt)
    (eLam () [(tInt, "x")] (eVar (tInt ~> tInt, "f")))
    [eLit (PInt 5)]

-- 
--  $lam1(f, x, $v1) = [f($v1)]
--
--  [$lam1(f, 5)]
--
expx8_ = 
  ( eCall ((tInt ~> tInt) ~> tInt ~> tInt ~> tInt, "$lam1")
    [eVar (tInt ~> tInt, "f"), eLit (PInt 5)]
  , Program (Map.fromList 
      [ ( "$lam1" 
        , Function (fromList [(tInt ~> tInt, "f"), (tInt, "x"), (tInt, "$v1")]) (tInt, eCall (tInt ~> tInt, "f") [eVar (tInt, "$v1")] ) )
      ])
    )




--  let
--    h =
--      lam(r) =>
--        r(4)
--    in
--      let 
--        f =
--          lam[x, y] => (z + x) + y
--        in
--          let 
--            q =
--              (if z > 5 then f else g)(5)
--            in
--              q(3) + h(q)
--
exp1 = 
  eLet
    ((tInt ~> tInt) ~> tInt, "h")
    (eLam () [(tInt ~> tInt, "r")]
      (eApp tInt (eVar (tInt ~> tInt, "r")) [eLit (PInt 4)]))
    (eLet
      (tInt ~> tInt ~> tInt, "f")
      (eLam () [(tInt, "x"), (tInt, "y")] (eOp2 oAddInt (eOp2 oAddInt (eVar (tInt, "z")) (eVar (tInt, "x"))) (eVar (tInt, "y"))))
      (eLet
        (tInt ~> tInt, "q")
        (eApp (tInt ~> tInt)
          (eIf (eOp2 oGtInt (eVar (tInt, "z")) (eLit (PInt 5))) (eVar (tInt ~> tInt ~> tInt, "f")) (eVar (tInt ~> tInt ~> tInt, "g"))) 
          [eLit (PInt 5)])
        (eOp2 oAddInt 
          (eApp tInt (eVar (tInt ~> tInt, "q")) [eLit (PInt 3)]) 
          (eApp tInt (eVar ((tInt ~> tInt) ~> tInt, "h")) [eVar (tInt ~> tInt, "q")]))
      )
    )

--  $lam1(r) = r(4)
--  $lam2(z, x, y) => (z + x) + y
--  $if3(z, f, $v1, $v2) = if z > 5 then f($v1, $v2) else g($v1, $v2)
--
--  let
--    h = [$lam1]
--    in
--      let 
--        f = [$lam2(z)]
--        in
--          let 
--            q =
--              [$if3(z, f, 5)]
--            in
--              q(3) + h(q)
--
exp1_ = 
  ( eLet
    ((tInt ~> tInt) ~> tInt, "h")
    (eCall ((tInt ~> tInt) ~> tInt, "$lam1") [])
    (eLet
      (tInt ~> tInt ~> tInt, "f")
      (eCall (tInt ~> tInt ~> tInt ~> tInt, "$lam2") [eVar (tInt, "z")])
      (eLet
        (tInt ~> tInt, "q")
          (eCall (tInt ~> (tInt ~> tInt ~> tInt) ~> tInt ~> tInt ~> tInt, "$if3") [eVar (tInt, "z"), eVar (tInt ~> tInt ~> tInt, "f"), eLit (PInt 5)])
        (eOp2 oAddInt 
          (eCall (tInt ~> tInt, "q") [eLit (PInt 3)]) 
          (eCall ((tInt ~> tInt) ~> tInt, "h") [eVar (tInt ~> tInt, "q")]))
      )
    )
  , Program (Map.fromList 
      [ ( "$if3" , Function (fromList [(tInt, "z"), (tInt ~> tInt ~> tInt, "f"), (tInt, "$v1"), (tInt, "$v2")]) (tInt, eIf (eOp2 oGtInt (eVar (tInt, "z")) (eLit (PInt 5))) (eCall (tInt ~> tInt ~> tInt, "f") [eVar (tInt, "$v1"), eVar (tInt, "$v2")]) (eCall (tInt ~> tInt ~> tInt, "g") [eVar (tInt, "$v1"), eVar (tInt, "$v2")])) ) 
      , ( "$lam1" , Function (fromList [(tInt ~> tInt, "r")]) (tInt, eCall (tInt ~> tInt, "r") [eLit (PInt 4)]) )
      , ( "$lam2" , Function (fromList [(tInt, "z"), (tInt, "x"), (tInt, "y")]) (tInt, eOp2 oAddInt (eOp2 oAddInt (eVar (tInt, "z")) (eVar (tInt, "x"))) (eVar (tInt, "y"))) )
      , ( "g" , Function (fromList [(tInt, "x"), (tInt, "y")]) (tInt, eLit (PInt 1)))
      ])
  )


-- 
--  let
--    fact =
--      lam(n) =>
--        if n == 0
--          then 1
--          else n * fact(n - 1)
--    in
--      fact(5)
--
expx9 = 
  eLet
    (tInt ~> tInt, "fact")
    (eLam () [(tInt, "n")] 
      (eIf
        (eOp2 oEqInt (eVar (tInt, "n")) (eLit (PInt 0)))
        (eLit (PInt 1))
        (eOp2 oMulInt (eVar (tInt, "n")) (eApp 
          tInt
          (eVar (tInt ~> tInt, "fact"))
          [eOp2 oSubInt (eVar (tInt, "n")) (eLit (PInt 1))]))))
    (eApp tInt (eVar (tInt ~> tInt, "fact")) [eLit (PInt 5)])
    


-- 
--  $lam1(fact, n) = 
--        if n == 0
--          then 1
--          else n * fact(n - 1)
--
--  let
--    fact =
--      [$lam1(fact)]
--    in
--      fact(5)
--
expx9_ = 
  ( eLet
    (tInt ~> tInt, "fact") (eCall ((tInt ~> tInt) ~> tInt ~> tInt, "$lam1") [eVar (tInt ~> tInt, "fact")])
    (eCall (tInt ~> tInt, "fact") [eLit (PInt 5)])
  , Program (Map.fromList 
      [ ( "$lam1" , Function (fromList [(tInt ~> tInt, "fact"), (tInt, "n")]) (tInt, 
              eIf
                (eOp2 oEqInt (eVar (tInt, "n")) (eLit (PInt 0)))
                (eLit (PInt 1))
                (eOp2 oMulInt (eVar (tInt, "n")) (eCall
                  (tInt ~> tInt, "fact")
                  [eOp2 oSubInt (eVar (tInt, "n")) (eLit (PInt 1))])))) 
      ])
  )




--  $f1(r) = r(4)
--  $f2(z, x, y) => (z + x) + y
--
--  let
--    h = $f1
--    in
--      let 
--        f = $f2(z)
--        in
--          let 
--            q =
--              (if z > 5 then f else g)(5)
--            in
--              q(3) + h(q)
--



-- $f1(r) = r(4) 
-- $f2(z, x, y) = (z + x) + y 
-- $f3(z, f, v1, v2) = if z > 5 then f(v1, v2) else g(v1, v2)
--
-- let
--   h =
--     [$f1]
--   in
--     let
--       f =
--         [$f2]
--       in
--         let
--           q =
--             [$f3(z, f, 5)]
--           in
--             [q(3)] + [h(q)]
exp2 = 
  undefined
--  eLet
--    ((tInt ~> tInt) ~> tInt, "h")
--    (eLam () [(tInt ~> tInt, "r")]
--      (eApp tInt (eVar (tInt ~> tInt, "r")) [eLit (PInt 4)]))
--    (eLet
--      (tInt ~> tInt ~> tInt, "f")
--      (eLam () [(tInt, "x"), (tInt, "y")] (eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))))
--      (eLet
--        (tInt ~> tInt, "q")
--        (eApp (tInt ~> tInt)
--          (eIf (eOp2 oGtInt (eVar (tInt, "z")) (eLit (PInt 5))) (eVar (tInt ~> tInt ~> tInt, "f")) (eVar (tInt ~> tInt ~> tInt, "g"))) 
--          [eLit (PInt 5)])
--        (eOp2 oAddInt 
--          (eApp tInt (eVar (tInt ~> tInt, "q")) [eLit (PInt 3)]) 
--          (eApp tInt (eVar ((tInt ~> tInt) ~> tInt, "h")) [eVar (tInt ~> tInt, "q")]))
--      )
--    )


-- let
--   xs = Cons(f(1), Nil())
-- 
-- match xs {
--   | Cons(y, ys) => y
-- }
--
--
--  f(xs) = 
--    match xs with
--      Cons(y, ys) => y(2, 3)
--
--  g() =
--    let
--      zs = 
--        Cons( (lam(x, y, z) => 3)(1)
--            , Nil()
--            )
--      in
--        f(zs, zs)
--





t123 = 
  runState (beebop exp1) (1, emptyProgram)


xyz :: (MonadState (Int, Program Ast) m) 
    => Name 
    -> [Label Type]
    -> [Label Type]
    -> Ast
    -> m Ast
xyz name vs args expr = do
  insertDef name def
  pure (eCall (typeOf def, name) (eVar <$> vs))
  where
    t = typeOf expr
    as = vs <> args `without` [(t, name)]
    def = Function (fromList as) (t, expr)

appxx :: [Ast] -> Ast -> Ast
appxx xs = 
  cata $ \case
    ECall _ g ys -> do
      eCall g (ys <> xs)
    EVar v -> do
      eCall v xs

extra :: Typed t => t -> [Label Type]
extra t = zip (argTypes t) ["$v" <> showt m | m <- [1 :: Int .. ]]

gorkx :: (MonadState (Int, Program Ast) m) => m [Label Type]
gorkx = do
  Program p <- gets snd
  pure (swap <$> Map.toList (Map.map typeOf p))

bernie :: (MonadState (Int, Program Ast) m) => TypedExpr -> m Ast
bernie =
  cata $ \case
    ELam t args expr1 -> do
      e1 <- expr1
      name <- uniqueName "$lam"
      defs <- gorkx
      let vs = freeVars e1 `without` (args <> defs)
          ys = extra (typeOf e1)
      xyz name vs (args <> ys) $ if null ys
         then e1
         else appxx (eVar <$> ys) e1

    EIf expr1 expr2 expr3 -> do
      e1 <- expr1
      e2 <- expr2
      e3 <- expr3
      let t = typeOf e3
      if isTCon ArrT t
        then do 
          defs <- gorkx
          name <- uniqueName "$if"
          let vs = nub (freeVars e1 <> freeVars e2 <> freeVars e3) `without` defs
              ys = extra t
              app = appxx (eVar <$> ys)
          xyz name vs ys (eIf e1 (app e2) (app e3))
        else 
          pure (eIf e1 e2 e3)

    EApp t fun args -> do
      f <- fun
      xs <- sequence args
      case project f of
        ECall _ g ys -> do
          pure (eCall g (ys <> xs))
        EVar v -> do
          pure (eCall v xs)

    ELet expr1 expr2 expr3 -> eLet expr1 <$> expr2 <*> expr3
    EOp2 op a1 a2 -> eOp2 op <$> a1 <*> a2
    EVar v -> pure (eVar v)
    ELit l -> pure (eLit l)

--    expr -> do 
--      e <- sequence expr
--      error (show e)


t0t0 = second snd (runState (bernie exp00) (1, emptyProgram)) == exp00_
t0t1 = second snd (runState (bernie exp0) (1, emptyProgram)) == exp0_
t0t2 = second snd (runState (bernie expx0) (1, emptyProgram)) == expx0_
t0t3 = second snd (runState (bernie expx8) (1, emptyProgram)) == expx8_
t0t4 = second snd (runState (bernie expx01) (1, Program (Map.fromList [("g", Function (fromList [(tInt, "x"), (tInt, "y")]) (tInt, eLit (PInt 1)))]))) == expx01_
t0t5 = second snd (runState (bernie exp1) (1, Program (Map.fromList [("g", Function (fromList [(tInt, "x"), (tInt, "y")]) (tInt, eLit (PInt 1)))]))) == exp1_
t0t6 = evalProgram__ expyx_ == LitValue (PInt 125)
t0t7 = evalProgram__ expyy_ == LitValue (PInt 125)
t0t8 = evalProgram__ exp0_ == LitValue (PInt 5)
t0t9 = evalProgram__ (second snd (runState (bernie expx9) (1, emptyProgram))) == LitValue (PInt 120)
t0t10 = second snd (runState (bernie expx9) (1, emptyProgram)) == expx9_

t0ta = t0t0 && t0t1 && t0t2 && t0t3 && t0t4 && t0t5 && t0t6 && t0t7 && t0t8 && t0t9 && t0t10


expyx_ =
  ( eLet
      (tInt, "z")
      (eLit (PInt 123))
        (eLet
        (tInt, "f")
        (eCall (tInt ~> tInt ~> tInt ~> tInt, "$lam1") [eVar (tInt, "z"), eLit (PInt 1), eLit (PInt 1)])
        (eVar (tInt, "f")))
  , Program (Map.fromList 
      [ ( "$lam1" 
        , Function (fromList [(tInt, "z"), (tInt, "x"), (tInt, "y")]) (tInt, eOp2 oAddInt (eOp2 oAddInt (eVar (tInt, "z")) (eVar (tInt, "x"))) (eVar (tInt, "y"))) )
      ])
  )


expyy_ =
  ( eLet
      (tInt, "z")
      (eLit (PInt 123))
        (eLet
        (tInt ~> tInt ~> tInt, "f")
        (eCall (tInt ~> tInt ~> tInt ~> tInt, "$lam1") [eVar (tInt, "z")])
        (eCall (tInt ~> tInt ~> tInt, "f") [eLit (PInt 1), eLit (PInt 1)]))
  , Program (Map.fromList 
      [ ( "$lam1" 
        , Function (fromList [(tInt, "z"), (tInt, "x"), (tInt, "y")]) (tInt, eOp2 oAddInt (eOp2 oAddInt (eVar (tInt, "z")) (eVar (tInt, "x"))) (eVar (tInt, "y"))) )
      ])
  )

