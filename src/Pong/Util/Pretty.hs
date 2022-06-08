{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pong.Util.Pretty where

import Pong.Data
import Pong.Lang
import Pong.Util
import Prettyprinter
import Prettyprinter.Render.Text

parensIf :: Bool -> Doc a -> Doc a
parensIf True doc = parens doc
parensIf _ doc = doc

{- ORMOLU_DISABLE -}

instance Pretty Prim where
  pretty = 
    \case
      PBool   True  -> "true"
      PBool   False -> "false"
      PUnit         -> "()"
      PInt    p     -> pretty p
      PFloat  p     -> pretty p
      PDouble p     -> pretty p
      PChar   p     -> squotes (pretty p)
      PString p     -> dquotes (pretty p)

{- ORMOLU_ENABLE -}

instance (Pretty v, Pretty s) => Pretty (Row (Type v s) Int) where
  pretty =
    para
      ( \case
          RNil -> ""
          RVar v -> pretty v
          RExt name t (Fix row, doc) ->
            let field = pretty name <+> ":" <+> pretty t
             in field <> case row of
                  RNil ->
                    ""
                  RVar v ->
                    " |" <+> "'" <> pretty v
                  _ ->
                    " ," <+> doc
      )

instance (Pretty v, Pretty s) => Pretty (Type v s) where
  pretty =
    para
      ( \case
          TUnit -> "unit"
          TBool -> "bool"
          TInt -> "int"
          TFloat -> "float"
          TDouble -> "double"
          TChar -> "char"
          TString -> "string"
          TArr (t1, doc1) (_, doc2) ->
            parensIf (isConT ArrT t1) doc1 <+> "->" <+> doc2
          TVar v -> "'" <> pretty v
          TGen s -> pretty s
          TRec row -> "{" <+> pretty row <+> "}"
          TCon con ts ->
            pretty con
              <+> hsep (uncurry (parensIf . addParens) <$> ts)
      )
    where
      addParens =
        cata
          ( \case
              TCon _ [] -> False
              TCon{} -> True
              TArr{} -> True
              _ -> False
          )

{-

\$lam1(x : int -> int, v0 : int) : int =
  x(v0)

\$lam2(x : int) : int =
  x

\$lam3(x : int, y : int) : int =
  x + y

\$lam4(x : int, v0 : int, v1 : int) : int =
  $lam3(v0, v1)

main(a : unit) : int =
  let
    $var_id_3 =
      $lam1
    in
      let
        $var_id_4 =
          $lam2
        in
          let
            $var_add_1 =
              $lam4
            in
              let
                add2 =
                  $var_add_1(2)
                in
                  $var_id_3(add2, $var_id_4(3)) + $var_add_2(4, 5)

-}
