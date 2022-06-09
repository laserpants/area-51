{-# LANGUAGE FlexibleContexts #-}
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

instance (Pretty v, Pretty (TypeVar v)) => Pretty (Row (Type v) v) where
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

newtype TypeVar a = TypeVar a

instance (Pretty (TypeVar Name)) where
  pretty (TypeVar v) = pretty v

instance (Pretty (TypeVar Int)) where
  pretty (TypeVar v) = "'" <> pretty v

instance (Pretty v, Pretty (TypeVar v)) => Pretty (Type v) where
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
          TVar v -> pretty (TypeVar v)
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
