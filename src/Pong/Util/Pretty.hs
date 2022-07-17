{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pong.Util.Pretty where

import Pong.Data
import Pong.Lang
import Pong.Util
import Prettyprinter

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
     PFloat  p     -> pretty p <> "f"
     PDouble p     -> pretty p
     PChar   p     -> squotes (pretty p)
     PString p     -> dquotes (pretty p)

{- ORMOLU_ENABLE -}

newtype TypeVar a = TypeVar a

instance (Pretty (TypeVar Name)) where
  pretty (TypeVar v) = pretty v

instance (Pretty (TypeVar Int)) where
  pretty (TypeVar v) = "'" <> pretty v

{- ORMOLU_DISABLE -}

instance (Pretty v, Pretty (TypeVar v)) => Pretty (Type v) where
  pretty =
    para
      ( \case
          TUnit   -> "unit"
          TBool   -> "bool"
          TInt    -> "int"
          TFloat  -> "float"
          TDouble -> "double"
          TChar   -> "char"
          TString -> "string"
          TVar v  -> pretty (TypeVar v)
          TCon con ts ->
            pretty con
              <+> hsep (uncurry (parensIf . addParens) <$> ts)
          TRec (Fix RNil, _) ->
            "{}"
          TRec (_, row) ->
            vsep
              [ lbrace <+> row
              , rbrace
              ]
          TArr (t1, doc1) (_, doc2) ->
            vsep
              [ parensIf (hasHeadT ArrT t1) (align doc1)
              , "->" <+> doc2
              ]
          RExt name (_, doc1) (t2, doc2) ->
            let field = pretty name <+> colon <+> doc1
             in case project t2 of
                  RNil ->
                    field
                  TVar{} ->
                    vsep
                      [ field
                      , pipe <+> doc2
                      ]
                  _ ->
                    vsep
                      [ field
                      , comma <+> doc2
                      ]
          _ ->
            mempty
      )
    where
      addParens =
        cata
          ( \case
              TCon _ [] -> False
              TCon{} -> True
              TArr{} -> True
              _      -> False
          )

{- ORMOLU_ENABLE -}
