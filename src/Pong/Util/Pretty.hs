{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

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
  pretty _ =
    "ROW TODO"

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
          TRec row -> pretty row
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
