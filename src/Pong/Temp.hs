module Pong.Temp where

{- ORMOLU_DISABLE -}

data RowF e r a
  = RNil                           -- ^ Empty row
  | RVar r                         -- ^ Row variable
  | RExt Name e a                  -- ^ Row extension

{- ORMOLU_ENABLE -}

-- | A row is a sequence of labeled fields. Rows encode the internal structure
-- of records, both at the type and expression level. A row can be either open
-- or closed. An open row is one that has a variable in the tail of the
-- sequence, whereas a closed row ends with the empty row.
type Row e r = Fix (RowF e r)

{- ORMOLU_DISABLE -}

data TypeF v s a
  = TUnit                          -- ^ Unit type
  | TBool                          -- ^ Boolean type
  | TInt                           -- ^ Type of integers (machine bounded)
  | TFloat                         -- ^ Single-precision floating point number
  | TDouble                        -- ^ Double-precision floating point number
  | TChar                          -- ^ Char type
  | TString                        -- ^ Unicode strings
  | TCon Name [a]                  -- ^ Algebraic data-types
  | TArr a a                       -- ^ Function types
  | TVar v                         -- ^ Type variable (monomorphic)
  | TGen s                         -- ^ Quantified type variable
  | TRow (Row (Type v s) Int)      -- ^ Row types

{- ORMOLU_ENABLE -}

type Type v s = Fix (TypeF v s)
