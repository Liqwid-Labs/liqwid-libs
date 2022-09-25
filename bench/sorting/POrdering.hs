module POrdering (
  POrdering' (..),
  pdot,
) where

import Plutarch.Internal.PlutusType (PlutusType (pcon', pmatch'))

-- Implementation of <> for the PInner for POrdering'
pdot :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pdot = phoistAcyclic $
  plam $ \x y ->
    pif (x #< 2) (x * y) x

-- Direct implementation of POrdering rep for benching against
data POrdering' (s :: S)
  = PLT'
  | PEQ'
  | PGT'
  deriving stock (Show, Generic)
  deriving anyclass (PEq, PShow, PPartialOrd, POrd)

instance PlutusType POrdering' where
  type PInner POrdering' = PInteger
  pcon' = \case
    PLT' -> 0
    PEQ' -> 1
    PGT' -> 2
  pmatch' x f =
    pif (x #== 0) (f PLT') (pif (x #== 1) (f PEQ') (f PGT'))

instance Semigroup (Term s POrdering') where
  x <> y = pmatch x $ \case
    PLT' -> pcon PLT'
    PEQ' -> y
    PGT' -> pcon PGT'
