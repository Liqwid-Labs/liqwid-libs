module Plutarch.Api.V1.Value.Unsorted (psort) where

import Plutarch (
    S,
    Term,
    pcon,
    phoistAcyclic,
    plam,
    pmatch,
    (#),
    type (:-->),
 )
import Plutarch.Api.V1 (
    AmountGuarantees (..),
    KeyGuarantees (..),
    PValue (PValue),
 )
import qualified Plutarch.Extra.Map as Map
import qualified Plutarch.Extra.Map.Unsorted as UnsortedMap

{- | / O(n^2*logn) /. Sort a 'PValue'.

   @since 1.1.0
-}
psort ::
    forall (a :: AmountGuarantees) (s :: S).
    Term s (PValue 'Unsorted a :--> PValue 'Sorted a)
psort = phoistAcyclic $
    plam $ \v -> pmatch v $ \case
        (PValue m) ->
            let m' = Map.pmap # UnsortedMap.psort # m
                m'' = UnsortedMap.psort # m'
             in pcon $ PValue m''
