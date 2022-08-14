module Plutarch.Extra.Value.Unsorted (psort) where

import Plutarch.Api.V1 (
    PValue (PValue),
 )
import Plutarch.Api.V2 (
    AmountGuarantees (..),
    KeyGuarantees (..),
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
