module Plutarch.Api.V1.Value (
    psingletonValue,
    --  passetClassValue,
    --  pvalueOf,
    --  padaOf
) where

import Plutarch (
    S,
    Term,
    pcon,
    phoistAcyclic,
    plam,
    unTermCont,
    (#),
    type (:-->),
 )
import Plutarch.Api.V1 (
    PCurrencySymbol,
    PMap (PMap),
    PTokenName,
    PValue (PValue),
 )
import Plutarch.Builtin (pdata, ppairDataBuiltin)
import Plutarch.Extra.TermCont (pletC)
import Plutarch.Integer (PInteger)
import Plutarch.List (psingleton)

psingletonValue ::
    forall (s :: S).
    Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PValue)
psingletonValue = phoistAcyclic $
    plam $ \cs tn i -> unTermCont $ do
        innerPair <- pletC (ppairDataBuiltin # pdata tn # pdata i)
        inner <- pletC (pcon . PMap $ psingleton # innerPair)
        outerPair <- pletC (ppairDataBuiltin # pdata cs # pdata inner)
        outer <- pletC (pcon . PMap $ psingleton # outerPair)
        pure . pcon . PValue $ outer
