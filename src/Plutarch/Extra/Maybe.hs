{-# LANGUAGE TypeApplications #-}

module Plutarch.Extra.Maybe (
    pfromMaybe,
    pfromMaybeData,
    ptraceIfNothing,
) where

import Data.Kind (Type)
import Plutarch (
    S,
    Term,
    phoistAcyclic,
    plam,
    pmatch,
    unTermCont,
    (#),
    type (:-->),
 )
import Plutarch.Api.V1.Maybe (PMaybeData (PDJust, PDNothing))
import Plutarch.Builtin (PIsData, pfromData)
import Plutarch.DataRepr (pfield)
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.String (PString)
import Plutarch.Trace (ptraceError)

-- | @since 1.0.0
pfromMaybe ::
    forall (a :: S -> Type) (s :: S).
    Term s (PMaybe a :--> a)
pfromMaybe = phoistAcyclic $
    plam $ \t -> unTermCont $ do
        t' <- pmatchC t
        pure $ case t' of
            PNothing -> ptraceError "pfromMaybe: found PNothing"
            PJust x -> x

-- | @since 1.0.0
pfromMaybeData ::
    forall (a :: S -> Type) (s :: S).
    (PIsData a) =>
    Term s (PMaybeData a :--> a)
pfromMaybeData = phoistAcyclic $
    plam $ \t -> unTermCont $ do
        t' <- pmatchC t
        case t' of
            PDNothing _ -> pure . ptraceError $ "pfromMaybeData: found PDNothing"
            PDJust x -> pure (pfromData $ pfield @"_0" # x)

-- | @since 1.0.0
ptraceIfNothing ::
    forall (a :: S -> Type) (s :: S).
    Term s PString ->
    Term s (PMaybe a) ->
    Term s a
ptraceIfNothing err t = pmatch t $ \case
    PNothing -> ptraceError err
    PJust x -> x
