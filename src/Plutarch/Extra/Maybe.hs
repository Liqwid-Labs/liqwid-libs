{-# LANGUAGE TypeApplications #-}

{- | Module: Plutarch.Extra.Maybe
 Description: Duplicated functions from `liqwid-plutarch-extra`

 Commit: d8be5f8dc91ad00158727fdbccb6db849e9f3559

 These functions are included here to avoid cyclic import issues.
 They should mirror the LPE implementations exactly.

 @since 2.0
-}
module Plutarch.Extra.Maybe (pmaybe, pfromDJust, pisDJust, pisJust) where

import Plutarch.Api.V2 (PMaybeData (PDJust, PDNothing))
import Plutarch.Prelude (
  PBool,
  PIsData,
  PMaybe (PJust, PNothing),
  S,
  Term,
  Type,
  pconstant,
  pfield,
  pfromData,
  phoistAcyclic,
  plam,
  pmatch,
  ptraceError,
  (#),
  (:-->),
 )

{- | Extract a 'PMaybe' by providing a default value in case of 'PJust'.

 @since 2.0
-}
pmaybe ::
  forall (a :: S -> Type) (s :: S).
  Term s (a :--> PMaybe a :--> a)
pmaybe = phoistAcyclic $
  plam $ \e a -> pmatch a $ \case
    PJust a' -> a'
    PNothing -> e

{- | Extracts the element out of a 'PDJust' and throws an error if
  its argument is 'PDNothing'.

 @since 2.0
-}
pfromDJust ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a) =>
  Term s (PMaybeData a :--> a)
pfromDJust = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PDNothing _ -> ptraceError "pfromDJust: found PDNothing"
    PDJust x -> pfromData $ pfield @"_0" # x

{- | Yield True if a given 'PMaybeData' is of form @'PDJust' _@.

 @since 2.0
-}
pisDJust ::
  forall (a :: S -> Type) (s :: S).
  Term s (PMaybeData a :--> PBool)
pisDJust = phoistAcyclic $
  plam $ \x -> pmatch x $ \case
    PDJust _ -> pconstant True
    _ -> pconstant False

{- | Yields true if the given 'PMaybe' value is of form @'PJust' _@.

 @since 2.0
-}
pisJust ::
  forall (a :: S -> Type) (s :: S).
  Term s (PMaybe a :--> PBool)
pisJust = phoistAcyclic $
  plam $ \v' ->
    pmatch v' $ \case
      PJust _ -> pconstant True
      _ -> pconstant False
