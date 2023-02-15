{-# LANGUAGE ViewPatterns #-}

module Plutarch.Extra.Time (
  PFullyBoundedTimeRange (..),
  pgetFullyBoundedTimeRange,
  fullyBoundedTimeRangeFromValidRange,
  passertFullyBoundedTimeRange,
  pisWithinTimeRange,
  pisTimeRangeWithin,
  ptimeRangeDuration,
) where

import Control.Composition ((.*))
import GHC.Records (HasField)
import Plutarch.Api.V1 (
  PExtended (PFinite),
  PInterval (PInterval),
  PLowerBound (PLowerBound),
  PPOSIXTime,
  PUpperBound (PUpperBound),
 )
import Plutarch.Api.V2 (PPOSIXTimeRange)
import Plutarch.Extra.Applicative (pliftA2)
import Plutarch.Extra.Field (pletAll, pletAllC)
import Plutarch.Extra.Maybe (passertPJust, pjust, pnothing)
import Plutarch.Extra.TermCont (pmatchC)

{- | Represent a fully bounded time range.

     Note: 'PFullyBoundedTimeRange' doesn't need a Haskell-level equivalent
     because it is only used in scripts, and does not go in datums. It is also
     Scott-encoded.

     @since 3.3.0
-}
data PFullyBoundedTimeRange (s :: S)
  = PFullyBoundedTimeRange
      (Term s PPOSIXTime)
      -- ^ The lower bound.
      (Term s PPOSIXTime)
      -- ^ The upper bound.
  deriving stock
    ( -- | @since 3.3.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.3.0
      PEq
    )

-- | @since 3.3.0
instance DerivePlutusType PFullyBoundedTimeRange where
  type DPTStrat _ = PlutusTypeScott

{- | Get the current time, given a 'PPOSIXTimeRange'.

     If it's impossible to get a full-bounded time (for example, either end of
     the 'PPOSIXTimeRange' is an infinity), then we return 'PNothing'.

     @since 3.3.0
-}
pgetFullyBoundedTimeRange ::
  forall (s :: S).
  Term
    s
    ( PPOSIXTimeRange
        :--> PMaybe PFullyBoundedTimeRange
    )
pgetFullyBoundedTimeRange = phoistAcyclic $
  plam $ \iv -> unTermCont $ do
    PInterval iv' <- pmatchC iv
    ivf <- pletAllC iv'
    PLowerBound lb <- pmatchC (getField @"from" ivf)
    PUpperBound ub <- pmatchC (getField @"to" ivf)

    let getBound = phoistAcyclic $
          plam $ \dontCheckInclusive ->
            flip pletAll $ \f ->
              pif
                (dontCheckInclusive #|| getField @"_1" f)
                ( pmatch (getField @"_0" f) $ \case
                    PFinite (pfromData . (pfield @"_0" #) -> d) -> pjust # d
                    _ ->
                      ptrace
                        "pcurrentTime: time range should be bounded"
                        pnothing
                )
                (ptrace "pcurrentTime: time range should be inclusive" pnothing)

        lb' = getBound # pcon PFalse # lb
        ub' = getBound # pcon PTrue # ub

        mkTime = phoistAcyclic $ plam $ pcon .* PFullyBoundedTimeRange
    pure $ pliftA2 # mkTime # lb' # ub'

{- | Calculate the current time by providing the @validRange@ field,
     which typically comes from 'PTxInfo'.

     @since 3.3.0
-}
fullyBoundedTimeRangeFromValidRange ::
  forall r (s :: S).
  (HasField "validRange" r (Term s PPOSIXTimeRange)) =>
  r ->
  Term s (PMaybe PFullyBoundedTimeRange)
fullyBoundedTimeRangeFromValidRange x =
  pgetFullyBoundedTimeRange
    # getField @"validRange" x

{- | Calculate the current time, and error out with the given message if we can't
     get it.

     @since 3.3.0
-}
passertFullyBoundedTimeRange ::
  forall (s :: S).
  Term
    s
    ( PString
        :--> PPOSIXTimeRange
        :--> PFullyBoundedTimeRange
    )
passertFullyBoundedTimeRange = phoistAcyclic $
  plam $
    \msg iv -> passertPJust # msg #$ pgetFullyBoundedTimeRange # iv

{- | Retutn 'PTrue' if a `PPOSIXTime` is in the current time range.

     @since 3.3.0
-}
pisWithinTimeRange ::
  forall (s :: S).
  Term
    s
    ( PPOSIXTime
        :--> PFullyBoundedTimeRange
        :--> PBool
    )
pisWithinTimeRange = phoistAcyclic $
  plam $ \time ctr ->
    pmatch ctr $ \(PFullyBoundedTimeRange lb ub) ->
      lb #<= time #&& time #<= ub

{- | Return 'PTrue' if current time is within the given time range.

     Note that the first argument is the lower bound of said time range, and
     the second is the upper bound.

     @since 3.3.0
-}
pisTimeRangeWithin ::
  forall (s :: S).
  Term
    s
    ( PPOSIXTime
        :--> PPOSIXTime
        :--> PFullyBoundedTimeRange
        :--> PBool
    )
pisTimeRangeWithin = phoistAcyclic $
  plam $ \lb' ub' ctr ->
    pmatch ctr $ \(PFullyBoundedTimeRange lb ub) ->
      lb' #<= lb #&& ub #<= ub'

{- | Return the duration of current time.

     @since 3.14.1
-}
ptimeRangeDuration ::
  forall (s :: S).
  Term
    s
    ( PFullyBoundedTimeRange
        :--> PPOSIXTime
    )
ptimeRangeDuration = phoistAcyclic $
  plam $
    flip pmatch $
      \(PFullyBoundedTimeRange lb ub) -> ub - lb
