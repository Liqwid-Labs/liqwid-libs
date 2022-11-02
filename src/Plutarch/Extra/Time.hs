{-# LANGUAGE ViewPatterns #-}

module Plutarch.Extra.Time (
  PCurrentTime (..),
  pcurrentTime,
  currentTime,
  passertCurrentTime,
  pisWithinCurrentTime,
  pisCurrentTimeWithin,
  pcurrentTimeDuration,
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

     Note: 'PCurrentTime' doesn't need a Haskell-level equivalent because it
     is only used in scripts, and does not go in datums. It is also
     Scott-encoded.

     @since 3.3.0
-}
data PCurrentTime (s :: S)
  = PCurrentTime
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
instance DerivePlutusType PCurrentTime where
  type DPTStrat _ = PlutusTypeScott

{- | Get the current time, given a 'PPOSIXTimeRange'.

     If it's impossible to get a full-bounded time (for example, either end of
     the 'PPOSIXTimeRange' is an infinity), then we return 'PNothing'.

     @since 3.3.0
-}
pcurrentTime ::
  forall (s :: S).
  Term
    s
    ( PPOSIXTimeRange
        :--> PMaybe PCurrentTime
    )
pcurrentTime = phoistAcyclic $
  plam $ \iv -> unTermCont $ do
    PInterval iv' <- pmatchC iv
    ivf <- pletAllC iv'
    PLowerBound lb <- pmatchC (getField @"from" ivf)
    PUpperBound ub <- pmatchC (getField @"to" ivf)

    let getBound = phoistAcyclic $
          plam $
            flip pletAll $ \f ->
              pif
                (getField @"_1" f)
                ( pmatch (getField @"_0" f) $ \case
                    PFinite (pfromData . (pfield @"_0" #) -> d) -> pjust # d
                    _ ->
                      ptrace
                        "pcurrentTime: time range should be bounded"
                        pnothing
                )
                (ptrace "pcurrentTime: time range should be inclusive" pnothing)

        lb' = getBound # lb
        ub' = getBound # ub

        mkTime = phoistAcyclic $ plam $ pcon .* PCurrentTime
    pure $ pliftA2 # mkTime # lb' # ub'

{- | Calculate the current time by providing the @validRange@ field,
     which typically comes from 'PTxInfo'.

     @since 3.3.0
-}
currentTime ::
  forall r (s :: S).
  (HasField "validRange" r (Term s PPOSIXTimeRange)) =>
  r ->
  Term s (PMaybe PCurrentTime)
currentTime x = pcurrentTime # getField @"validRange" x

{- | Calculate the current time, and error out with the given message if we can't
     get it.

     @since 3.3.0
-}
passertCurrentTime ::
  forall (s :: S).
  Term
    s
    ( PString
        :--> PPOSIXTimeRange
        :--> PCurrentTime
    )
passertCurrentTime = phoistAcyclic $
  plam $
    \msg iv -> passertPJust # msg #$ pcurrentTime # iv

{- | Retutn 'PTrue' if a `PPOSIXTime` is in the current time range.

     @since 3.3.0
-}
pisWithinCurrentTime ::
  forall (s :: S).
  Term
    s
    ( PPOSIXTime
        :--> PCurrentTime
        :--> PBool
    )
pisWithinCurrentTime = phoistAcyclic $
  plam $ \time ctr ->
    pmatch ctr $ \(PCurrentTime lb ub) ->
      lb #<= time #&& time #<= ub

{- | Return 'PTrue' if current time is within the given time range.

     Note that the first argument is the lower bound of said time range, and
     the second is the upper bound.

     @since 3.3.0
-}
pisCurrentTimeWithin ::
  forall (s :: S).
  Term
    s
    ( PPOSIXTime
        :--> PPOSIXTime
        :--> PCurrentTime
        :--> PBool
    )
pisCurrentTimeWithin = phoistAcyclic $
  plam $ \lb' ub' ctr ->
    pmatch ctr $ \(PCurrentTime lb ub) ->
      lb' #<= lb #&& ub #<= ub'

{- | Return the duration of current time.

     @since 3.14.1
-}
pcurrentTimeDuration ::
  forall (s :: S).
  Term
    s
    ( PCurrentTime
        :--> PPOSIXTime
    )
pcurrentTimeDuration = phoistAcyclic $
  plam $
    flip pmatch $
      \(PCurrentTime lb ub) -> ub - lb
