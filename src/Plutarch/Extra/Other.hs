{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Extra.Other (
    -- * Plutarch deriving wrappers
    DerivePNewtype' (..),
    DerivePConstantViaNewtype' (..),
) where

import Control.Arrow (first)
import Data.Coerce (Coercible)
import Data.Kind (Constraint, Type)
import qualified Generics.SOP as SOP
import Plutarch (
    DerivePNewtype (..),
    POpaque,
    PType,
    PlutusType (PInner),
    S,
    Term,
    pto,
 )
import Plutarch.Bool (PEq, POrd)
import Plutarch.Builtin (PAsData, PData, PIsData)
import Plutarch.DataRepr (PDataFields (..))
import Plutarch.Integer (PIntegral)
import Plutarch.Internal (S (SI), punsafeCoerce)
import Plutarch.Lift (DerivePConstantViaNewtype, PConstantDecl, PLift, PUnsafeLiftDecl (PLifted))
import Plutarch.TryFrom (PTryFrom (..))

-- Plutarch deriving wrappers

{- | Convenience wrapper for 'DerivePNewtype', by automatically picking the type under a newtype to derive via.

 @since 1.1.0
-}
newtype DerivePNewtype' (a :: PType) (s :: S) = DerivePNewtype' (a s)

{- | Coercion between a Haskell-level of a 'PType' & the 'Term' of another 'PType'.

 @since 1.1.0
-}
type TermCoercible :: PType -> PType -> Constraint
type TermCoercible a b = forall (s :: S). Coercible (a s) (Term s b)

-- | Apply Constraint under a plutarch-level newtype.
type PNewtypeHas :: (PType -> Constraint) -> PType -> Constraint
type PNewtypeHas c a = (c (PNewtypeOf a))

-- | The SOP Code of a newtype over a given 'PType'.
type family PNewtypeOf (a :: PType) :: PType where
    PNewtypeOf a = PNewtypeOfCode (SOP.Code (a 'SI))

-- | The SOP Code of a newtype given the SOP Code of a 'PType'.
type family PNewtypeOfCode (a :: [[Type]]) :: PType where
    PNewtypeOfCode '[ '[Term s p]] = p

-- | @since 1.1.0
deriving via (DerivePNewtype a (PNewtypeOf a)) instance (PNewtypeHas PEq a) => PEq (DerivePNewtype' a)

-- | @since 1.1.0
deriving via (DerivePNewtype a (PNewtypeOf a)) instance (PNewtypeHas POrd a) => POrd (DerivePNewtype' a)

-- | @since 1.1.0
deriving via
    (DerivePNewtype a (PNewtypeOf a))
    instance
        ( PNewtypeOf a ~ b
        , PInner a POpaque ~ b
        , PIsData b
        , TermCoercible a b
        ) =>
        PIsData (DerivePNewtype' a)

-- | @since 1.1.0
deriving via (DerivePNewtype a (PNewtypeOf a)) instance (PNewtypeHas PIntegral a) => PIntegral (DerivePNewtype' a)

-- | @since 1.1.0
deriving via
    (DerivePNewtype a (PNewtypeOf a))
    instance
        ( PNewtypeOf a ~ b
        , TermCoercible a b
        ) =>
        PlutusType (DerivePNewtype' a)

-- | @since 1.1.0
instance (PDataFields b) => PDataFields (DerivePNewtype a b) where
    type PFields (DerivePNewtype a b) = (PFields b)
    ptoFields x = ptoFields $ pto x

-- | @since 1.1.0
deriving via (DerivePNewtype a (PNewtypeOf a)) instance (PNewtypeHas PDataFields a) => PDataFields (DerivePNewtype' a)

-- | @since 1.1.0
deriving via
    (DerivePNewtype a (PNewtypeOf a))
    instance
        ( PNewtypeOf a ~ b
        , TermCoercible a b
        , PTryFrom c b
        ) =>
        PTryFrom c (DerivePNewtype' a)

-- | @since 1.1.0
instance
    ( PNewtypeOf a ~ b
    , PTryFrom PData (PAsData b)
    ) =>
    PTryFrom PData (PAsData (DerivePNewtype' a))
    where
    type
        PTryFromExcess PData (PAsData (DerivePNewtype' a)) =
            PTryFromExcess PData (PAsData (PNewtypeOf a))
    ptryFrom' d k =
        ptryFrom' @_ @(PAsData (PNewtypeOf a)) d $ k . first punsafeCoerce

{- | Convenience wrapper for 'DerivePConstantViaNewtype', by automatically picking the type under a newtype to derive via.

 @since 1.1.0
-}
newtype DerivePConstantViaNewtype' (h :: Type) (p :: PType) = DerivePConstantViaNewtype' h

-- | @since 1.1.0
deriving via
    (DerivePConstantViaNewtype h p (PNewtypeOf p))
    instance
        ( PNewtypeOf p ~ ip
        , PLift ip
        , PLift p
        , Coercible h (DerivePConstantViaNewtype h p ip)
        , Coercible h (PLifted ip)
        ) =>
        PConstantDecl (DerivePConstantViaNewtype' h p)
