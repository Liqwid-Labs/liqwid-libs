{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Plutarch.Extra.Field (pletAll, pletAllC) where

import GHC.TypeLits (Symbol)
import Plutarch.DataRepr.Internal.Field (
    BindFields,
    Bindings,
    BoundTerms,
    HRec,
    HRecOf,
    PDataFields (PFields),
 )

type family BindAll (ps :: [PLabeledType]) :: [Symbol] where
    BindAll '[] = '[]
    BindAll ((name ':= _) ': xs) = name : BindAll xs

{- | Same as `pletFields` but instead of specifiying fields, it will take all fields.

 @since 1.3.0
-}
pletAll ::
    forall (a :: S -> Type) (s :: S) (b :: S -> Type) (ps :: [PLabeledType]) bs.
    ( PDataFields a
    , ps ~ PFields a
    , bs ~ Bindings ps (BindAll ps)
    , BindFields ps bs
    ) =>
    Term s a ->
    (HRecOf a (BindAll ps) s -> Term s b) ->
    Term s b
pletAll = pletFields @(BindAll ps)

{- | TermCont version of `pletAll`

 @since 1.3.0
-}
pletAllC ::
    forall (a :: S -> Type) (s :: S) (b :: S -> Type) (ps :: [PLabeledType]) bs.
    ( PDataFields a
    , ps ~ PFields a
    , bs ~ Bindings ps (BindAll ps)
    , BindFields ps bs
    ) =>
    Term s a ->
    TermCont @b s (HRec (BoundTerms ps bs s))
pletAllC = tcont . pletFields @(BindAll ps)
