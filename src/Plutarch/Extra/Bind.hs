{-# LANGUAGE QuantifiedConstraints #-}

module Plutarch.Extra.Bind (
  -- * Type class
  PBind (..),

  -- * Functions
  pjoin,
) where

import Plutarch.Api.V1.Maybe (PMaybeData (PDJust, PDNothing))
import Plutarch.Extra.Applicative (PApply)
import Plutarch.Extra.Function (pidentity)
import Plutarch.Extra.Functor (PSubcategory)
import Plutarch.Lift (PUnsafeLiftDecl)

{- | Gives the capability to bind a Kleisli arrow over @f@ to a value:
 essentially, the equivalent of Haskell's '>>='. Unlike Haskell, we don't
 require the availability of 'pure': to recover the equivalent of Haskell's
 'Monad', you want both 'PApplicative' and 'PBind'.

 = Laws

 * @(m '#>>=' f) '#>>=' g@ @=@ @m '#>>=' ('plam' '$' \x -> (f '#' x) '#>>=' g)@
 * @f '#<*>' x@ @=@ @f '#>>=' ('#<$>' x)@

 @since 3.0.1
-}
class (PApply f) => PBind (f :: (S -> Type) -> S -> Type) where
  -- | '>>=', but as a function on 'Term's.
  (#>>=) ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PSubcategory f a, PSubcategory f b) =>
    Term s (f a) ->
    Term s (a :--> f b) ->
    Term s (f b)

infixl 1 #>>=

-- | @since 3.0.1
instance PBind PMaybe where
  {-# INLINEABLE (#>>=) #-}
  xs #>>= f = pmatch xs $ \case
    PNothing -> pcon PNothing
    PJust t -> f # t

-- | @since 3.0.1
instance PBind PMaybeData where
  {-# INLINEABLE (#>>=) #-}
  xs #>>= f = pmatch xs $ \case
    PDNothing t -> pcon . PDNothing $ t
    PDJust t -> f # pfromData (pfield @"_0" # t)

-- | @since 3.0.1
instance PBind PList where
  (#>>=) ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    Term s (PList a) ->
    Term s (a :--> PList b) ->
    Term s (PList b)
  {-# INLINEABLE (#>>=) #-}
  xs #>>= f = pelimList go pnil xs
    where
      go :: Term s a -> Term s (PList a) -> Term s (PList b)
      go h t = pconcat # (f # h) # (t #>>= f)

-- | @since 3.0.1
instance PBind PBuiltinList where
  (#>>=) ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PUnsafeLiftDecl a, PUnsafeLiftDecl b) =>
    Term s (PBuiltinList a) ->
    Term s (a :--> PBuiltinList b) ->
    Term s (PBuiltinList b)
  {-# INLINEABLE (#>>=) #-}
  xs #>>= f = pelimList go pnil xs
    where
      go :: Term s a -> Term s (PBuiltinList a) -> Term s (PBuiltinList b)
      go h t = pconcat # (f # h) # (t #>>= f)

-- | @since 3.0.1
instance (forall (s :: S). Semigroup (Term s a)) => PBind (PPair a) where
  {-# INLINEABLE (#>>=) #-}
  xs #>>= f = pmatch xs $ \case
    PPair acc t -> pmatch (f # t) $ \case
      PPair acc' res -> pcon . PPair (acc <> acc') $ res

{- | Forwards the /first/ 'PLeft'.

 @since 3.0.1
-}
instance PBind (PEither e) where
  {-# INLINEABLE (#>>=) #-}
  xs #>>= f = pmatch xs $ \case
    PLeft t -> pcon . PLeft $ t
    PRight t -> f # t

{- | \'Flattens\' two identical 'PBind' layers into one.

 @since 3.0.1
-}
pjoin ::
  forall (a :: S -> Type) (f :: (S -> Type) -> S -> Type) (s :: S).
  (PBind f, PSubcategory f a, PSubcategory f (f a)) =>
  Term s (f (f a) :--> f a)
pjoin = phoistAcyclic $ plam $ \xs -> xs #>>= pidentity
