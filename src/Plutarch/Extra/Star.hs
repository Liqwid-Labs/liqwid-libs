module Plutarch.Extra.Star (
  -- * Type
  PStar (..),

  -- * Functions
  papplyStar,
) where

import Plutarch.Extra.Applicative (PApplicative (ppure), PApply (pliftA2))
import Plutarch.Extra.Bind (PBind ((#>>=)))
import Plutarch.Extra.Category (PCategory (pidentity), PSemigroupoid ((#>>>)))
import Plutarch.Extra.Functor (PFunctor (PSubcategory, pfmap), Plut)
import Plutarch.Extra.Profunctor (PProfunctor (PCoSubcategory, PContraSubcategory, pdimap))
import Plutarch.Extra.TermCont (pmatchC)

{- | The (profunctorial) view over a Kleisli arrow. Its name comes from category
 theory, as it is one of the ways we can lift a functor (in this case, @f@)
 into a profunctor.

 This essentially enables us to work with @a :--> f b@ using 'PSemigroupoid'
 and 'PCategory' operations as easily as we do @a :--> b@, provided that @f@
 is at least a 'PBind'. With the addition of a 'PApplicative' (for identities),
 we become a full 'PCategory'. Furthermore, we can also compose freely with
 ordinary Plutarch ':-->' at /both/ ends of a 'PStar', provided @f@ is at
 least 'PFunctor'.

 @since 3.0.1
-}
newtype PStar (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PStar (Term s (a :--> f b))
  deriving stock
    ( -- | @since 3.0.1
      Generic
    )
  deriving anyclass
    ( -- | @since 3.0.1
      PlutusType
    )

-- | @since 3.0.1
instance DerivePlutusType (PStar f a b) where
  type DPTStrat _ = PlutusTypeNewtype

{- | If @f@ is at least a 'PFunctor', we can pre-process and post-process work
 done in @PStar f@ using pure functions.

 @since 3.1.0
-}
instance (PFunctor f) => PProfunctor (PStar f) where
  type PContraSubcategory (PStar f) = Plut
  type PCoSubcategory (PStar f) = PSubcategory f
  pdimap = phoistAcyclic $
    plam $ \into outOf xs -> unTermCont $ do
      PStar g <- pmatchC xs
      pure . pcon . PStar $ pdimap # into # (pfmap # outOf) # g

{- | Strengthening @f@ to 'PBind' allows us to compose @PStar f@ computations
 like ordinary Plutarch functions.

 @since 3.0.1
-}
instance (PBind f) => PSemigroupoid (PStar f) where
  {-# INLINEABLE (#>>>) #-}
  t #>>> t' = pmatch t $ \case
    PStar ab -> pmatch t' $ \case
      PStar bc -> pcon . PStar . plam $ \x -> (ab # x) #>>= bc

{- | Strengthening @f@ by adding 'PApplicative' gives us an identity, which
 makes us a full category, on par with @Plut@ as evidenced by ':-->'.
,
 @since 3.0.1
-}
instance
  (PApplicative f, PBind f) =>
  PCategory (PStar f)
  where
  pidentity = pcon . PStar . plam $ \x -> ppure # x

{- | This essentially makes @PStar f a b@ equivalent to the Haskell @ReaderT a f
 b@: that is, a read-only environment of type @a@ producing a result of type
 @b@ in an effect @f@. If @f@ is /only/ a 'PFunctor', we can only lift, but
 not compose.

 @since 3.0.1
-}
instance (PFunctor f) => PFunctor (PStar f a) where
  type PSubcategory (PStar f a) = PSubcategory f
  pfmap = phoistAcyclic $
    plam $ \f xs -> unTermCont $ do
      PStar g <- pmatchC xs
      pure . pcon . PStar . plam $ \x -> pfmap # f # (g # x)

{- | Strengthening to 'PApply' for @f@ allows us to combine together
 computations in @PStar f a@ using the same \'view\' as in the 'PFunctor'
 instance.

 @since 3.0.1
-}
instance (PApply f) => PApply (PStar f a) where
  pliftA2 = phoistAcyclic $
    plam $ \f xs ys -> unTermCont $ do
      PStar g <- pmatchC xs
      PStar h <- pmatchC ys
      pure . pcon . PStar . plam $ \x -> pliftA2 # f # (g # x) # (h # x)

{- | Strengthening to 'PApplicative' for @f@ allows arbitrary lifts into @PStar
 f a@, using the same \'view\' as in the 'PFunctor' instance.

 @since 3.0.1
-}
instance (PApplicative f) => PApplicative (PStar f a) where
  ppure = phoistAcyclic $ plam $ \x -> pcon . PStar . plam $ \_ -> ppure # x

{- | Strengthening to 'PBind' for @f@ allows dynamic control flow on the basis
 of the result of a @PStar f a@, using the same \'view\' as the 'PFunctor'
 instance.

 @since 3.0.1
-}
instance (PBind f) => PBind (PStar f a) where
  {-# INLINEABLE (#>>=) #-}
  xs #>>= f = pmatch xs $ \case
    PStar g -> pcon . PStar . plam $
      \x -> (g # x) #>>= (f #>>> (papplyStar # x))

{- | \'Run\' the 'PStar' as the function it secretly is. Useful for cases where
 you want to build up a large computation using 'PStar' instances, then
 execute.

 @since 3.0.1
-}
papplyStar ::
  forall (a :: S -> Type) (b :: S -> Type) (f :: (S -> Type) -> S -> Type) (s :: S).
  Term s (a :--> PStar f a b :--> f b)
papplyStar = phoistAcyclic $
  plam $ \x f -> pmatch f $ \case
    PStar f' -> f' # x
