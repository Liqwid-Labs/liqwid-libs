module Plutarch.Extra.State (
  PState,
  pstate,
  prunState,
  pevalState,
  pexecState,
  pget,
  pput,
  pmodify,
) where

import Plutarch.Extra.Applicative (PApplicative (ppure), PApply (pliftA2))
import Plutarch.Extra.Bind (PBind ((#>>=)))
import Plutarch.Extra.Functor (PFunctor (PSubcategory, pfmap), Plut)
import Plutarch.Extra.TermCont (pmatchC)

-- | @since 1.0.0
newtype PState (s :: S -> Type) (a :: S -> Type) (s' :: S)
  = PState (Term s' (s :--> PPair s a))
  deriving stock
    ( -- | @since 1.4.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.0.0
      PlutusType
    )

-- | @since 1.4.0
instance DerivePlutusType (PState s a) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.1.0
instance PFunctor (PState s) where
  type PSubcategory (PState s) = Plut
  pfmap = phoistAcyclic $
    plam $ \f state -> unTermCont $ do
      PState g <- pmatchC state
      pure . pcon . PState $ plam $ \s -> pfmap # f # (g # s)

-- | @since 1.0.0
instance PApply (PState s) where
  pliftA2 = phoistAcyclic $
    plam $ \f xs ys -> unTermCont $ do
      PState g <- pmatchC xs
      PState h <- pmatchC ys
      pure . pcon . PState $
        plam $ \s -> unTermCont $ do
          PPair s' x <- pmatchC (g # s)
          PPair s'' y <- pmatchC (h # s')
          pure . pcon . PPair s'' $ f # x # y

-- | @since 1.0.0
instance PApplicative (PState s) where
  ppure =
    phoistAcyclic $ plam $ \x -> pcon . PState $ plam $ \s -> pcon . PPair s $ x

-- | @since 3.0.1
instance PBind (PState s) where
  {-# INLINEABLE (#>>=) #-}
  xs #>>= f = pmatch xs $ \case
    PState g -> pcon . PState . plam $ \s -> pmatch (g # s) $ \case
      PPair s' res -> pmatch (f # res) $ \case
        PState h -> h # s'

{- | Lift a Plutarch lambda into 'PState'.

 @since 1.0.0
-}
pstate ::
  forall (s :: S -> Type) (a :: S -> Type) (s' :: S).
  Term s' ((s :--> PPair s a) :--> PState s a)
pstate = phoistAcyclic $ plam $ pcon . PState

-- | @since 1.0.0
prunState ::
  forall (s :: S -> Type) (a :: S -> Type) (s' :: S).
  Term s' (PState s a :--> s :--> PPair s a)
prunState = phoistAcyclic $
  plam $ \comp state -> unTermCont $ do
    PState f <- pmatchC comp
    pure $ f # state

-- | @since 1.0.0
pevalState ::
  forall (s :: S -> Type) (a :: S -> Type) (s' :: S).
  Term s' (PState s a :--> s :--> a)
pevalState = phoistAcyclic $
  plam $ \comp state -> unTermCont $ do
    PPair _ x <- pmatchC (prunState # comp # state)
    pure x

-- | @since 1.0.0
pexecState ::
  forall (s :: S -> Type) (a :: S -> Type) (s' :: S).
  Term s' (PState s a :--> s :--> s)
pexecState = phoistAcyclic $
  plam $ \comp state -> unTermCont $ do
    PPair state' _ <- pmatchC (prunState # comp # state)
    pure state'

-- | @since 1.0.0
pget ::
  forall (s :: S -> Type) (s' :: S).
  Term s' (PState s s)
pget = pcon . PState $ plam $ \s -> pcon . PPair s $ s

-- | @since 1.0.0
pput ::
  forall (s :: S -> Type) (s' :: S).
  Term s' (s :--> PState s PUnit)
pput = phoistAcyclic $ plam $ \x -> pcon . PState $ plam $ \_ -> pcon . PPair x . pcon $ PUnit

-- | @since 1.0.0
pmodify ::
  forall (s :: S -> Type) (s' :: S).
  Term s' ((s :--> s) :--> PState s PUnit)
pmodify = phoistAcyclic $ plam $ \f -> pcon . PState $ plam $ \s -> pcon . PPair (f # s) . pcon $ PUnit
