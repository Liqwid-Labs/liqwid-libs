module Plutarch.Extra.Function (
    pconst,
    pidentity,
    papply,
    pon,
    pbuiltinUncurry,
    pflip,
) where

-- | @since 1.0.0
pconst ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    Term s (a :--> b :--> a)
pconst = phoistAcyclic $ plam const

-- | @since 1.0.0
pidentity ::
    forall (a :: S -> Type) (s :: S).
    Term s (a :--> a)
pidentity = phoistAcyclic $ plam id

-- | @since 1.0.0
papply ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    Term s ((a :--> b) :--> a :--> b)
papply = phoistAcyclic $ plam $ \f x -> f # x

{- | Plutarch level 'Data.Function.on'.
     @since 1.3.0
-}
pon ::
    forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
    Term s ((b :--> b :--> c) :--> (a :--> b) :--> a :--> a :--> c)
pon = phoistAcyclic $
    plam $ \f g x y ->
        let a = g # x
            b = g # y
         in f # a # b

{- | Make uncurried function with Haskell function with two arguments.
     @since 1.3.0
-}
pbuiltinUncurry ::
    forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
    ( PIsData a
    , PIsData b
    ) =>
    (Term s a -> Term s b -> Term s c) ->
    Term s (PBuiltinPair (PAsData a) (PAsData b)) ->
    Term s c
pbuiltinUncurry f p =
    let p1 = pfromData $ pfstBuiltin # p
        p2 = pfromData $ psndBuiltin # p
     in f p1 p2

-- | @since 1.4.0
pflip ::
    forall
        (a :: S -> Type)
        (b :: S -> Type)
        (c :: S -> Type)
        (s :: S).
    Term
        s
        ((a :--> b :--> c) :--> b :--> a :--> c)
pflip = phoistAcyclic $ plam $ \f y x -> f # x # y
