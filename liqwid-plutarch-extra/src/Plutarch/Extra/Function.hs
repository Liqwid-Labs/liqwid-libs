module Plutarch.Extra.Function (
  pconst,
  pidentity,
  papply,
  pon,
  pbuiltinUncurry,
  pflip,
  (#.*),
  (#.**),
  (#.***),
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

infixr 8 #.*

-- | @since 3.0.3
(#.*) ::
  forall
    (d :: S -> Type)
    (c :: S -> Type)
    (b :: S -> Type)
    (a :: S -> Type)
    (s :: S).
  Term s (c :--> d) ->
  Term s (a :--> b :--> c) ->
  Term s (a :--> b :--> d)
(#.*) f g = plam $ \x y -> f #$ g # x # y

infixr 8 #.**

-- | @since 3.0.2
(#.**) ::
  forall
    (e :: S -> Type)
    (d :: S -> Type)
    (c :: S -> Type)
    (b :: S -> Type)
    (a :: S -> Type)
    (s :: S).
  Term s (d :--> e) ->
  Term s (a :--> b :--> c :--> d) ->
  Term s (a :--> b :--> c :--> e)
(#.**) f g = plam $ \x y z -> f #$ g # x # y # z

infixr 8 #.***

-- | @since 3.0.2
(#.***) ::
  forall
    (f :: S -> Type)
    (e :: S -> Type)
    (d :: S -> Type)
    (c :: S -> Type)
    (b :: S -> Type)
    (a :: S -> Type)
    (s :: S).
  Term s (e :--> f) ->
  Term s (a :--> b :--> c :--> d :--> e) ->
  Term s (a :--> b :--> c :--> d :--> f)
(#.***) f g = plam $ \x y z a -> f #$ g # x # y # z # a
