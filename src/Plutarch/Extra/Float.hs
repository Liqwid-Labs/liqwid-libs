module Plutarch.Extra.Float (
  PFixed,
  pconvertExp,
  pfromFixed,
  ptoFixed,
  pfromRational,
  ptoRational,
  punsafeMkFixed,
) where

import Control.Composition (on, (.*))
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, Natural, natVal)
import Plutarch.Extra.Function (pflip)
import Plutarch.Extra.Rational ((#%))
import Plutarch.Num (PNum, pabs, pfromInteger, (#*), (#-))
import Plutarch.Show (pshow')
import Plutarch.Unsafe (punsafeCoerce)

{- | Fixed precision number. It behaves like scientific notation:
     `exp` shows to what power of base 10 an integer is multiplied.

     For example, Underlying value of 123456 with type `PFixed 3` is
     `123.456 (123456 * 10 ^ -3)`. If it's coerced into `PFixed 5`, it will be
     `1.23456 (123456 * 10 ^ -5)`. `PFixed 0` will be identical to `PInteger`.

     Note, `exp` is the negative exponent to base 10. 'PFixed' does not support
     positive expoent.

     Compared to 'PRational', 'PFixed' gives addition and subtraction
     as fast as regular 'PInteger', allows negative values, and does
     not require simplifications.

 @since 3.11.0
-}
newtype PFixed (exp :: Natural) (s :: S)
  = PFixed (Term s PInteger)
  deriving stock
    ( -- | @since 3.11.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.11.0
      PlutusType
    , -- | @since 3.11.0
      PIsData
    , -- | @since 3.11.0
      PEq
    , -- | @since 3.11.0
      PPartialOrd
    , -- | @since 3.11.0
      POrd
    )

-- | @since 3.11.0
instance forall (exp :: Natural). DerivePlutusType (PFixed exp) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.11.0
instance forall (exp :: Natural). KnownNat exp => PShow (PFixed exp) where
  pshow' wrap z =
    wrap' $
      "PFixed "
        <> pshow (pquot # pto z # base)
        <> "."
        <> (replicateStr # pconstant baseExp #- (places # decimal) # "0")
        <> pshow decimal
    where
      baseExp = natVal (Proxy @exp)
      base = pconstant (10 ^ baseExp)
      decimal = prem # (pabs # pto z) # base
      wrap' x = if wrap then "(" <> x <> ")" else x

      places =
        pfix #$ plam $ \self x ->
          plet (pquot # x # 10) $ \q ->
            pif (q #== 0) 1 (1 + self # q)

      replicateStr :: Term s (PInteger :--> PString :--> PString)
      replicateStr =
        pfix #$ plam $ \self x str ->
          pif (0 #< x) (str <> (self # (x #- 1) # str)) ""

-- | @since 3.11.0
instance forall (exp :: Natural). KnownNat exp => PNum (PFixed exp) where
  (#*) =
    (pcon . PFixed)
      .* (pflip # pdiv # pconstant (10 ^ natVal (Proxy @exp)) #)
      .* (#*) `on` pto
  pfromInteger =
    pcon
      . PFixed
      . (* pconstant (10 ^ natVal (Proxy @exp)))
      . pconstant

-- | @since 3.11.0
instance forall (exp :: Natural). KnownNat exp => PIntegral (PFixed exp) where
  pdiv =
    phoistAcyclic $
      plam $ \x y ->
        pcon . PFixed $
          pdiv # (pto x * pconstant (10 ^ natVal (Proxy @exp))) # pto y
  pmod = phoistAcyclic $ plam $ \x y -> pcon . PFixed $ pmod # pto x # pto y
  pquot =
    phoistAcyclic $
      plam $ \x y ->
        pcon . PFixed $
          pquot # (pto x * pconstant (10 ^ natVal (Proxy @exp))) # pto y
  prem =
    phoistAcyclic $ plam $ \x y -> pcon . PFixed $ prem # pto x # pto y

{- | Change decimal point.

 *Caution* This function will drop precision when converting from more
 decimal points to less decimal points.

 For example, converting `1.234 :: Fixed 3` into `Fixed 1` will drop
 hundredth and thousandth place value and will give `1.2 :: Fixed 1`.

 There is not data loss going from small decimal points to big decimal points,
 but they will take up more memory.

 @since 3.11.0
-}
pconvertExp ::
  forall (exp2 :: Natural) (exp1 :: Natural) (s :: S).
  (KnownNat exp1, KnownNat exp2) =>
  Term s (PFixed exp1 :--> PFixed exp2)
pconvertExp = phoistAcyclic $
  plam $ \z ->
    let ediff = (natVal (Proxy @exp2) - natVal (Proxy @exp1))
     in pcon . PFixed $
          if ediff > 0
            then pto z * pconstant (10 ^ abs ediff)
            else pdiv # pto z #$ pconstant (10 ^ abs ediff)

{- | Convert 'PFixed' into 'PInteger'.

 *Caution* This will drop all decimal point values. For example,
 converting `12.345 :: Fixed 3` will give `12 :: Integer`. Pay close
 attention using this function.

 If one needs to retrive all decimal point values, use `pto` instead.

 @since 3.11.0
-}
pfromFixed ::
  forall (exp :: Natural) (s :: S).
  KnownNat exp =>
  Term s (PFixed exp :--> PInteger)
pfromFixed = phoistAcyclic $
  plam $ \z -> pdiv # pto z #$ pconstant (10 ^ natVal (Proxy @exp))

{- | Convert 'PInteger' into 'PFixed'.

 There is no dataloss, but takes more memory.

 @since 3.11.0
-}
ptoFixed ::
  forall (exp :: Natural) (s :: S).
  KnownNat exp =>
  Term s (PInteger :--> PFixed exp)
ptoFixed = phoistAcyclic $
  plam $ \z ->
    pcon
      . PFixed
      $ z * pconstant (10 ^ natVal (Proxy @exp))

{- | Convert 'PFixed' into 'PRational'.

 Note, it will *not* simplify. There is no data loss.

 @since 3.11.0
-}
ptoRational ::
  forall (exp :: Natural) (s :: S).
  KnownNat exp =>
  Term s (PFixed exp :--> PRational)
ptoRational = phoistAcyclic $
  plam $ \z -> pto z #% pconstant (10 ^ natVal (Proxy @exp))

{- | Convert 'PRational' into 'PFixed'.

 Note, there can be data loss when decimal places are not enough to present
 rational value in full.

 @since 3.11.0
-}
pfromRational ::
  forall (exp :: Natural) (s :: S).
  KnownNat exp =>
  Term s (PRational :--> PFixed exp)
pfromRational = phoistAcyclic $
  plam $
    flip pmatch $ \(PRational num denom) ->
      pcon . PFixed $ pdiv # (num * (10 ^ natVal (Proxy @exp))) # pto denom

{- | Make 'PFixed' from 'PInteger'.

 *Caution* 'PInteger' given will not be equal to returned 'PFixed'.
 Input ignores decimal point: `1234 :: Integer` will return `12.34 :: Fixed 2`.

 @since 3.11.0
-}
punsafeMkFixed ::
  forall (exp :: Natural) (s :: S).
  Term s (PInteger :--> PFixed exp)
punsafeMkFixed = plam punsafeCoerce
