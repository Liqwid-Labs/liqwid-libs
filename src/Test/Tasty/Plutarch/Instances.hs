-- Our whole goal is orphans
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Module: Test.Tasty.Plutarch.Instances
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Instances for QuickCheck type classes for various Plutarch types, and some
 helper wrappers.
-}
module Test.Tasty.Plutarch.Instances (
    -- * Helper newtypes
    BS64 (..),
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHC.Exts (fromList, fromListN, toList)
import PlutusCore.Data (Data (B, Constr, I, List, Map))
import Test.QuickCheck (
    Arbitrary (arbitrary, shrink),
    CoArbitrary (coarbitrary),
    Function (function),
    Gen,
    NonNegative (NonNegative),
    chooseInt,
    functionMap,
    getNonNegative,
    oneof,
    scale,
    shrinkList,
    sized,
    variant,
    vectorOf,
 )

-- | @since 1.0.0
instance Arbitrary Data where
    arbitrary = sized genData
    shrink = shrinkData

-- | @since 1.0.0
instance CoArbitrary Data where
    coarbitrary dat = case dat of
        I i -> variant (0 :: Int) . coarbitrary i
        B bs -> variant (1 :: Int) . coarbitrary (BS64 bs)
        List ds -> variant (2 :: Int) . coarbitrary ds
        Map kvs -> variant (3 :: Int) . coarbitrary kvs
        Constr ix ds -> variant (4 :: Int) . coarbitrary ix . coarbitrary ds

-- | @since 1.0.0
instance Function Data where
    function = functionMap into outOf
      where
        into ::
            Data ->
            Either
                (Either Integer BS64)
                ( Either
                    [Data]
                    ( Either
                        [(Data, Data)]
                        (Integer, [Data])
                    )
                )
        into = \case
            I i -> Left . Left $ i
            B bs -> Left . Right . BS64 $ bs
            List ds -> Right . Left $ ds
            Map kvs -> Right . Right . Left $ kvs
            Constr ix ds -> Right . Right . Right $ (ix, ds)
        outOf ::
            Either
                (Either Integer BS64)
                ( Either
                    [Data]
                    ( Either
                        [(Data, Data)]
                        (Integer, [Data])
                    )
                ) ->
            Data
        outOf = \case
            Left (Left i) -> I i
            Left (Right (BS64 bs)) -> B bs
            Right (Left ds) -> List ds
            Right (Right (Left kvs)) -> Map kvs
            Right (Right (Right (ix, ds))) -> Constr ix ds

{- | Helper newtype to generate and shrink 'ByteString's whose length is no
 greater than 64.

 @since 1.0.0
-}
newtype BS64 = BS64 {unBS64 :: ByteString}
    deriving
        ( -- | @since 1.0.0
          Eq
        , -- | @since 1.0.0
          Ord
        )
        via ByteString
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

-- | @since 1.0.0
instance Arbitrary BS64 where
    arbitrary =
        BS64 . uncurry fromListN <$> do
            len <- chooseInt (0, 64)
            (len,) <$> vectorOf len arbitrary
    shrink (BS64 bs) = do
        let asList = toList bs
        asList' <- shrink asList
        pure . BS64 . fromList $ asList'

-- | @since 1.0.0
instance CoArbitrary BS64 where
    coarbitrary (BS64 bs) = case BS.uncons bs of
        Nothing -> variant (0 :: Int)
        Just (w8, bs') -> variant (1 :: Int) . coarbitrary (w8, BS64 bs')

-- | @since 1.0.0
instance Function BS64 where
    function = functionMap (toList . unBS64) (BS64 . fromList)

-- Helpers

genData :: Int -> Gen Data
genData size
    | size <= 0 = oneof [I <$> arbitrary, B . unBS64 <$> arbitrary]
    | otherwise =
        oneof
            [ I <$> arbitrary
            , B . unBS64 <$> arbitrary
            , List <$> scale (`quot` 2) arbitrary
            , Map <$> scale (`quot` 2) arbitrary
            , Constr <$> (getNonNegative <$> arbitrary)
                <*> scale (`quot` 2) arbitrary
            ]

shrinkData :: Data -> [Data]
shrinkData = \case
    I i -> I <$> shrink i
    B bs -> B . unBS64 <$> (shrink . BS64 $ bs)
    List ds -> List <$> shrinkList shrink ds
    Map kvs -> Map <$> shrinkList shrink kvs
    Constr ix ds ->
        Constr <$> (getNonNegative <$> (shrink . NonNegative $ ix))
            <*> shrinkList shrink ds
