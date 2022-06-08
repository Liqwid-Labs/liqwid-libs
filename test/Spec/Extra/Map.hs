{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Extra.Map (tests) where

--------------------------------------------------------------------------------

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (
    Arbitrary (arbitrary),
    Property,
    Testable (property),
    elements,
    forAll,
    suchThat,
    testProperty,
    (.&&.),
 )

--------------------------------------------------------------------------------

import qualified Data.Map as M
import qualified Data.Set as S

--------------------------------------------------------------------------------

import Plutarch (PCon (pcon), Term, phoistAcyclic, plam, (#))
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant, plift)
import Plutarch.Maybe (PMaybe (..))
import qualified PlutusTx.AssocMap as AssocMap

--------------------------------------------------------------------------------

import Control.Monad.Cont (cont, runCont)

--------------------------------------------------------------------------------

import Plutarch.Extra.Map (pupdate, update)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests = [testProperty "'pupdate' updates assoc maps as 'update' does" prop_updateAssocMapParity]

--------------------------------------------------------------------------------

{- | Test the parity between 'update' and 'pupdate',
     also ensure they both work correctly.
-}
prop_updateAssocMapParity :: Property
prop_updateAssocMapParity =
    runCont
        ( do
            -- Generate a bunch unique keys.
            keys <-
                cont $
                    forAll $
                        arbitrary @(S.Set Integer) `suchThat` (not . S.null)

            -- Generate key-value pairs.
            kvPairs <- cont $ forAll $ mapM (\k -> (k,) <$> (arbitrary @Integer)) $ S.toList keys

            let initialMap = AssocMap.fromList kvPairs

                pinitialMap :: Term _ _
                pinitialMap = phoistAcyclic $ pconstant initialMap

                referenceMap = M.fromList kvPairs

            let pupdatedValue :: Maybe Integer -> Term _ (PMaybe PInteger)
                pupdatedValue updatedValue = phoistAcyclic $ case updatedValue of
                    Nothing -> pcon PNothing
                    Just v -> pcon $ PJust $ pconstant v

                -- Given the key and the updated value, test the parity
                parity key updatedValue =
                    let native = update (const updatedValue) key initialMap

                        plutarch :: AssocMap.Map Integer Integer
                        plutarch =
                            plift $
                                pupdate
                                    # plam (\_ -> pupdatedValue updatedValue)
                                    # pconstant key
                                    # pinitialMap

                        expected =
                            AssocMap.fromList $
                                M.toList $
                                    M.update (const updatedValue) key referenceMap
                     in expected == native
                            && expected == plutarch

            -- Select a key, generate a maybe value.
            -- The value at the key should be set to the new value or removed.
            (targetKey, _) <- cont $ forAll $ elements kvPairs
            updatedValue <- cont $ forAll $ arbitrary @(Maybe Integer)

            -- Now what if the key doesn't exist in our map?
            nonexistentKey <-
                cont $
                    forAll $
                        arbitrary @Integer `suchThat` (\k -> not $ S.member k keys)

            pure
                ( property (parity targetKey updatedValue)
                    .&&. property (parity nonexistentKey updatedValue)
                )
        )
        id
