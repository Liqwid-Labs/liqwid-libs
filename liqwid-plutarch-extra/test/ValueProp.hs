{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}

module ValueProp (tests) where

import Data.Maybe (catMaybes)
import Plutarch.Api.V2 (PMap (PMap))
import Plutarch.Builtin (ppairDataBuiltin)
import "liqwid-plutarch-extra" Plutarch.Extra.List (pfromList)
import Plutarch.Extra.Value (phasOnlyOneTokenOfCurrencySymbol, pvalue)
import Plutarch.Test.QuickCheck (fromPFun)
import Test.QuickCheck (
  Property,
  arbitrary,
  forAllShrinkShow,
  shrink,
 )
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

tests :: TestTree
tests =
  adjustOption go $
    testGroup
      "Value"
      [ testProperty "phasOnlyOneTokenOfCurrencySymbol" propOnlyOneToken
      ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 5000

propOnlyOneToken :: Property
propOnlyOneToken =
  let buildScenario (targetSym, targetToken, targetAmount) maybeAdaAmount aux =
        pfromData
          $ pvalue
            #$ pfromList
          $ catMaybes
            [ Just $
                ppairDataBuiltin
                  # pdata targetSym
                  # pdata
                    ( pcon . PMap $
                        pfromList
                          [ ppairDataBuiltin # pdata targetToken # pdata targetAmount
                          ]
                    )
            , fmap
                ( \adaAmount ->
                    ppairDataBuiltin
                      # pconstantData ""
                      # pdata
                        ( pcon . PMap $
                            pfromList
                              [ ppairDataBuiltin # (pconstantData "") # pdata adaAmount
                              ]
                        )
                )
                maybeAdaAmount
            , fmap
                ( \(auxSym, auxToken, auxAmount) ->
                    ppairDataBuiltin
                      # pdata auxSym
                      # pdata
                        ( pcon . PMap $
                            pfromList
                              [ ppairDataBuiltin # pdata auxToken # pdata auxAmount
                              ]
                        )
                )
                aux
            ]
   in forAllShrinkShow arbitrary shrink show $
        fromPFun $
          plam $ \targetSym targetToken targetAmount adaAmount auxSym auxToken auxAmount ->
            foldr
              (#&&)
              (pcon PTrue)
              [ pnot #$ phasOnlyOneTokenOfCurrencySymbol # targetSym # (pfromData $ pvalue # pconstant [])
              , (targetAmount #== 1)
                  #== ( phasOnlyOneTokenOfCurrencySymbol
                          # targetSym
                          # buildScenario (targetSym, targetToken, targetAmount) Nothing Nothing
                      )
              , pif (targetSym #== pconstant "") (pcon PTrue) $
                  ((targetAmount #== 1 #&& adaAmount #== 0))
                    #== ( phasOnlyOneTokenOfCurrencySymbol
                            # targetSym
                            # buildScenario (targetSym, targetToken, targetAmount) (Just adaAmount) Nothing
                        )
              , pif (targetSym #== pconstant "" #|| auxSym #== pconstant "") (pcon PTrue) $
                  pnot
                    # ( phasOnlyOneTokenOfCurrencySymbol
                          # targetSym
                          # buildScenario (targetSym, targetToken, targetAmount) (Just adaAmount) (Just (auxSym, auxToken, auxAmount))
                      )
              ]
