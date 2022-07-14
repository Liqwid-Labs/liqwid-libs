module Spec.Extra.Traversable (tests) where

import Plutarch (PCon (pcon), Term, plam, (#), (#$))
import Plutarch.Extra.Traversable (PTraversable (ptraverse))
import Plutarch.Prelude (
    PBool,
    PBuiltinList,
    PEq ((#==)),
    PInteger,
    PListLike (pcons, pnil),
    PMaybe (PJust),
    plift,
 )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)

li :: Term s (PBuiltinList PInteger)
li = pcons # 10 #$ pcons # 20 #$ pcons # 30 #$ pnil

sample :: Term s (PMaybe (PBuiltinList PInteger))
sample = ptraverse # (plam $ \x -> pcon $ PJust x) # li

trav :: Term s (PMaybe (PBuiltinList PInteger))
trav = pcon $ PJust li

shouldMatch :: forall s. Term s PBool
shouldMatch = sample #== trav

tests :: [TestTree]
tests =
    [ testCase "ptraverse" $
        assertBool "PListLike" (plift shouldMatch)
    ]
