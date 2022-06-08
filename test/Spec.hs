import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

import qualified Spec.Extra.List as List
import qualified Spec.Extra.Map as Map

--------------------------------------------------------------------------------

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain $
        testGroup
            "test suite"
            [ testGroup "list utilities" List.tests
            , testGroup "map utilities" Map.tests
            ]
