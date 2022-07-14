import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

import qualified Spec.Api.V1.Value as Value
import qualified Spec.Extra.List as List
import qualified Spec.Extra.Map as Map
import qualified Spec.Extra.Traversable as Traversable

--------------------------------------------------------------------------------

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain $
        testGroup
            "test suite"
            [ testGroup "list utilities" List.tests
            , testGroup "map utilities" Map.tests
            , testGroup "value utilities" Value.tests
            , testGroup "traversable" Traversable.tests
            ]
