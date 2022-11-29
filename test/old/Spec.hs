import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

import Spec.Extra.List qualified as List
import Spec.Extra.Map qualified as Map

-- import qualified Spec.Extra.MultiSig as MultiSig
import Spec.Extra.Traversable qualified as Traversable

--------------------------------------------------------------------------------

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "test suite"
      [ testGroup "list utilities" List.tests
      , testGroup "map utilities" Map.tests
      , testGroup "traversable" Traversable.tests
      ]
