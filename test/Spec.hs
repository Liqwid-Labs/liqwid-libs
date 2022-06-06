import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain $
        testGroup "test suiite" []
