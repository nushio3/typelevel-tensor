import           Control.Exception (finally)
import           Control.Monad
import           Test.Framework (defaultMain)
-- Actual tests
import Test.Tensor

main :: IO ()
main = defaultMain [testTensor]

