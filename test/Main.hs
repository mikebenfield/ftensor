import Test.Tasty

import qualified Tests.Math.FTensor.Lib.Array
import qualified Tests.Math.FTensor.General
import qualified Tests.Math.FTensor.SizedList

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Tests.Math.FTensor.Lib.Array.tests
    , Tests.Math.FTensor.General.tests
    , Tests.Math.FTensor.SizedList.tests
    ]
