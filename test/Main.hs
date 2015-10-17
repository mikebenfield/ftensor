import Test.Tasty

import qualified Tests.Math.Ftensor.Lib.Array
import qualified Tests.Math.Ftensor.General
import qualified Tests.Math.Ftensor.SizedList

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Tests.Math.Ftensor.Lib.Array.tests
    , Tests.Math.Ftensor.General.tests
    , Tests.Math.Ftensor.SizedList.tests
    ]
