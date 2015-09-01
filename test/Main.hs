
import Test.Tasty

import qualified Tests.Math.FTensor.Core2
import qualified Tests.Math.FTensor.Internal
-- import qualified Tests.Math.FTensor.Core
-- import qualified Tests.Math.FTensor.Simple
-- import qualified Tests.Math.FTensor.Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Tests.Math.FTensor.Core2.tests
    , Tests.Math.FTensor.Internal.tests
    ]
    -- [ Tests.Math.FTensor.Core.tests
    -- , Tests.Math.FTensor.Simple.tests
    -- , Tests.Math.FTensor.Types.tests
    -- ]
