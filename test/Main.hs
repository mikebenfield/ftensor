
import Test.Tasty

--import qualified Tests.Math.FTensor.Core2
--import qualified Tests.Math.FTensor.Internal
import qualified Tests.Math.FTensor.Lib.Array
import qualified Tests.Math.FTensor.Internal.TaggedList
import qualified Tests.Math.FTensor.SizedList
-- import qualified Tests.Math.FTensor.Core
import qualified Tests.Math.FTensor.Simple
import qualified Tests.Math.FTensor.General
-- import qualified Tests.Math.FTensor.Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Tests.Math.FTensor.Lib.Array.tests
    , Tests.Math.FTensor.Internal.TaggedList.tests
    --[ Tests.Math.FTensor.Internal.tests
    , Tests.Math.FTensor.General.tests
    , Tests.Math.FTensor.Simple.tests
    , Tests.Math.FTensor.SizedList.tests
    ]
    -- [ Tests.Math.FTensor.Core2.tests
    -- [ Tests.Math.FTensor.Core.tests
    -- , Tests.Math.FTensor.Types.tests
    -- ]
