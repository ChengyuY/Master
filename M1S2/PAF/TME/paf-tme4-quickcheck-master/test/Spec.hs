import Test.Hspec

import RevrevSpec as RR
import SplitSpec as SP
import BridgeSpec as BS

main :: IO ()
main = hspec $ do
  -- revrev
  RR.revrevSpec
  RR.revappSpec
  -- split
  SP.splitSpec0
  SP.splitSpec1
  SP.splitSpec2
  SP.splitSpec3
  -- bridge
  BS.bridgeSpecInit
  BS.bridgeSpecGenOk
  --BS.bridgeSpecGenFree
  BS.enterToIslandSpec
  BS.leaveToIslandSpec
  BS.enterFromIslandSpec
  BS.leaveFromIslandSpec
