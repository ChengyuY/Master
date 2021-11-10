import Test.Hspec
import SerieSpec as SE
import PolySpec as PE

main :: IO ()
main = hspec $ do
  SE.engineSpec
  PE.engineSpec
