import Test.Hspec
import Niveau
import NiveauSpec as NS
import QuickCheckNiveau as QCN
import Environnement
import EnviSpec as ES


main :: IO ()
main = hspec $ do
  -- revrev
  NS.engineSpec 
  ES.engineSpec
  QCN.genNiveauSpec 
