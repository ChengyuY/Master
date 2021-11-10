module QuickCheckNiveau where

import Test.Hspec
import Test.QuickCheck

import Coord
import Niveau
import System.Random


prop_revapp :: Eq a => [a] -> [a] -> Bool
prop_revapp xs ys = reverse (xs <> ys) == reverse ys <> reverse xs

revrevSpec = do
  describe "reverse" $ do
    context "when used with ints" $ do
      it "is idempotent" $ property $          
        \xs ys ->
        prop_revapp (xs :: [Int]) ys 

genNiveau :: Gen Niveau 
genNiveau = do 
    seed <- choose (0, 1000)
    let gen = mkStdGen seed 
    return (generateNiveauVerified gen)
    
instance Arbitrary Niveau where
    arbitrary = genNiveau

instance Arbitrary Coord where
    arbitrary =  do
        x <- choose (1,10)
        y <- choose (1,10)
        return (C x y)

prop_genNiveau_inv :: Property 
prop_genNiveau_inv = forAll genNiveau $ prop_niveauInclusion


genNiveauSpec = do
  describe "Niveau générateur QuickCheck invariant" $ do
    it "Teste l'invariant pour les cartés générées aléatoirement" $ 
      property prop_genNiveau_inv






