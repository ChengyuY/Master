

module QuickCheckModel where

import Test.Hspec
import Test.QuickCheck

import QuickCheckState

import State
import Envi
import Model
import Carte

import qualified Data.Map.Strict as M

import System.Random
import Foreign.C.Types (CDouble (..) )
import qualified Keyboard as K




instance Arbitrary Modele where 
    arbitrary = do 
        etat <- (arbitrary :: Gen Etat)
        let envi = envi_tour etat 
        seed <- choose (0, 1000)
        let gen = mkStdGen seed in 
         return  (Model (carte_tour etat) envi gen "" K.createKeyboard)


prop_modele_inv_spec :: Property 
prop_modele_inv_spec = forAll (arbitrary::Gen Modele) $ prop_modele_inv


prop_modele_inv_spec2 :: Property 
prop_modele_inv_spec2 = forAll (arbitrary::Gen Modele) $ prop_modele_inv1

prop_modele_inv_spec3 :: Property 
prop_modele_inv_spec3 = forAll (arbitrary::Gen Modele) $ prop_modele_inv2

prop_modele_inv_spec4 :: Property 
prop_modele_inv_spec4 = forAll (arbitrary::Gen Modele) $ prop_modele_inv4

prop_genModele_inv = do
  describe "genTestCarte_QuickCheck" $ do
    it " invariant" $ 
      property prop_modele_inv_spec


prop_genModele_inv2 = do
  describe "prop_genModele_inv2" $ do
    it  "invariant" $ 
      property prop_modele_inv_spec2


prop_genModele_inv3 = do
  describe "prop_genModele_inv3" $ do
    it " invariant" $ 
      property prop_modele_inv_spec3


prop_genModele_inv4 = do
  describe "prop_genModele_inv4" $ do
    it " invariant" $ 
      property prop_modele_inv_spec4


atRandIndex :: [a] -> IO a  -- note that this is gives itself an IO action
atRandIndex l = do
    i <- randomRIO (0, length l - 1)
    return $ l !! i
    

genMove :: Gen Coord 
genMove = do 
      (x,y)<-elements [(1,0),(-1,0),(0,1),(0,-1)] 
      return (C x y)

prop_post_interact_object  :: Coord -> Modele  ->Entite -> Property 
prop_post_interact_object c@(C x y ) model  ent = 
    classify (x == 1 && y == 0 )  " Use right object" $ 
    classify (x == -1 && y == 0 )  "Use left object" $  
    classify (x == 0 && y ==1 )  "Use bottom object" $
    classify (x == 0 && y == -1 )  "Use top object" $ 
    let modelInteracted = interactObject model c in 
    property $ post_interact_object modelInteracted c ent

prop_post_interact_object_spec = do
  describe "Object interaction" $ do
    it " post condition verified" $ property  (forAll genMove (prop_post_interact_object ))


prop_post_porte_ouverte  :: Coord -> Modele  -> Property 
prop_post_porte_ouverte c@(C x y ) model= 
    classify (x == 1 && y == 0 )  " Open right door" $ 
    classify (x == -1 && y == 0 )  "Open left door" $  
    classify (x == 0 && y ==1 )  "Open bottom door" $
    classify (x == 0 && y == -1 )  "Open top door" $ 
    let modelInteracted = openDoorGenerique model c in 
    property $ post_openDoor modelInteracted c 



prop_post_open_door_spec = do
  describe "Door opening" $ do
    it " post condition verified" $ property  (forAll genMove (prop_post_porte_ouverte ))


