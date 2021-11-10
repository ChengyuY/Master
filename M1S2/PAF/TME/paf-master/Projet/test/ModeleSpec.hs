module ModeleSpec where

import qualified Data.Set as Set 

import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M

import Test.Hspec
import System.Random 
import Carte 
import Envi
import Model
import Keyboard (Keyboard)
import Keyboard 
import qualified Keyboard as K

import Control.Exception (evaluate)

carte1 = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nX X    XXX\nXE  X  XXX\nXXXXXXXXXX" ::Carte
envi1 = Envi M.empty
envi3 = ajout_env (C 2 2, (Treasure)) envi1
envi2 = ajout_env (C 2 8, (Player 2 100 False False)) envi3

kdb1 = K.createKeyboard

model1 = Model carte1 envi2 (mkStdGen 40) "" kdb1



modTest1  = do
  
  describe "Tests modele" $ do
    it "Model invariant" $ do
      prop_modele_inv model1 `shouldBe` True
    it "pre_moveGenerique" $ do
      pre_moveGenerique model1 (C 1 0) `shouldBe` True
    it "post_moveGenerique" $ do
      let model2 = moveGenerique model1 (C 1 0)
      post_moveGenerique model2 (C 3 8) `shouldBe` True
      prop_modele_inv model2 `shouldBe` True


  

    


engineSpec = do
  modTest1