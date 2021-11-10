{-

1) pour tester une opération vous devez construire une "situation initale correcte"
il vous faut générer un e :: EtatDuJeu et un m :: Mob tels que
a) prop_inv_EtatDuJeu e soit True
b) prop_pre_ajouteMob e m  soit True
2) vous calculez le résultat de ajouteMob e m
3) vous testez l'invariant prop_inv_EtatDuJeu (ajouteMob e m) et la postcondition (prop_post_ajouteMob e m)
-}




module GameStateSpec where

import qualified Data.Set as Set 

import qualified Data.Sequence as Seq

import Envi
import Test.Hspec
import Carte 
import System.Random 
import Foreign.C.Types (CDouble (..) )


import State 

import Control.Exception (evaluate)


carte1 = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nX X    XXX\nXE  X  XXX\nXXXXXXXXXX" ::Carte


gameState1 = initGameState carte1 (mkStdGen 40) (10.0::CDouble)

add_entity_state_test  = do
  describe "Tests sur Game state" $ do
    let mob = (Mob 1 100 30) 
    let coord = (C 3 4) 
    it "Game state invariant " $ do
      prop_state_inv gameState1 `shouldBe` True
    it "Precondition add entity " $ do
      prop_pre_add_entity_state gameState1 coord mob `shouldBe` True
    it "Post condition add entity " $ do
      let env_res = add_entity_state gameState1 coord mob
      prop_post_add_entity_state env_res coord mob `shouldBe` True
      prop_state_inv env_res `shouldBe` True



engineSpec = do
  add_entity_state_test