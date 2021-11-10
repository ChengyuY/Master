{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module EnviSpec where

import qualified Data.Set as Set 

import qualified Data.Sequence as Seq

import qualified Data.Map.Strict as M

import Test.Hspec

import Niveau

import Control.Exception (evaluate)

import Lemming

import Coord

import Environnement

ide :: Int 
ide = 4

lem1 :: Entite 
lem1 = Lem ide (Marcheur Droite (C 2 1) Mar)

envi1 :: Envi
envi1 = envide 13 10

enviTest  = do
  
  describe "Tests environnement" $ do
    it "Environnement saint" $ do
      prop_envi_inv envi1 `shouldBe` True
    it "Apres add entite" $ do
      prop_post_add ide (addEntite lem1 envi1)`shouldBe` True 
      prop_envi_inv envi1 `shouldBe` True
    it "Apres enlever entite" $ do
      prop_post_enleve ide (enleveEnvi ide envi1)`shouldBe` True 
      prop_envi_inv envi1 `shouldBe` True
    

engineSpec = do
  enviTest