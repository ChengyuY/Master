module PolySpec where

import Test.Hspec

import Poly
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

showSpec = do
  describe "showPoly" $ do

    it "show a Poly" $ do
      showPoly Poly.exemple1
        `shouldBe` "1.X**4+0.X**3+1.X**2+0.X**1+4.X**0"

plusSpec = do
  describe "plusPoly" $ do

    it "returns the result of addition " $ do
      showPoly (plusP Poly.exemple3 Poly.exemple2)
        `shouldBe` "8.X**0"

minusSpec = do
  describe "minusPoly" $ do

    it "returns the result of sub" $ do
      showPoly (minusP Nul Poly.exemple2)
        `shouldBe` "-1.X**2+0.X**1+-4.X**0"

absSpec = do
  describe "absPoly" $ do

    it "returns the result of the abs method " $ do
      showPoly (absolP Poly.exemple3)
        `shouldBe` "1.X**2+0.X**1+4.X**0"

negaSpec = do
  describe "negaPoly" $ do

    it "returns the result of nega " $ do
      showPoly (negaP Poly.exemple2)
        `shouldBe` "-1.X**2+0.X**1+-4.X**0"

multSpec = do
  describe "multPoly" $ do

    it "returns the result of multication " $ do
      showPoly (multP Poly.exemple1 Poly.exemple2)
        `shouldBe` "1.X**6+0.X**5+5.X**4+0.X**3+8.X**2+0.X**1+16.X**0"

deriveSpec = do
  describe "derivePoly" $ do

    it "returns the result of derive " $ do
      show (deriveP (Poly.exemple2*Poly.exemple1))
        `shouldBe` "6.X**5+0.X**4+20.X**3+0.X**2+16.X**1+0.X**0"

equalSpec = do
  describe "equalPoly" $ do

    it "returns the result of equal " $ do
      equalP (deriveP (Poly.exemple2*Poly.exemple1)) (plusP (multP Poly.exemple1 (deriveP Poly.exemple2)) (multP Poly.exemple2 (deriveP Poly.exemple1)))
        `shouldBe` True 


      

engineSpec = do
    showSpec
    plusSpec
    minusSpec
    absSpec
    negaSpec
    multSpec
    deriveSpec
    equalSpec