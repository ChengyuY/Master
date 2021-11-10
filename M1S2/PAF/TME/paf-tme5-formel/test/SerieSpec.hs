module SerieSpec where

import Test.Hspec

import GFun
    ( deriveS,
      equalSerie,
      gmapS,
      minusS,
      multS,
      fInt,
      negaS,
      plusS,
      showSerie,
      uns )

showSpec = do
  describe "showSerie" $ do

    it "show a serie" $ do
      showSerie GFun.uns
        `shouldBe` "1.z^0+1.z^1+1.z^2+1.z^3+1.z^4+1.z^5+1.z^6+1.z^7+1.z^8+1.z^9+ ..."

plusSpec = do
  describe "plusSerie" $ do

    it "returns the result of addition " $ do
      showSerie (plusS GFun.uns GFun.uns )
        `shouldBe` "2.z^0+2.z^1+2.z^2+2.z^3+2.z^4+2.z^5+2.z^6+2.z^7+2.z^8+2.z^9+ ..."

minusSpec = do
  describe "minusSerie" $ do

    it "returns the result of sub" $ do
      showSerie (minusS GFun.uns GFun.uns )
        `shouldBe` "0.z^0+0.z^1+0.z^2+0.z^3+0.z^4+0.z^5+0.z^6+0.z^7+0.z^8+0.z^9+ ..."

gmapSpec = do
  describe "gmapSerie" $ do

    it "returns the result of the map method " $ do
      showSerie (gmapS (*2) GFun.uns )
        `shouldBe` "2.z^0+2.z^1+2.z^2+2.z^3+2.z^4+2.z^5+2.z^6+2.z^7+2.z^8+2.z^9+ ..."

absSpec = do
  describe "absSerie" $ do

    it "returns the result of the abs method " $ do
      showSerie (abs GFun.uns)
        `shouldBe` "1.z^0+1.z^1+1.z^2+1.z^3+1.z^4+1.z^5+1.z^6+1.z^7+1.z^8+1.z^9+ ..."

negaSpec = do
  describe "negaSerie" $ do

    it "returns the result of nega " $ do
      showSerie (negaS GFun.uns )
        `shouldBe` "-1.z^0+-1.z^1+-1.z^2+-1.z^3+-1.z^4+-1.z^5+-1.z^6+-1.z^7+-1.z^8+-1.z^9+ ..."

multSpec = do
  describe "multSerie" $ do

    it "returns the result of multication " $ do
      showSerie (multS GFun.uns GFun.uns)
        `shouldBe` "1.z^0+2.z^1+3.z^2+4.z^3+5.z^4+6.z^5+7.z^6+8.z^7+9.z^8+10.z^9+ ..."

fromIntSpec = do
  describe "equalSerie" $ do

    it "returns the result of fromInteger " $ do
      showSerie (fInt 10)
        `shouldBe` "10.z^0+10.z^1+10.z^2+10.z^3+10.z^4+10.z^5+10.z^6+10.z^7+10.z^8+10.z^9+ ..."

deriveSpec = do
  describe "deriveSerie" $ do

    it "returns the result of derive " $ do
      showSerie (deriveS GFun.uns)
        `shouldBe` "1.z^0+2.z^1+3.z^2+4.z^3+5.z^4+6.z^5+7.z^6+8.z^7+9.z^8+10.z^9+ ..."

equalSpec = do
  describe "equalSerie" $ do

    it "returns the result of equal " $ do
      equalSerie (multS GFun.uns GFun.uns) (deriveS GFun.uns)
        `shouldBe` True 

      

engineSpec = do
    showSpec
    plusSpec
    minusSpec
    absSpec
    negaSpec
    gmapSpec
    multSpec
    fromIntSpec
    deriveSpec
    equalSpec