module Test where

import PAF_TME8_GenMonad
import Test.QuickCheck
import System.Random (Random)


-- Je sais pas qu'est-ce que se passe sur mon ord 
-- il affiche toujours des warning quand j'utilse choose
-- mais il fait bon dans ce fichier
-- >>> samples 5 (arbitrary :: Gen [Integer])
-- [[],[],[-1,-4],[-6,-6,-5,6],[-2,4,2,1,5]]

-- >>> samples 5 (arbitrary :: Gen (Maybe Int))
-- [Just 0,Just 2,Just 4,Just (-1),Just 2]

-- >>> samples 5 (arbitrary :: Gen (Double, Bool))
-- [(0.0,False),(1.74469310325273,False),(-0.12444878490421825,True),(2.3184605946936165,True),(-1.1765206638189418,True)]

-- >>> samples 5 (arbitrary :: Gen [Either Int (Maybe Bool)])
-- [[],[Left 2],[Left (-3),Right (Just False),Left (-3),Left 4],[Right (Just False),Left 5,Right Nothing,Left (-3),Left 2,Right (Just False)],[Left 1,Left 1,Right Nothing]]

-- >>> sample' $ choose ((1,10)::(Integer, Integer))
-- [3,4,8,7,7,6,3,7,4,9,10]

-- >>> sample' $ choose ('a', 'z')
-- "ccvwickhtlv"

-- >>> sample' $ chooseNat (10::Integer)
-- [6,1,8,4,3,8,1,7,6,10,7]

-- >>> sample' $ elements [5, 10, 15]
-- [15,15,5,5,5,15,15,10,10,15,5]

-- >>> samples 5 $ listOf (chooseNat (10::Integer))
-- [[],[],[5,1,4,5],[6,6,6,2,6,4],[8]]

-- >>> sample' $ elements ['a' .. 'z']
-- "appkjqjyajl"

-- >>> samples 5 $ resize 5 $ listOf (chooseNat (10::Integer))
-- [[8,5,8,2,7],[4,10],[2],[8,1],[3,9,3,5]]

-- >>> sample' $ resize 10 $ choose ('a', 'z')
-- "okfdbiztuib"

-- >>> sample' $ oneof [choose (1::Integer, 10::Integer), choose (10::Integer, 20::Integer), choose (20::Integer, 30::Integer)]
-- [1,17,15,11,3,8,17,14,30,15,26]

-- >>> sample' $ frequency [(60, choose (1::Integer, 10::Integer)), (30, choose (10::Integer, 20::Integer)), (10, choose (20::Integer, 30::Integer))]
-- [4,2,9,12,16,8,7,15,1,20,5]

-- >>> checkFrequency $ resize 10000 $ listOf genFreq
-- (59.95328781890047,30.039525691699605,10.007186489399928)

-- >>> sample' $ chooseNat (20::Integer) `suchThat` even
-- [4,16,10,12,12,4,14,12,18,20,16]

-- >>> sample' $ (pure 10 :: Gen Int)
-- [10,10,10,10,10,10,10,10,10,10,10]

-- >>> sample' $ chooseInv $ chooseNat (10::Integer)
-- [-8,-9,-6,-5,-3,-9,-3,-7,-5,-10,-7]

-- >>> sample' $ chooseInv2 $ chooseNat (10::Integer)
-- [-4,-1,-2,-5,-10,-4,-10,-8,-4,-7,-6]

-- >>> samples 5 $ genPair (chooseNat (10::Integer)) (chooseNat (10::Integer))
-- [(4,4),(7,2),(5,10),(4,10),(5,8)]

-- >>> samples 5 $ genPair2 (chooseNat (10::Integer)) (chooseNat (10::Integer))
-- [(4,2),(3,6),(1,9),(3,4),(8,10)]

-- >>> samples 1 $ genPersonne
-- [Personne {nom = "bnqqdohkta", prenom = "envdnzqrhm", age = 22}]

-- >>> samples 5 $ genMaybe $ choose (1::Integer, 10::Integer)
-- [Just 3,Nothing,Just 9,Just 2,Just 3]

-- >>> samples 5 genGeom
-- [Circle {rayon = 9},Rect {longueur = 41, largeur = 16},Circle {rayon = 8},Rect {longueur = 56, largeur = 37},Rect {longueur = 57, largeur = 9}]

-- >>> samples 5 $ resize 10 $ genNat
-- [S (S (S (S (S (S (S Z)))))),S (S Z),S (S (S (S (S (S (S (S Z))))))),S (S (S (S (S (S Z))))),S (S (S Z))]

-- >>> samples 5 $ listOfSize (chooseNat (10::Integer)) 5
-- [[2,10,2,10,4],[3,5,3,2,9],[3,3,6,4,3],[3,9,5,5,2],[6,2,6,3,7]]

-- >>> samples 5 $ resize 5 $ sizedList $ chooseNat (10::Integer)
-- [[9,2,1,3,8],[2,3,5,3,6],[2,6,2,7,8],[1,7,1,7,9],[8,10,8,4,3]]

-- >>> samples 5 $ resize 10 $ sizedList' $ chooseNat (10::Integer)
-- [[8,9,10,3,5,9,1,5,8,9],[2,2,1,1,6,2,4,9,9,7],[9,6,2,1,5,3,6,8,8,3],[7,5,4,3,5,4,7,5,2,10],[2,5,5,7,1,10,1,4,8,9]]

-- >>> samples 1 $ genBinTreeNaive (choose (1::Integer, 5::Integer)) 5
-- [Node 4 (Node 2 Tip Tip) (Node 1 Tip (Node 4 (Node 4 Tip (Node 4 (Node 2 Tip Tip) (Node 4 Tip Tip))) (Node 5 Tip (Node 3 (Node 4 Tip Tip) (Node 5 Tip Tip)))))]

-- >>> samples 1 $ resize 5 $ genBinTree (choose (1::Integer, 5::Integer))
-- [Node 3 (Node 2 Tip (Node 5 (Node 4 Tip (Node 1 (Node 2 (Node 4 Tip Tip) (Node 2 Tip Tip)) (Node 1 Tip Tip))) (Node 3 Tip Tip))) (Node 4 Tip Tip)]
