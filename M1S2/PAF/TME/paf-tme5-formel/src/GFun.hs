{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GFun where

import Data.Map (Map)
import qualified Data.Map as Map

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Serie a = Z a (Serie a)


uns :: Serie Integer
uns = Z 1 uns


-- pour observer les series
prend :: Int -> Serie a -> [a]
prend 0 (Z a _)= []
prend n (Z a rest) = [a] <> prend (n-1) rest

-- >>> prend 10 uns
-- [1,1,1,1,1,1,1,1,1,1]

-- >>> show 1 <> show 2 <> show 3 <> " ..."
-- "123 ..."

showSerie :: Show a => Serie a  -> String 
showSerie serie = 
    sss serie 0 []
    where 
        sss (Z a rest) tmp str = 
            if tmp == 10 then str <> " ..."  
            else sss rest (tmp+1) (str <> show a <> ".z^" <> show tmp <> "+")

-- >>> showSerie uns 
-- "1.z^0+1.z^1+1.z^2+1.z^3+1.z^4+1.z^5+1.z^6+1.z^7+1.z^8+1.z^9+ ..."

instance Show a => Show (Serie a) where
    show = showSerie

gmapS :: (a -> b) -> Serie a -> Serie b
gmapS f (Z a rest) = Z (f a) (gmapS f rest)

-- >>> show (gmapS (*2) uns)
-- "2.z^0+2.z^1+2.z^2+2.z^3+2.z^4+2.z^5+2.z^6+2.z^7+2.z^8+2.z^9+ ..."

plusS :: Num a => Serie a -> Serie a -> Serie a
plusS (Z a rest1) (Z b rest2) = Z (a+b) (plusS rest1 rest2)

-- >>> show (plusS uns uns)
-- "2.z^0+2.z^1+2.z^2+2.z^3+2.z^4+2.z^5+2.z^6+2.z^7+2.z^8+2.z^9+ ..."

minusS :: Num a => Serie a -> Serie a -> Serie a
minusS (Z a rest1) (Z b rest2) = Z (a-b) (minusS rest1 rest2)

-- >>> show( minusS uns uns )
-- "0.z^0+0.z^1+0.z^2+0.z^3+0.z^4+0.z^5+0.z^6+0.z^7+0.z^8+0.z^9+ ..."

multS :: Num a => Serie a -> Serie a -> Serie a
multS (Z a rest1) (Z b rest2) = Z (a*b) (plusS (plusS (gmapS (*a) rest2)  (gmapS (*b) rest1))  (Z 0 (multS rest1 rest2)))

-- >>> show(multS uns uns)
-- "1.z^0+2.z^1+3.z^2+4.z^3+5.z^4+6.z^5+7.z^6+8.z^7+9.z^8+10.z^9+ ..."

absolS :: Num a => Serie a -> Serie a
absolS = gmapS abs

-- >>> show(absolS uns)
-- "1.z^0+1.z^1+1.z^2+1.z^3+1.z^4+1.z^5+1.z^6+1.z^7+1.z^8+1.z^9+ ..."

negaS :: Num a => Serie a -> Serie a
negaS = gmapS (*(-1))

-- >>> show (negaS uns)
-- "-1.z^0+-1.z^1+-1.z^2+-1.z^3+-1.z^4+-1.z^5+-1.z^6+-1.z^7+-1.z^8+-1.z^9+ ..."

fInt :: Num a => a -> Serie a
fInt n = Z n (fInt n)


-- >>> show (fInt 10)
-- "10.z^0+10.z^1+10.z^2+10.z^3+10.z^4+10.z^5+10.z^6+10.z^7+10.z^8+10.z^9+ ..."

instance Num a => Num(Serie a) where
    (+) = plusS
    (-) = minusS
    (*) = multS
    abs = absolS
    negate = negaS
    --signum a
    --fromInteger = fInt

-- >>> show (fromInteger 5)
-- "5"

deriveS :: Num a => Eq a => Serie a -> Serie a
deriveS serie = 
    der serie 0
    where
        der (Z a rest) n = 
            if n == 0 then der rest (n+1)
            else Z (a*n) (der rest (n+1))

-- >>> show (deriveS (uns*uns))
-- "2.z^0+6.z^1+12.z^2+20.z^3+30.z^4+42.z^5+56.z^6+72.z^7+90.z^8+110.z^9+ ..."

instance Functor Serie where
    fmap f serie = gmapS f serie

-- >>> fmap (*10) uns
-- 10.z^0+10.z^1+10.z^2+10.z^3+10.z^4+10.z^5+10.z^6+10.z^7+10.z^8+10.z^9+ ...

equalSerie :: Eq a => Serie a -> Serie a -> Bool
equalSerie s1 s2 = 
    eq s1 s2 10
    where
        eq (Z a rest1) (Z b rest2) n = 
            if n == 0 then a == b
            else (a == b) && eq rest1 rest2 (n-1)

instance Eq a => Eq (Serie a) where
    (==) s1 s2 = equalSerie s1 s2

-- >>> (uns + uns) == (gmapS (*2) uns)
-- True



