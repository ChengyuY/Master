{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poly where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

data Poly a = Nul 
    | Poly (Int, Map Int Int) 

exemple1 :: Poly Int
exemple1 = Poly (4 , Map.fromList [(0, 4),(1, 0),(2, 1),(3, 0),(4, 1)])

exemple2 :: Poly Int
exemple2 = Poly (2 , Map.fromList [(0, 4),(1, 0),(2, 1)])

exemple3 :: Poly Int
exemple3 = Poly (2 , Map.fromList [(0, 4),(1, 0),(2, -1)])

-- >>> :t exemple1
-- exemple1 :: Poly Int

polyInv :: (Num a,Eq a) => Poly a -> Bool 
polyInv Nul = True
polyInv (Poly (d,m)) = fst (Map.findMax m) == d && snd (Map.findMax m) /= 0 

showPoly :: (Show a) => Poly a -> String 
showPoly Nul = "()"
showPoly (Poly (d,m))= 
    sp (Poly (d,m)) d []
    where 
        sp (Poly (d,m)) n str = 
            if n == 0 then str <> show (m Map.! n) <> ".X**" <> show n
            else str <> show (m Map.! n) <> ".X**" <> show n <> "+" <> sp (Poly (d,m)) (n-1) str

-- >>> showPoly exemple1
-- "1.X**4+0.X**3+1.X**2+0.X**1+4.X**0"

--Verifier que les polynomes sont dans le forme correcte , sinon les change.
polyVer :: Poly a -> Poly a
polyVer (Poly (d,m)) =
    if polyInv (Poly (d,m)) then Poly (d,m)
    else polyVer (Poly (d-1,snd (Map.deleteFindMax m)))

-- >>>  showPoly (polyVer (plusP exemple2 exemple3))
-- "8.X**0"

plusP :: Num a => Poly a -> Poly a -> Poly a
plusP Nul poly = poly
plusP poly Nul = poly
plusP (Poly (d1,m1)) (Poly (d2,m2)) = 
    case max d1 d2 of
         d1 -> polyVer (Poly (d1 , Map.unionWith (+) m1 m2))
         d2 -> polyVer (Poly (d2 , Map.unionWith (+) m1 m2))

-- >>> showPoly (plusP exemple3 exemple2)
-- "8.X**0"

negaP :: Num a => Poly a -> Poly a
negaP (Poly (d,m)) = 
    Poly (d , Map.unionWith (-) (Map.fromList (makeP d)) m)
    where 
        makeP n = 
            if n == 0 then [(0,0)]
            else [(n,0)] <> makeP (n-1)

-- >>> showPoly (negaP exemple2)
-- "-1.X**2+0.X**1+-4.X**0"


minusP :: Num a => Poly a -> Poly a -> Poly a
minusP poly Nul = poly 
minusP Nul poly = negaP poly
minusP (Poly (d1,m1)) (Poly (d2,m2)) = 
    case max d1 d2 of
         d1 -> polyVer (Poly (d1 , Map.unionWith (-) m1 m2))
         d2 -> polyVer (Poly (d2 , Map.unionWith (-) m1 m2))

-- >>>showPoly (minusP Nul exemple2)
-- "-1.X**2+0.X**1+-4.X**0"


absolP :: Num a => Poly a -> Poly a
absolP (Poly (d,m)) = 
    Poly (d , Map.fromList (makeP d))
    where 
        makeP :: Int -> [(Int,Int)]
        makeP n
            | n == 0 =
                if m Map.! n >= 0 then [(0,m Map.! n)]
                else  [(0,- (m Map.! n))]
            | m Map.! n >= 0 = [(n,m Map.! n)] <> makeP (n-1) 
            | otherwise  = [(n,- (m Map.! n))] <> makeP (n-1)

-- >>> showPoly (absolP exemple3)
-- "1.X**2+0.X**1+4.X**0"


subMult :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
subMult (puis,cn) ys = case ys of
    [] -> []
    (puis1,cn1):rest -> [(puis+puis1,cn*cn1)]<>subMult (puis,cn) rest

-- >>> subMult (1,3) [(1,1),(2,3)]
-- [(2,3),(3,9)]

multP :: Num a => Poly a -> Poly a -> Poly a
multP Nul _ = Nul
multP _ Nul = Nul
multP (Poly (d1,m1)) (Poly (d2,m2)) = 
    polyVer (Poly (d1+d2,mult m1 (Map.toList m2) d1 (Map.fromList [(0,0)])))
    where 
        mult :: Map Int Int -> [(Int,Int)] -> Int -> Map Int Int -> Map Int Int
        mult m1 l2 n m =
            if n == 0 then Map.fromList (subMult (n,m1 Map.! n) l2)
            else Map.unionWith (+) (Map.fromList (subMult (n,m1 Map.! n) l2)) (mult m1 l2 (n-1) m) 


-- >>> showPoly (multP exemple1 exemple2)
-- "1.X**6+0.X**5+5.X**4+0.X**3+8.X**2+0.X**1+16.X**0"

instance Show a => Show (Poly a) where
    show = showPoly

instance Num a => Num(Poly a) where
    (+) = plusP
    (-) = minusP
    (*) = multP
    abs = absolP
    negate = negaP
    --signum
    --fromInteger

-- >>> show (exemple1*exemple2)
-- "1.X**6+0.X**5+5.X**4+0.X**3+8.X**2+0.X**1+16.X**0"

equalP :: Eq a => Poly a -> Poly a -> Bool
equalP p1 p2 = 
    eq p1 p2 0
    where
        eq (Poly (d1,m1)) (Poly (d2,m2)) n = 
            if n == d1 then m1 Map.! n == m2 Map.! n
            else (m1 Map.! n == m2 Map.! n) && eq (Poly (d1,m1)) (Poly (d2,m2)) (n+1)

-- >>> equalP exemple1 exemple1
-- True

deriveP :: Num a => Eq a => Poly a -> Poly a
deriveP (Poly (d,m)) =
    Poly ( d-1,der (Poly (d,m)) 1)
    where
        der :: Poly a -> Int -> Map Int Int
        der (Poly (d,m)) n = 
            if n == d then Map.fromList [(n-1,m Map.! n * n)]
            else Map.unionWith (+)  (Map.fromList [(n-1,m Map.! n * n)]) (der (Poly (d,m)) (n+1))

-- >>> show (deriveP (exemple2*exemple1))
-- "6.X**5+0.X**4+20.X**3+0.X**2+16.X**1+0.X**0"

-- >>> show ((exemple1*(deriveP exemple2))+(exemple2*(deriveP exemple1)))
-- "6.X**5+0.X**4+20.X**3+0.X**2+16.X**1+0.X**0"

-- >>> equalP (deriveP (exemple2*exemple1)) ((exemple1*(deriveP exemple2))+(exemple2*(deriveP exemple1)))
-- True

split :: Char -> String -> [String]
split c s = case break (==c) s 
    of 
  (ls, "") -> [ls]
  (ls, x:rest) -> ls : split c rest 

-- >>> split '+' (show exemple1)
-- ["1.X**4","0.X**3","1.X**2","0.X**1","4.X**0"]

-- >>> show (read "1"::Int)
-- "1"

readP :: (Read a,Show a) => String -> Poly a
readP "()" = Nul
readP str = 
    Poly (rp (split '+' str) 0 (0,Map.fromList [(0,0)]))
    where 
        rp :: [String] -> Int  -> (Int , Map Int Int) -> (Int , Map Int Int)
        rp ls n m = 
            case ls of
                s:rest -> (n + 1, Map.insert (read [head s]::Int) (read [last s]::Int) (snd m))
                [s] -> (n + 1 , Map.insert (read [head s]::Int) (read [last s]::Int) (snd m))

-- >>> show (readP "()"::Poly Int)
-- "()"



-- >>> show (readP "6.X**5+0.X**4+20.X**3+0.X**2+16.X**1+0.X**0"::Poly Int)
-- Ambiguous type variable ‘a0’ arising from a use of ‘show’
-- prevents the constraint ‘(Show a0)’ from being solved.
-- Probable fix: use a type annotation to specify what ‘a0’ should be.
-- These potential instances exist:
--   instance [safe] Show FailureReason -- Defined in ‘Test.HUnit.Lang’
--   instance [safe] Show HUnitFailure -- Defined in ‘Test.HUnit.Lang’
--   instance [safe] Show Result -- Defined in ‘Test.HUnit.Lang’
--   ...plus 237 others
--   (use -fprint-potential-instances to see them all)
-- No instance for (Read a) arising from a use of ‘readP’
-- Possible fix:
--   add (Read a) to the context of
--     an expression type signature:
--       forall a. Poly a














