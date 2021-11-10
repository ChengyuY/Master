module Coord where

data Coord = C Int Int 
    deriving (Show,Eq)

creeCoord :: Int -> Int -> Coord
creeCoord = C 

gauche :: Coord -> Coord 
gauche (C x y) = C (x - 1) y

droite :: Coord -> Coord 
droite (C x y) = C (x + 1) y

bas :: Coord -> Coord 
bas (C x y) = C x (y - 1)

haut :: Coord -> Coord
haut (C x y) = C x (y + 1)

instance Ord Coord where
    (<=) (C x1 y1) (C x2 y2) = (y1 > y2) || (y1 == y2 && x1 <= x2)

data Deplacement = N | G | D | H | B | GH | GB | DH | DB deriving (Eq,Show)

bougeCoord :: Deplacement -> Coord -> Coord
bougeCoord N c = c
bougeCoord G (C x y) = C (x-1) y
bougeCoord GH (C x y) = C (x-1) (y+1)
bougeCoord GB (C x y) = C (x-1) (y-1)
bougeCoord D (C x y) = C (x+1) y
bougeCoord DH (C x y) = C (x+1) (y+1)
bougeCoord DB (C x y) = C (x+1) (y+1) 
bougeCoord H (C x y) = C x (y+1)
bougeCoord B (C x y) = C x (y-1)

prop_bougeCoordGaucheDroite :: Coord -> Bool 
prop_bougeCoordGaucheDroite c = c == (bougeCoord G . bougeCoord D) c

prop_bougeCoordGaucheHaut :: Coord -> Bool
prop_bougeCoordGaucheHaut c = (bougeCoord G . bougeCoord H) c == bougeCoord GH c 


class Placable a where 
    coordP :: a -> Coord 
    bougeP :: Deplacement -> a -> a 
    deplaceP :: Coord -> a -> a 

prop_placableGauche_law :: (Placable a,Eq a) => a -> Bool
prop_placableGauche_law x = bougeP G x == deplaceP (gauche (coordP x)) x

