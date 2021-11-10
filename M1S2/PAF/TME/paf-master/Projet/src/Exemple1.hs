module Exemple1 where
import Environnement
import Niveau
import Lemming
import Coord
import Etat

niv1 :: Niveau
niv1 = read "XXXXXXXXXX\nX E      X\nX        X\nX000000  X\nX        X\nX        X\nX      00X\nX        X\nX0000    X\nX    00  X\nX    00  X\nX       SX\nXXXXXXXXXX" 

niv2 :: Niveau 
niv2 = read "XXXXXXXXXX\nXE       X\nX        X\nX        X\nX       0X\nX0       X\nX      00X\nX        X\nX0000    X\nX    00  X\nX    00  X\nX       SX\nXXXXXXXXXX" 

envi1 :: Envi
envi1 = envide 13 10

monster1 :: Entite 
monster1 = Lem 1 (Monster Droite 0 (C 4 1) Mon)

restants1 :: Int
restants1 = 10

marcheur1 :: Entite 
marcheur1 = Lem 2 (Marcheur Droite (C 3 1) Mar)

soldier1 :: Entite
soldier1 = Lem 3 (Soldier Droite 0 (C 2 1) Sol)

poseur1 :: Entite 
poseur1 = Lem 4 (Poseur Droite 0 (C 2 1) Pos)

vivants1 :: Int
vivants1 = 0

sauvants1 :: Int
sauvants1 = 0

morts1 :: Int 
morts1 = 0

etat1 :: Etat
etat1 = Etat (addEntite marcheur1 (addEntite monster1 envi1)) niv2 restants1 vivants1 sauvants1 morts1
 
-- >>> prop_envi_inv envi1
-- True

-- >>> prop_niveauInvariant niv1
-- True

