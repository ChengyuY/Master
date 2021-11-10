module Niveau where

import qualified Data.List as L 
import qualified Data.Map.Strict as M
import Coord 

import System.IO

import Control.Monad (guard)
import Control.Concurrent (threadDelay)
import System.Random

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl',find)

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Debug.Trace as T


data Case = Metal | Terre | Entree | Sortie | Vide 
    deriving Eq 

instance Show Case  where 
    show Vide = " "
    show Metal = "X"
    show Terre = "0"
    show Entree = "E"
    show Sortie = "S"

instance Read Case where
    readsPrec _ x = [(chaineDeCase x,"")]

chaineDeCase :: String -> Case
chaineDeCase " " = Vide 
chaineDeCase "X" = Metal 
chaineDeCase "0" = Terre 
chaineDeCase "E" = Entree
chaineDeCase "S" = Sortie 
chaineDeCase _ = Vide 

data Niveau = Niveau {hniveau :: Int, lNiveau :: Int, casesNiveau :: M.Map Coord Case} 
    deriving Eq 

instance Show Niveau where
    show (Niveau h l cases) = let (s,_,_) = M.foldl' aux ("",0,0) cases in s 
        where aux (s,x,y) v = if x == (l - 1) 
                            then (if y == (h - 1) then (s ++ show v,0,0) else (s ++ show v ++ "\n", 0, y+1))
                            else (s ++ show v , x+1, y)

instance Read Niveau where
    readsPrec _ x = [(retourneNiveau (chaineDeNiveau x), "")]

chaineDeNiveau :: String -> Niveau
chaineDeNiveau = (\(cases,l,h) -> Niveau (h+1) l cases) . L.foldl' aux (M.empty ,0,0) where 
     aux (cases,x,y) '\n' = (cases,0,y+1)
     aux (cases,x,y) c = (M.insert (C x y) (read [c]) cases, x+1, y)

retourneNiveau :: Niveau -> Niveau
retourneNiveau (Niveau h l cases) = Niveau h l $ M.foldrWithKey etape M.empty cases
    where etape (C x y) c = M.insert (C x (h-1-y)) c 

prop_niveauFerme :: Niveau -> Bool
prop_niveauFerme (Niveau h l cases) = M.foldrWithKey etape True cases 
    where etape (C x y) c acc
            | x == 0 || x == l - 1 || y == 0 || y == h - 1 = (c == Metal) && acc
            | otherwise = acc

prop_niveauEntreeSortie :: Niveau -> Bool
prop_niveauEntreeSortie (Niveau h l cases) = let (e,s) = M.foldrWithKey etape (0,0) cases in e == 1 && s == 1
    where etape (C x y) Entree (en,so) = (en+1,so)
          etape (C x y) Sortie (en,so) = (en,so+1)
          etape (C x y) _ acc = acc 

trouveEntree :: Niveau -> Maybe Coord 
trouveEntree (Niveau h l cases) = M.foldrWithKey  etape Nothing cases 
    where etape c Entree _ = Just c 
          etape _ _ acc = acc 

trouveSortie :: Niveau -> Maybe Coord 
trouveSortie (Niveau h l cases) = M.foldrWithKey  etape Nothing cases 
    where etape c Sortie _ = Just c 
          etape _ _ acc = acc  

prop_niveauEntreeCorrecte :: Niveau -> Bool 
prop_niveauEntreeCorrecte niv = case trouveEntree niv of
                                     Nothing -> False
                                     Just (C x y) -> case M.lookup (C x (y-1)) (casesNiveau niv) of 
                                                        Just Vide -> True
                                                        _ -> False 

prop_niveauSortieCorrecte :: Niveau -> Bool 
prop_niveauSortieCorrecte niv = case trouveSortie niv of
                                     Nothing -> False
                                     Just (C x y) -> case M.lookup (C x (y-1)) (casesNiveau niv) of 
                                                        Just Metal -> True
                                                        _ -> False                                                      

prop_niveauInclusion :: Niveau -> Bool
prop_niveauInclusion (Niveau h l cases) = M.foldrWithKey etape True cases
    where etape (C x y) _ acc = acc && (x >= 0) && (x < l) && (y >= 0) && (y < h)

prop_niveauInvariant :: Niveau -> Bool
prop_niveauInvariant niv = prop_niveauFerme niv && prop_niveauEntreeSortie niv && prop_niveauEntreeCorrecte niv  && prop_niveauSortieCorrecte niv &&  prop_niveauInclusion niv 

passable :: Coord -> Niveau -> Bool
passable c (Niveau h l cases) = case M.lookup c cases of
                                    Just Vide -> True
                                    Just Entree -> True
                                    Just Sortie -> True
                                    _ -> False

dur :: Coord -> Niveau -> Bool
dur c (Niveau h l cases) = case M.lookup c cases of
                                Just Metal -> True
                                Just Terre -> True
                                _ -> False 

-- Verifier si le case en bas peut etre creuse.
creusable :: Coord -> Niveau -> Bool
creusable c (Niveau h l cases) = case M.lookup c cases of
                                Just Terre -> True
                                _ -> False

creuser :: Coord -> Niveau -> Niveau
creuser c (Niveau h l cases) = 
    Niveau h l (M.update f c cases)
    where f c = Just Vide

stopper :: Coord -> Niveau -> Niveau
stopper c (Niveau h l cases) = 
    Niveau h l (M.update f c cases)
    where f c = Just Metal

astopper :: Coord -> Niveau -> Niveau
astopper c (Niveau h l cases) = 
    Niveau h l (M.update f c cases)
    where f c = Just Vide

poser :: Coord -> Niveau -> Niveau
poser c (Niveau h l cases) = 
    Niveau h l (M.update f c cases)
    where f c = Just Terre

caseSuivante :: (Int -> Int -> Int -> [(Coord,Case)]) -> Int -> Int -> Int -> [(Coord,Case)]
caseSuivante aux i j c = 
    if i == 14
            then if j == 14
                then []
                else  aux 0 (j+1) c
            else aux (i+1) j c

niveauGenerator :: Int ->  Niveau
niveauGenerator c = 
    let larg = 14 in
    let haut = 14 in 
    Niveau larg haut (M.fromList (aux 0 0 c)) 
    where
        aux :: Int -> Int -> Int -> [(Coord,Case)]
        --creer les mur
        aux 0 j c = let nouv_case = Metal in           
                [(C 0 j,nouv_case)] <> caseSuivante aux 0 j c
        aux 14 j c = let nouv_case = Metal in
                [(C 14 j,nouv_case)] <> caseSuivante aux 14 j c
        aux i 0 c = let nouv_case = Metal in
                [(C i 0,nouv_case)] <> caseSuivante aux i 0 c
        aux i 14 c = let nouv_case = Metal in                   
                [(C i 14,nouv_case)] <> caseSuivante aux i 14 c
        -- creer entre et sortie
        aux i j c = let a = mod c 13 + 1 in              
                    let d = mod c 2 + 11 in              
                    let f = mod c 7 + 3 in
                    if j == d || j == f                   
                    then if i == a && j == d
                        then let nouv_case = Vide in 
                        [(C i j,nouv_case)] <> caseSuivante aux i j c 
                        else 
                            let nouv_case = Terre in 
                                [(C i j,nouv_case)] <> caseSuivante aux i j c
                    else if ((i == mod (a+4) 13  + 1) && j>f && j< d ) || ((i == mod (a+10) 13 + 1) && j > 0 && j < max 9 f)    -- Si il s'agit d'un mur vertical
                        then 
                            let nouv_case = Vide in 
                                [(C i j,nouv_case)] <> caseSuivante aux i j c                        
                        else  if j == 13 && i == mod (a+8) 13 + 1 
                            then let nouv_case = Entree in 
                            [(C i j,nouv_case)] <> caseSuivante aux i j c
                            else if j == 1 && i == a
                                then let nouv_case = Sortie in 
                                [(C i j,nouv_case)] <> caseSuivante aux i j c
                                else let nouv_case = Vide in 
                                [(C i j,nouv_case)] <> caseSuivante aux i j c 
                    
                        

generateNiveauVerified :: StdGen -> Niveau
generateNiveauVerified g =
    let c = randomRs (150, 200) g  in
    let maCarte = niveauGenerator (head c) in 
    aux 0 maCarte 
    where 
        aux :: Int -> Niveau -> Niveau
        aux 10 _ = error "Cela fait 10 tours que le niveau n'est pas saine" 
        aux i niveau = 
            if prop_niveauInvariant niveau 
            --if True
            then niveau
            else let c = randomRs (150, 200) g  in
                aux (i+1) (niveauGenerator (head c))            

    
        




