module Environnement where

import Lemming 
import Coord 
import Niveau 

import qualified Data.Sequence as S 
import qualified Data.Map as M 
import qualified Data.List as L
import qualified Data.Maybe as Y
import qualified Data.Foldable as F

data Entite = Lem Int Lemming
    deriving Eq 

data Envi = Envi {hEnvi :: Int, lEnvi :: Int, entitesEnvi:: S.Seq Entite,
                    casesEnvi :: M.Map Coord (S.Seq  Entite)}
                    deriving Eq 

instance Show Entite where
    show (Lem _ l) = show l 

instance Placable Entite where
    coordP (Lem _ l) = coordP l 
    bougeP d (Lem i l) = Lem i $ bougeP d l
    deplaceP c (Lem i l) =  Lem i $ deplaceP  c l

idEnt :: Entite -> Int
idEnt (Lem i _) = i 

envide :: Int -> Int -> Envi
envide h l = Envi h l S.empty M.empty 

entitesEnvi2 :: Envi -> S.Seq  Entite
entitesEnvi2 (Envi h l _ cases) = M.foldl' etape S.Empty cases
    where etape acc s = s <> acc 

trouveIdEnv :: Int -> Envi -> Maybe Entite
trouveIdEnv n = trouveIdSeq n . entitesEnvi

trouveIdSeq :: Int -> S.Seq Entite -> Maybe Entite
trouveIdSeq n = foldr etape Nothing
    where etape e acc = if idEnt e == n then Just e else acc 

trouveIdMap :: Int -> M.Map Coord (S.Seq Entite) -> Maybe Coord
trouveIdMap n = M.foldrWithKey  etape Nothing
    where etape c s acc =  case trouveIdSeq n s of 
                                Nothing -> acc
                                Just _ -> Just c 

prop_enviInclusion1 :: Envi -> Bool
prop_enviInclusion1 (Envi _ _ ents cases) = foldr etape True ents
    where etape e acc = case trouveIdMap (idEnt e) cases of
                            Nothing -> False
                            Just c -> c == coordP e

prop_enviInclusion2 :: Envi -> Bool
prop_enviInclusion2 (Envi _ _ ents cases) = M.foldrWithKey etape True cases
    where etape c s acc = foldr (etape2 c) acc s
          etape2 c e acc = case trouveIdSeq (idEnt e) ents of
                                Nothing -> False 
                                Just e2 -> acc && coordP e2 == c && coordP e2 == coordP e

prop_envi_inv :: Envi -> Bool
prop_envi_inv envi = prop_enviInclusion1 envi  && prop_enviInclusion2 envi     

prop_post_add :: Int -> Envi -> Bool 
prop_post_add n envi = 
    case trouveIdEnv n envi of
        Nothing  -> False 
        _ -> True  

prop_post_enleve :: Int -> Envi -> Bool 
prop_post_enleve n envi = 
    case trouveIdEnv n envi of
        Nothing -> True 
        _ -> False

instance Show Envi where
    show = showEnvi

showEnvi :: Envi -> String
showEnvi (Envi h l _ cases) = let s = aux 0 (h-1) in s
        where aux x y = if x == (l-1)
                        then (if y == 0 then lacase x y else lacase x y ++ "\n" ++ aux 0 (y-1))
                        else lacase x y ++ aux (x+1) y
              lacase x y = case M.lookup (C x y) cases of
                            Nothing -> " "
                            Just S.Empty -> " "
                            Just (e S.:<| es) -> show e

caseVide :: Coord -> Envi -> Bool
caseVide (C x y) (Envi h l _ cases) = (x < l) && (x >= 0) && (y < h) && (y >= 0)

appliqueIdSeq :: Int -> (Entite -> Entite) -> S.Seq Entite -> S.Seq Entite
appliqueIdSeq i f = foldr etape S.empty
    where etape n acc
            | idEnt n == i = f n S.:<| acc
            | otherwise  = n S.:<| acc

appliqueIdEnv :: Int -> (Entite -> Entite) -> Envi -> Envi
appliqueIdEnv n f (Envi h l ents cases) = case trouveIdSeq n ents of
                                            Nothing  -> error $ "appliqueIdEnv : pas trouve l'entite" ++ show n ++ "dans la sequence"
                                            Just e -> Envi h l nents ncases
                                            where nents = appliqueIdSeq n f ents
                                                  ncases = case trouveIdMap n cases of
                                                            Nothing -> error $ "appliqueIdEnv : pas trouve l'entite" ++ show n
                                                            Just endroit -> M.foldrWithKey etape M.empty cases
                                                                where etape co s
                                                                        | co == endroit = M.insert co (appliqueIdSeq n f s)
                                                                        | otherwise = M.insert co s  

enleveId :: Int -> S.Seq Entite -> S.Seq Entite
enleveId i = foldr etape S.empty
        where etape e acc
                | idEnt e == i = acc
                | otherwise = e S.:<| acc

enleveEnvi :: Int -> Envi -> Envi
enleveEnvi n (Envi h l ents cases) =  Envi h l nents ncases
                                        where nents = enleveId n ents
                                              ncases = case trouveIdMap n cases of 
                                                Nothing -> cases
                                                Just endroit -> case M.lookup endroit cases of
                                                    Nothing -> undefined
                                                    Just s -> M.insert endroit (enleveId n s) cases

deplaceDansEnvi :: Int -> Coord -> Envi -> Envi
deplaceDansEnvi n dest (Envi h l ents cases) = case trouveIdSeq n ents of 
                                                    Nothing -> error $ "deplaceDansEnvi: pas toruve l'entité" ++ show n ++ "dans la sequence"
                                                    Just e -> Envi h l nents ncases
                                                        where nents = appliqueIdSeq n (deplaceP dest) ents
                                                              ncases = case trouveIdMap n cases of
                                                                        Nothing -> error $ "deplaceDansEnvi : pas trouve l'entité" ++ show n
                                                                        Just source -> let dents = Y.fromMaybe S.empty $ M.lookup dest cases in
                                                                                       let sents = Y.fromMaybe S.empty $ M.lookup source cases in
                                                                                       let ncases = M.insert source (enleveId n sents) cases in
                                                                                       M.insert dest (deplaceP dest e S.:<| dents) ncases

idFrais :: Envi -> Int
idFrais (Envi h l ents cases) = l + foldr etape 0 ents
    where etape ent = max (idEnt ent)

addEntite :: Entite -> Envi -> Envi
addEntite ent (Envi h l ents cases) = Envi h l nents ncases 
                                        where nents = ent S.:<| ents
                                              ncases = M.insert (coordP ent) (ent S.:<| cents) cases
                                                 where cents = Y.fromMaybe S.empty $ M.lookup (coordP ent) cases
 
trouvePersonne :: Coord -> Envi -> Maybe Int
trouvePersonne c (Envi h l ents cases) =
    case M.lookup c cases of
        Nothing -> Nothing 
        seq -> 
            case seq of
                Just S.Empty -> Nothing 
                Just s -> 
                    if getmetier (Just (S.index s 0)) == Mon
                        then Nothing 
                        else Just (idEnt (S.index s 0))

getmetier :: Maybe Entite -> Metier
getmetier m = case m of 
                Just (Lem i (Marcheur _ _ m)) -> m
                Just (Lem i (Tombeur _ _ _ m)) -> m
                Just (Lem i (Creuseur _ _ m)) -> m 
                Just (Lem i (Stoppeur _ _ m)) -> m
                Just (Lem i (Poseur _ _ _ m)) -> m
                Just (Lem i (Grimpeur _ _ m)) -> m
                Just (Lem i (Flotteur _ _ m)) -> m
                Just (Lem i (Monster _ _ _ m)) -> m
                Just (Lem i (Soldier _ _ _ m)) -> m

trouveMonster :: Coord -> Envi -> Maybe Int
trouveMonster c (Envi h l ents cases) =
    case M.lookup c cases of
        Nothing -> Nothing 
        seq -> 
            case seq of
                Just S.Empty -> Nothing 
                Just s -> foldr etape Nothing s
                        where etape e acc  = if getmetier (Just e) == Mon then Just (idEnt e) else acc 

changeMarcheur :: Entite -> Entite
changeMarcheur (Lem i ((Marcheur dir c met))) =  Lem i (Marcheur dir c Mar)
changeMarcheur (Lem i ((Tombeur dir n c met))) =  Lem i (Tombeur dir n c Mar)
changeMarcheur (Lem i ((Creuseur dir c met))) =  Lem i (Creuseur dir c Mar)
changeMarcheur (Lem i ((Stoppeur dir c met))) =  Lem i (Stoppeur dir c Mar)
changeMarcheur (Lem i ((Poseur dir n c met))) =  Lem i (Poseur dir n c Mar)
changeMarcheur (Lem i ((Grimpeur dir c met))) =  Lem i (Grimpeur dir c Mar)
changeMarcheur (Lem i ((Flotteur dir c met))) =  Lem i (Flotteur dir c Mar)
changeMarcheur (Lem i ((Monster dir n c met))) =  Lem i (Monster dir n c Mar)
changeMarcheur (Lem i ((Soldier dir n c met))) =  Lem i (Soldier dir n c Mar)

changeCreuseur :: Entite -> Entite
changeCreuseur (Lem i ((Marcheur dir c met))) =  Lem i (Marcheur dir c Cre)
changeCreuseur (Lem i ((Tombeur dir n c met))) =  Lem i (Tombeur dir n c Cre)
changeCreuseur (Lem i ((Creuseur dir c met))) =  Lem i (Creuseur dir c Cre)
changeCreuseur (Lem i ((Stoppeur dir c met))) =  Lem i (Stoppeur dir c Cre)
changeCreuseur (Lem i ((Poseur dir n c met))) =  Lem i (Poseur dir n c Cre)
changeCreuseur (Lem i ((Grimpeur dir c met))) =  Lem i (Grimpeur dir c Cre)
changeCreuseur (Lem i ((Flotteur dir c met))) =  Lem i (Flotteur dir c Cre)
changeCreuseur (Lem i ((Monster dir n c met))) =  Lem i (Monster dir n c Cre)
changeCreuseur (Lem i ((Soldier dir n c met))) =  Lem i (Soldier dir n c Cre)

changeStoppeur :: Entite -> Entite
changeStoppeur (Lem i ((Marcheur dir c met))) =  Lem i (Marcheur dir c Sto)
changeStoppeur (Lem i ((Tombeur dir n c met))) =  Lem i (Tombeur dir n c Sto)
changeStoppeur (Lem i ((Creuseur dir c met))) =  Lem i (Creuseur dir c Sto)
changeStoppeur (Lem i ((Stoppeur dir c met))) =  Lem i (Stoppeur dir c Sto)
changeStoppeur (Lem i ((Poseur dir c n met))) =  Lem i (Poseur dir c n Sto)
changeStoppeur (Lem i ((Grimpeur dir c met))) =  Lem i (Grimpeur dir c Sto)
changeStoppeur (Lem i ((Flotteur dir c met))) =  Lem i (Flotteur dir c Sto)
changeStoppeur (Lem i ((Monster dir n c met))) =  Lem i (Monster dir n c Sto)
changeStoppeur (Lem i ((Soldier dir n c met))) =  Lem i (Soldier dir n c Sto)

changePoseur :: Entite -> Entite
changePoseur (Lem i ((Marcheur dir c met))) =  Lem i (Marcheur dir c Pos)
changePoseur (Lem i ((Tombeur dir n c met))) =  Lem i (Tombeur dir n c Pos)
changePoseur (Lem i ((Creuseur dir c met))) =  Lem i (Creuseur dir c Pos)
changePoseur (Lem i ((Stoppeur dir c met))) =  Lem i (Stoppeur dir c Pos)
changePoseur (Lem i ((Poseur dir n c met))) =  Lem i (Poseur dir n c Pos)
changePoseur (Lem i ((Grimpeur dir c met))) =  Lem i (Grimpeur dir c Pos)
changePoseur (Lem i ((Flotteur dir c met))) =  Lem i (Flotteur dir c Pos)
changePoseur (Lem i ((Monster dir n c met))) =  Lem i (Monster dir n c Pos)
changePoseur (Lem i ((Soldier dir n c met))) =  Lem i (Soldier dir n c Pos)

changeGrimpeur :: Entite -> Entite
changeGrimpeur (Lem i ((Marcheur dir c met))) =  Lem i (Marcheur dir c Gri)
changeGrimpeur (Lem i ((Tombeur dir n c met))) =  Lem i (Tombeur dir n c Gri)
changeGrimpeur (Lem i ((Creuseur dir c met))) =  Lem i (Creuseur dir c Gri)
changeGrimpeur (Lem i ((Stoppeur dir c met))) =  Lem i (Stoppeur dir c Gri)
changeGrimpeur (Lem i ((Poseur dir n c met))) =  Lem i (Poseur dir n c Gri)
changeGrimpeur (Lem i ((Grimpeur dir c met))) =  Lem i (Grimpeur dir c Gri)
changeGrimpeur (Lem i ((Flotteur dir c met))) =  Lem i (Flotteur dir c Gri)
changeGrimpeur (Lem i ((Monster dir n c met))) =  Lem i (Monster dir n c Gri)
changeGrimpeur (Lem i ((Soldier dir n c met))) =  Lem i (Soldier dir n c Gri)

changeFlotteur :: Entite -> Entite
changeFlotteur (Lem i ((Marcheur dir c met))) =  Lem i (Marcheur dir c Flo)
changeFlotteur (Lem i ((Tombeur dir n c met))) =  Lem i (Tombeur dir n c Flo)
changeFlotteur (Lem i ((Creuseur dir c met))) =  Lem i (Creuseur dir c Flo)
changeFlotteur (Lem i ((Stoppeur dir c met))) =  Lem i (Stoppeur dir c Flo)
changeFlotteur (Lem i ((Poseur dir n c met))) =  Lem i (Poseur dir n c Flo)
changeFlotteur (Lem i ((Grimpeur dir c met))) =  Lem i (Grimpeur dir c Flo)
changeFlotteur (Lem i ((Flotteur dir c met))) =  Lem i (Flotteur dir c Flo)
changeFlotteur (Lem i ((Monster dir n c met))) =  Lem i (Monster dir n c Flo)
changeFlotteur (Lem i ((Soldier dir n c met))) =  Lem i (Soldier dir n c Flo)

changeMonster :: Entite -> Entite
changeMonster (Lem i ((Marcheur dir c met))) =  Lem i (Marcheur dir c Mon)
changeMonster (Lem i ((Tombeur dir n c met))) =  Lem i (Tombeur dir n c Mon)
changeMonster (Lem i ((Creuseur dir c met))) =  Lem i (Creuseur dir c Mon)
changeMonster (Lem i ((Stoppeur dir c met))) =  Lem i (Stoppeur dir c Mon)
changeMonster (Lem i ((Poseur dir n c met))) =  Lem i (Poseur dir n c Mon)
changeMonster (Lem i ((Grimpeur dir c met))) =  Lem i (Grimpeur dir c Mon)
changeMonster (Lem i ((Flotteur dir c met))) =  Lem i (Flotteur dir c Mon)
changeMonster (Lem i ((Monster dir n c met))) =  Lem i (Monster dir n c Mon)
changeMonster (Lem i ((Soldier dir n c met))) =  Lem i (Soldier dir n c Mon)
                    
changeSoldier :: Entite -> Entite
changeSoldier (Lem i ((Marcheur dir c met))) =  Lem i (Marcheur dir c Sol)
changeSoldier (Lem i ((Tombeur dir n c met))) =  Lem i (Tombeur dir n c Sol)
changeSoldier (Lem i ((Creuseur dir c met))) =  Lem i (Creuseur dir c Sol)
changeSoldier (Lem i ((Stoppeur dir c met))) =  Lem i (Stoppeur dir c Sol)
changeSoldier (Lem i ((Poseur dir n c met))) =  Lem i (Poseur dir n c Sol)
changeSoldier (Lem i ((Grimpeur dir c met))) =  Lem i (Grimpeur dir c Sol)
changeSoldier (Lem i ((Flotteur dir c met))) =  Lem i (Flotteur dir c Sol)
changeSoldier (Lem i ((Monster dir n c met))) =  Lem i (Monster dir n c Sol)
changeSoldier (Lem i ((Soldier dir n c met))) =  Lem i (Soldier dir n c Sol)                    