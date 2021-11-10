module Lemming where

import Coord

data Direction = Gauche | Droite
    deriving (Eq,Show)

data Metier = Mar 
            | Cre
            | Pos
            | Sto
            | Mor
            | Gri
            | Flo
            | Min
            | Mon
            | Sol
        deriving Eq

data Lemming = Marcheur Direction Coord Metier
             | Tombeur Direction Int Coord Metier
             | Mort Coord
             | Creuseur Direction Coord Metier
             | Stoppeur Direction Coord Metier
             | Poseur Direction Int Coord Metier
             | Grimpeur Direction Coord Metier
             | Flotteur Direction Coord Metier
             | Mineur Direction Coord Metier
             | Monster Direction Int Coord Metier
             | Soldier Direction Int Coord Metier 
        deriving Eq

instance Show Lemming where
    show (Mort _) = "+"
    show (Marcheur Gauche _ _) = "<"
    show (Marcheur Droite _ _) = ">"
    show Tombeur{} = "v"
    show Creuseur{} = "#"
    show Stoppeur{} = "A"
    show Poseur{} = "P"
    show Grimpeur{} = "G"
    show Flotteur{} = "F"
    show Mineur{} = "?"
    show Monster{} = "M"
    show Soldier{} = "S"

 
instance Placable Lemming where
    coordP = coordLemming
    bougeP = bougeLemming
    deplaceP = deplaceLemming

coordLemming :: Lemming -> Coord
coordLemming (Marcheur _ c _) = c
coordLemming (Mort c)= c 
coordLemming (Tombeur _ _ c _) = c
coordLemming (Creuseur _ c _) = c 
coordLemming (Stoppeur _ c _) = c
coordLemming (Poseur _ _ c _) = c
coordLemming (Grimpeur _ c _) = c
coordLemming (Flotteur _ c _) = c
coordLemming (Mineur _ c _) = c
coordLemming (Monster _ _ c _) = c
coordLemming (Soldier _ _ c _) = c
 
tueLemming :: Lemming -> Lemming
tueLemming l = Mort (coordLemming l)

prop_tueLemming_post :: Lemming -> Bool
prop_tueLemming_post l = case tueLemming l of 
                            Mort c -> c == coordLemming l
                            _ -> False 

bougeLemming :: Deplacement -> Lemming -> Lemming
bougeLemming dep (Marcheur dir c met) = Marcheur dir (bougeCoord dep c) met
bougeLemming dep (Mort c) = Mort (bougeCoord dep c)
bougeLemming dep (Tombeur dir n c met) = Tombeur dir n (bougeCoord dep c) met
bougeLemming dep (Creuseur dir c met) = Creuseur dir (bougeCoord dep c) met
bougeLemming dep (Stoppeur dir c met) = Stoppeur dir (bougeCoord dep c) met
bougeLemming dep (Poseur dir n c met) = Poseur dir n (bougeCoord dep c) met
bougeLemming dep (Grimpeur dir c met) = Grimpeur dir (bougeCoord dep c) met
bougeLemming dep (Flotteur dir c met) =  Flotteur dir (bougeCoord dep c) met
bougeLemming dep (Mineur dir c met) = Mineur dir (bougeCoord dep c) met
bougeLemming dep (Monster dir n c met) = Monster dir n (bougeCoord dep c) met
bougeLemming dep (Soldier dir n c met) = Soldier dir n (bougeCoord dep c) met

prop_bougeLemmingDHDBG :: Lemming -> Bool
prop_bougeLemmingDHDBG l = bougeLemming D l == (bougeLemming G . bougeLemming DB . bougeLemming DH) l

deplaceLemming :: Coord -> Lemming -> Lemming
deplaceLemming c (Mort _) = Mort c
deplaceLemming c (Marcheur d _ met) = Marcheur d c met
deplaceLemming c (Tombeur d n _ met) = Tombeur d n c met
deplaceLemming c (Creuseur d _ met) = Creuseur d c met
deplaceLemming c (Stoppeur d _ met) = Stoppeur d c met
deplaceLemming c (Poseur d n _ met) = Poseur d n c met
deplaceLemming c (Grimpeur d _ met) = Grimpeur d c met
deplaceLemming c (Flotteur d _ met) = Flotteur d c met
deplaceLemming c (Mineur d _ met) = Mineur d c met
deplaceLemming c (Monster d n _ met) = Monster d n c met
deplaceLemming c (Soldier d n _ met) = Soldier d n c met
                                           