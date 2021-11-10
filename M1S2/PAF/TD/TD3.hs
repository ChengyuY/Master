module Lib where

import Data.Text (Text)
import Data.Text as Text

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- *** SYSTEME ***

-- outil map
map_pourtout :: (a -> Bool) -> Map k a -> Bool
map_pourtout p m = Map.fold aux m True
    where aux v f = f && (p v)

-- outil seq
seq_pourtout :: (a -> Bool) -> Seq a -> Bool
seq_pourtout p s = fold aux s True
    where aux e f = f && (p e)

-- *** SYSTEME *** 

data Systeme = {
    membres :: Map MembreId Member, 
    projets :: Map ProjetId Projet
}



-- invariant
prop_SystemeInv :: Systeme -> Bool
prop_SystemeInv s{membres = m, projets = p} = map_pourtout prop_MembreInv m
-- on peut ajouter que le login de chaque membre est unique

-- constructeur
initSysteme :: () -> Systeme
initSysteme = Systeme Map.empty Map.empty


-- post constructeur
prop_PostInitSysteme :: () -> Bool
prop_PostInitSysteme () = prop_SystemeInv (initSysteme ())

-- *** MEMBRES ***

newtype MembreId = MembreId Integer
    deriving (Show, Eq, Ord)

newtype LoginInfo = Login Text

newtype AdresseMail = AdresseMail Text

-- invariant
prop_AdresseMailInv :: AdresseMail -> Bool
prop_AdresseMailInv (AdresseMail a) = True -- à implémenter

data Membre = {
    prenom :: Text,
    nom :: Text,
    login :: LoginInfo,
    mele :: AdresseMail
}

-- invariant
prop_MembreInv :: Membre -> Bool
prop_MembreInv m {prenom = p, nom = n, login (LoginInfo l), mele (AdresseMail a)} = not (Text.empty p) &&  not (Text.empty n) && not (Text.empty l) && not (Text.empty a)

-- constructeur
initMembre :: Text -> Text -> Text -> Text -> Maybe Membre
initMembre p n l m 
 | not (Text.empty p) &&  not (Text.empty n) && not (Text.empty l) && not (Text.empty a) = Just $ Membre p n (LoginInfo l) (AdresseMail m)
 | otherwise = Nothing

 -- *** Projets ***

newtype ProjetId = ProjetId Text

data Projet = Projet {
    nomComplet :: Text,
    equipe :: Set MembreId,
    tableaux :: Map TabId TabInfo
}

-- invariant

-- constructeur


-- *** Tableaux ***

newtype TabId = TabId Text

data TabInfo = Tab {
    cartes :: Seq Carte
}

-- invariant

-- constructeur

-- *** Cartes ***

data Carte = Carte {
    carteNom :: Text,
    carteDesc :: Text,
    carteMembres :: Set MembreId,
    taches :: Seq Tache
}

-- invariant
prop_CarteInvText :: Carte -> Bool
prop_CarteInvText Carte {carteNom = c, carteDesc = d} = not (Text.empty c) && (Text.empty d)

prop_CarteInvTache :: Carte -> Bool
prop_CarteInvTache Carte{taches = t} = fold aux True t
    where aux ta f = f && (propTacheInv ta)

prop_CarteInvMembre :: Tache -> Bool
prop_CarteInvMembre Carte {carteMembres m, taches = t} = seq_pourtout aux t
    where aux ta = (tacheMembres ta) `Set.isSubsetOf` m
-- prop_CarteInvMembre Carte {carteMembres m, taches = t} = seq_pourtout aux (fmap tacheMembres t)
--    where aux em = em `Set.isSubsetOf` m

prop_CarteInv :: Carte -> Bool
prop_CarteInv c = (prop_CarteInvText c) && (prop_CarteInvTache c) && (prop_CarteInvMembre c)

-- constructeur

-- *** Taches ***

data TacheInfo = TacheInfo {
    tacheDescr :: Text,
    tacheDate :: Date
}

data Tache = TacheLibre TacheInfo
    | TacheEnCours TacheInfo (Set MembreId) TacheDeadline
    | TacheFinie TacheInfo (Set MembreId) Date

data TacheDeadline = Illimite | Deadline Date

data Date = DateAbstraite deriving (Show, Eq, Ord)

-- observateur
tacheMembres :: Tache -> Set MembreId
tacheMembres (TacheEnCours _ ms _) = ms
tacheMembres (TacheFinie _ ms _) = ms
tacheMembres (TacheLibre _) = Set.empty

-- invariant

-- constructeur

-- operations

-- ** assignement **

-- assignement : precondition
prop_tacheAssignePre :: Tache -> Set Membre Id -> TacheDeadline -> Bool
prop_tacheAssignePre (TacheLibre _) mes _ = not (Set.null mes)
prop_tacheAssignePre _ _ _ = False

-- assignement : code
tacheAssigne :: Tache -> Set MembreId -> TacheDeadline -> Tache
tacheAssigne (TacheLibre tinfo) ms dl = TacheEnCours tinfo ms dl
tacheAssigne _ _ _ = error "Tache deja assignee (ou completee)."

-- assignement : condition 
prop_tacheAssignePost :: Tache -> Set MembreId -> TacheDeadline -> Bool
prop_tacheAssignePost ta mes dl = case tacheAssigne ta mes of
                            (TacheEnCours ti tmes tdl) -> tmes == mes && dl == tdl -- et que l'info reste la meme
                            | _ -> False