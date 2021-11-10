module Etat where

import Lemming 

import Coord 

import Environnement

import Niveau

data Etat = Etat {
    enviE :: Envi,
    niveauE :: Niveau,
    lrestantsE :: Int,
    lvivantsE :: Int,
    lsauvesE :: Int,
    lmortsE :: Int
}

data Fin = Victoire Int | Defaite

hauteurMortelle :: Int
hauteurMortelle = 5

rassembleEnvNiv :: String -> String -> String
rassembleEnvNiv [] _ = [] 
rassembleEnvNiv _ [] = []
rassembleEnvNiv (x1:xs1) (x2:xs2) = if x1 == ' ' then x2:rassembleEnvNiv xs1 xs2 else x1:rassembleEnvNiv xs1 xs2 

showEtat :: Etat -> String
showEtat e = rassembleEnvNiv (show (enviE e)) (show (niveauE e))

instance Show Etat where
    show = showEtat

tourLemming :: Int -> Lemming -> Etat -> Etat
-- Mort
-- 在环境中删除
tourLemming n (Mort c) (Etat envi niv r m s mor) =  Etat (enleveEnvi n envi) niv r (m-1) s (mor+1)
-- Marcheur
-- 如果脚下是空的 坠落
-- 非空 如果面朝方向及面朝方向上方是空的 向面朝方向移动一格
-- 如果面朝方向上方及其上方是空的 向面朝方向上方移动一格
tourLemming n (Marcheur Gauche c metier) (Etat envi niv r m s mor) =
    case metier of
        Cre -> Etat (appliqueIdEnv n (const (Lem n (Creuseur Gauche c Cre))) envi) niv r m s mor
        Sto -> Etat (appliqueIdEnv n (const (Lem n (Stoppeur Gauche c Sto))) envi) (stopper c niv) r m s mor
        Pos -> Etat (appliqueIdEnv n (const (Lem n (Poseur Gauche 0 c Pos))) envi) niv r m s mor
        Gri -> Etat (appliqueIdEnv n (const (Lem n (Grimpeur Gauche c Gri))) envi) niv r m s mor
        Flo -> Etat (appliqueIdEnv n (const (Lem n (Flotteur Gauche c Flo))) envi) niv r m s mor
        Sol -> Etat (appliqueIdEnv n (const (Lem n (Soldier Gauche 0 c Sol))) envi) niv r m s mor
        Mon -> Etat (appliqueIdEnv n (const (Lem n (Monster Gauche 0 c Mon))) envi) niv r m s mor
        Mar -> case trouveSortie niv of
            Nothing -> suite 
            Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) mor else suite
            where suite = 
                    case (passable (gauche c) niv && passable (haut (gauche c)) niv, passable (haut (gauche c)) niv && passable (haut (haut (gauche c))) niv, dur (bas c) niv) of
                    (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Gauche 0 c metier))) envi) niv r m s mor
                    (True,_,_) -> Etat (deplaceDansEnvi n (gauche c) envi) niv r m s mor
                    (_,True,_) -> Etat (deplaceDansEnvi n (haut (gauche c)) envi) niv r m s mor
                    (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Droite c metier))) envi) niv r m s mor
tourLemming n (Marcheur Droite c metier) (Etat envi niv r m s mor) =
    case metier of
        Cre -> Etat (appliqueIdEnv n (const (Lem n (Creuseur Droite c Cre))) envi) niv r m s mor
        Sto -> Etat (appliqueIdEnv n (const (Lem n (Stoppeur Droite c Sto))) envi) niv r m s mor
        Pos -> Etat (appliqueIdEnv n (const (Lem n (Poseur Droite 0 c Pos))) envi) niv r m s mor
        Gri -> Etat (appliqueIdEnv n (const (Lem n (Grimpeur Droite c Gri))) envi) niv r m s mor
        Flo -> Etat (appliqueIdEnv n (const (Lem n (Flotteur Droite c Flo))) envi) niv r m s mor
        Sol -> Etat (appliqueIdEnv n (const (Lem n (Soldier Droite 0 c Sol))) envi) niv r m s mor
        Mon -> Etat (appliqueIdEnv n (const (Lem n (Monster Droite 0 c Mon))) envi) niv r m s mor
        Mar -> case trouveSortie niv of
            Nothing -> suite
            Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) mor else suite
            where suite =
                    case (passable (droite c) niv && passable (haut (droite c)) niv, passable (haut (droite c)) niv && passable (haut (haut (droite c))) niv, dur (bas c) niv) of
                    (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Droite 0 c metier))) envi) niv r m s mor
                    (True,_,_) -> Etat (deplaceDansEnvi n (droite c) envi) niv r m s mor
                    (_,True,_) -> Etat (deplaceDansEnvi n (haut (droite c)) envi) niv r m s mor
                    (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Gauche c metier))) envi) niv r m s mor
-- 如果下方一格结实 判断坠落距离 若大于则死亡 小于则此轮行动模式变成行走者
-- 不结实 继续坠落
-- Tombeur
tourLemming n (Tombeur dir k c metier) (Etat envi niv r m s mor) = 
    case (dur (bas c) niv, k >= hauteurMortelle) of
        (True,True) -> Etat (appliqueIdEnv n (const (Lem n (Mort c))) envi) niv r m s mor
        (True,_) -> Etat (appliqueIdEnv n (const (Lem n (Marcheur dir c metier))) envi) niv r m s mor
        (_,_) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur dir (k+1) (bas c) metier))) (deplaceDansEnvi n (bas c) envi)) niv r m s mor
-- 判断下方是否结实 否则坠落
-- 是则判断下方及其左右三格是否能够挖掘 能则挖掘 不能此轮行动模式变成行走者
-- Creuseur
tourLemming n (Creuseur Gauche c metier) (Etat envi niv r m s mor) =
    case metier of
        Mar -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Gauche c Mar))) envi) niv r m s mor
        Sto -> Etat (appliqueIdEnv n (const (Lem n (Stoppeur Gauche c Sto))) envi) (stopper c niv) r m s mor
        Pos -> Etat (appliqueIdEnv n (const (Lem n (Poseur Gauche 0 c Pos))) envi) niv r m s mor
        Gri -> Etat (appliqueIdEnv n (const (Lem n (Grimpeur Gauche c Gri))) envi) niv r m s mor
        Flo -> Etat (appliqueIdEnv n (const (Lem n (Flotteur Gauche c Flo))) envi) niv r m s mor
        Sol -> Etat (appliqueIdEnv n (const (Lem n (Soldier Gauche 0 c Sol))) envi) niv r m s mor
        Mon -> Etat (appliqueIdEnv n (const (Lem n (Monster Gauche 0 c Mon))) envi) niv r m s mor
        Cre -> case trouveSortie niv of 
                Nothing -> suite
                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) mor else suite
                where suite = 
                        case (creusable (bas c) niv, creusable (bas (gauche c)) niv, creusable (bas (droite c)) niv, dur (bas c) niv) of
                        (_,_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Gauche 0 c metier))) envi) niv r m s mor
                        (True,True,True,_) -> Etat (deplaceDansEnvi n (bas c) envi) (creuser (bas c) (creuser (bas (gauche c)) (creuser (bas (droite c)) niv))) r m s mor
                        (True,False,True,_) -> Etat (deplaceDansEnvi n (bas c) envi) (creuser (bas c) (creuser (bas (droite c)) niv)) r m s mor
                        (True,True,False,_) -> Etat (deplaceDansEnvi n (bas c) envi) (creuser (bas c) (creuser (bas (gauche c)) niv)) r m s mor
                        (True,False,False,_) -> Etat (deplaceDansEnvi n (bas c) envi) (creuser (bas c) niv) r m s mor
                        (_,_,_,_) -> 
                            case (passable (gauche c) niv && passable (haut (gauche c)) niv, passable (haut (gauche c)) niv && passable (haut (haut (gauche c))) niv, dur (bas c) niv) of
                            (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Gauche 0 c metier))) envi) niv r m s mor
                            (True,_,_) -> Etat (deplaceDansEnvi n (gauche c) envi) niv r m s mor
                            (_,True,_) -> Etat (deplaceDansEnvi n (haut (gauche c)) envi) niv r m s mor
                            (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Creuseur Droite c metier))) envi) niv r m s mor
tourLemming n (Creuseur Droite c metier) (Etat envi niv r m s mor) =
    case metier of
        Mar -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Droite c Mar))) envi) niv r m s mor
        Sto -> Etat (appliqueIdEnv n (const (Lem n (Stoppeur Droite c Sto))) envi) (stopper c niv) r m s mor
        Pos -> Etat (appliqueIdEnv n (const (Lem n (Poseur Droite 0 c Pos))) envi) niv r m s mor
        Gri -> Etat (appliqueIdEnv n (const (Lem n (Grimpeur Droite c Gri))) envi) niv r m s mor
        Flo -> Etat (appliqueIdEnv n (const (Lem n (Flotteur Droite c Flo))) envi) niv r m s mor
        Sol -> Etat (appliqueIdEnv n (const (Lem n (Soldier Droite 0 c Sol))) envi) niv r m s mor
        Mon -> Etat (appliqueIdEnv n (const (Lem n (Monster Droite 0 c Mon))) envi) niv r m s mor
        Cre -> case trouveSortie niv of 
                Nothing -> suite
                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) mor else suite
                where suite = 
                        case (creusable (bas c) niv, creusable (bas (gauche c)) niv, creusable (bas (droite c)) niv, dur (bas c) niv) of
                        (_,_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Droite 0 c metier))) envi) niv r m s mor
                        (True,True,True,_) -> Etat (deplaceDansEnvi n (bas c) envi) (creuser (bas c) (creuser (bas (gauche c)) (creuser (bas (droite c)) niv))) r m s mor
                        (True,False,True,_) -> Etat (deplaceDansEnvi n (bas c) envi) (creuser (bas c) (creuser (bas (droite c)) niv)) r m s mor
                        (True,True,False,_) -> Etat (deplaceDansEnvi n (bas c) envi) (creuser (bas c) (creuser (bas (gauche c)) niv)) r m s mor
                        (True,False,False,_) -> Etat (deplaceDansEnvi n (bas c) envi) (creuser (bas c) niv) r m s mor
                        (_,_,_,_) -> 
                            case (passable (droite c) niv && passable (haut (droite c)) niv, passable (haut (droite c)) niv && passable (haut (haut (droite c))) niv, dur (bas c) niv) of
                            (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Droite 0 c metier))) envi) niv r m s mor
                            (True,_,_) -> Etat (deplaceDansEnvi n (droite c) envi) niv r m s mor
                            (_,True,_) -> Etat (deplaceDansEnvi n (haut (droite c)) envi) niv r m s mor
                            (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Creuseur Gauche c metier))) envi) niv r m s mor
-- 变成墙壁 无法移动 如果职业改变则取消墙壁
-- Stoppeur
tourLemming n (Stoppeur dir c metier) (Etat envi niv r m s mor) = 
    case metier of
        Sto -> Etat (appliqueIdEnv n (const  (Lem n (Stoppeur dir c Sto))) envi) (stopper c niv) r m s mor
        Mar -> Etat (appliqueIdEnv n (const  (Lem n (Marcheur dir c Mar))) envi) (astopper c niv) r m s mor
        Pos -> Etat (appliqueIdEnv n (const (Lem n (Poseur dir 0 c Pos))) envi) (astopper c niv) r m s mor
        Gri -> Etat (appliqueIdEnv n (const (Lem n (Grimpeur dir c Gri))) envi) (astopper c niv) r m s mor
        Flo -> Etat (appliqueIdEnv n (const (Lem n (Flotteur dir c Flo))) envi) (astopper c niv) r m s mor
        Sol -> Etat (appliqueIdEnv n (const (Lem n (Soldier dir 0 c Sol))) envi) (astopper c niv) r m s mor
        Mon -> Etat (appliqueIdEnv n (const (Lem n (Monster dir 0 c Mon))) envi) (astopper c niv) r m s mor
-- 如果挖掘次数没到4 且面朝方向不结实 且面朝方向上方，上上方为空 则在面朝方向填一块土 自身向面朝方向上方移动一格 
-- 否则在此轮行动模式变为行走者
-- Poseur
tourLemming n (Poseur Gauche t c metier) (Etat envi niv r m s mor) = 
    case metier of
        Mar -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Gauche c Mar))) envi) niv r m s mor
        Sto -> Etat (appliqueIdEnv n (const (Lem n (Stoppeur Gauche c Sto))) envi) (stopper c niv) r m s mor
        Cre -> Etat (appliqueIdEnv n (const (Lem n (Creuseur Gauche c Pos))) envi) niv r m s mor
        Gri -> Etat (appliqueIdEnv n (const (Lem n (Grimpeur Gauche c Gri))) envi) niv r m s mor
        Flo -> Etat (appliqueIdEnv n (const (Lem n (Flotteur Gauche c Flo))) envi) niv r m s mor
        Sol -> Etat (appliqueIdEnv n (const (Lem n (Soldier Gauche 0 c Sol))) envi) niv r m s mor
        Mon -> Etat (appliqueIdEnv n (const (Lem n (Monster Gauche 0 c Mon))) envi) niv r m s mor
        Pos -> case trouveSortie niv of 
                Nothing -> suite
                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) mor else suite
                where suite = 
                        if t <= 4 then 
                        case (dur (gauche c) niv , passable (haut (gauche c)) niv && passable (haut(haut(gauche c))) niv) of
                        (False,True) -> Etat (appliqueIdEnv n (const (Lem n (Poseur Gauche (t-1) (gauche (haut c)) metier))) (deplaceDansEnvi n (gauche (haut c)) envi)) (poser (gauche c) niv) r m s mor 
                        (_,_) -> 
                            case (passable (gauche c) niv && passable (haut (gauche c)) niv, passable (haut (gauche c)) niv && passable (haut (haut (gauche c))) niv, dur (bas c) niv) of
                            (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Gauche 0 c metier))) envi) niv r m s mor
                            (True,_,_) -> Etat (deplaceDansEnvi n (gauche c) envi) niv r m s mor
                            (_,True,_) -> Etat (deplaceDansEnvi n (haut (gauche c)) envi) niv r m s mor
                            (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Poseur Droite t c metier))) envi) niv r m s mor
                        else 
                            case (passable (gauche c) niv && passable (haut (gauche c)) niv, passable (haut (gauche c)) niv && passable (haut (haut (gauche c))) niv, dur (bas c) niv) of
                            (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Gauche 0 c metier))) envi) niv r m s mor
                            (True,_,_) -> Etat (deplaceDansEnvi n (gauche c) envi) niv r m s mor
                            (_,True,_) -> Etat (deplaceDansEnvi n (haut (gauche c)) envi) niv r m s mor
                            (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Poseur Droite t c metier))) envi) niv r m s mor
tourLemming n (Poseur Droite t c metier) (Etat envi niv r m s mor) = 
    case metier of
        Mar -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Droite c Mar))) envi) niv r m s mor
        Sto -> Etat (appliqueIdEnv n (const (Lem n (Stoppeur Droite c Sto))) envi) (stopper c niv) r m s mor
        Cre -> Etat (appliqueIdEnv n (const (Lem n (Creuseur Droite c Pos))) envi) niv r m s mor
        Gri -> Etat (appliqueIdEnv n (const (Lem n (Grimpeur Droite c Gri))) envi) niv r m s mor
        Flo -> Etat (appliqueIdEnv n (const (Lem n (Flotteur Droite c Flo))) envi) niv r m s mor
        Sol -> Etat (appliqueIdEnv n (const (Lem n (Soldier Droite 0 c Sol))) envi) niv r m s mor
        Mon -> Etat (appliqueIdEnv n (const (Lem n (Monster Droite 0 c Min))) envi) niv r m s mor
        Pos -> case trouveSortie niv of 
                Nothing -> suite
                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) mor else suite
                where suite = 
                        if t <= 4 then 
                        case (dur (droite c) niv , passable (haut (droite c)) niv && passable (haut(haut(droite c))) niv) of
                        (False,True) -> Etat (appliqueIdEnv n (const (Lem n (Poseur Droite (t-1) (droite (haut c)) metier))) (deplaceDansEnvi n (droite (haut c)) envi)) (poser (droite c) niv) r m s mor 
                        (_,_) -> 
                            case (passable (droite c) niv && passable (haut (droite c)) niv, passable (haut (droite c)) niv && passable (haut (haut (droite c))) niv, dur (bas c) niv) of
                            (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Droite 0 c metier))) envi) niv r m s mor
                            (True,_,_) -> Etat (deplaceDansEnvi n (droite c) envi) niv r m s mor
                            (_,True,_) -> Etat (deplaceDansEnvi n (haut (droite c)) envi) niv r m s mor
                            (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Poseur Gauche t c metier))) envi) niv r m s mor
                        else 
                            case (passable (droite c) niv && passable (haut (droite c)) niv, passable (haut (droite c)) niv && passable (haut (haut (droite c))) niv, dur (bas c) niv) of
                            (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Droite 0 c metier))) envi) niv r m s mor
                            (True,_,_) -> Etat (deplaceDansEnvi n (droite c) envi) niv r m s mor
                            (_,True,_) -> Etat (deplaceDansEnvi n (haut (droite c)) envi) niv r m s mor
                            (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Poseur Gauche t c metier))) envi) niv r m s mor
-- 如果面朝方向及其上方结实 且上方不结实 则向上方移动一格
-- 如果面朝方向结实 上方和面朝方向上方不结实 则向面朝方向上方移动一格
-- 如果上方及面朝方向结实 则坠落
-- 否则此轮行动模式变为行走者
-- Grimpeur
tourLemming n (Grimpeur Gauche c metier) (Etat envi niv r m s mor) = 
    case metier of
        Mar -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Gauche c Mar))) envi) niv r m s mor
        Sto -> Etat (appliqueIdEnv n (const (Lem n (Stoppeur Gauche c Sto))) envi) (stopper c niv) r m s mor
        Cre -> Etat (appliqueIdEnv n (const (Lem n (Creuseur Gauche c Cre))) envi) niv r m s mor
        Pos -> Etat (appliqueIdEnv n (const (Lem n (Poseur Gauche 0 c Pos))) envi) niv r m s mor
        Flo -> Etat (appliqueIdEnv n (const (Lem n (Flotteur Gauche c Flo))) envi) niv r m s mor
        Sol -> Etat (appliqueIdEnv n (const (Lem n (Soldier Gauche 0 c Sol))) envi) niv r m s mor
        Mon -> Etat (appliqueIdEnv n (const (Lem n (Monster Gauche 0 c Min))) envi) niv r m s mor
        Gri -> case trouveSortie niv of
                Nothing -> suite
                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) mor else suite
                where suite = 
                        case (dur (gauche c) niv, dur (gauche (haut c)) niv, dur (haut c) niv) of
                            (True,True,False) -> Etat (deplaceDansEnvi n (haut c) envi) niv r m s mor
                            (True,False,False) -> Etat (deplaceDansEnvi n (haut (gauche c)) envi) niv r m s mor
                            (True,_,True) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Gauche 0 c metier))) envi) niv r m s mor
                            (False,_,_) -> 
                                case (passable (gauche c) niv && passable (haut (gauche c)) niv, passable (haut (gauche c)) niv && passable (haut (haut (gauche c))) niv, dur (bas c) niv) of
                                (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Gauche 0 c metier))) envi) niv r m s mor
                                (True,_,_) -> Etat (deplaceDansEnvi n (gauche c) envi) niv r m s mor
                                (_,True,_) -> Etat (deplaceDansEnvi n (haut (gauche c)) envi) niv r m s mor
                                (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Grimpeur Droite c metier))) envi) niv r m s mor
tourLemming n (Grimpeur Droite c metier) (Etat envi niv r m s mor) = 
        case metier of
        Mar -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Droite c Mar))) envi) niv r m s mor
        Sto -> Etat (appliqueIdEnv n (const (Lem n (Stoppeur Droite c Sto))) envi) (stopper c niv) r m s mor
        Cre -> Etat (appliqueIdEnv n (const (Lem n (Creuseur Droite c Cre))) envi) niv r m s mor
        Pos -> Etat (appliqueIdEnv n (const (Lem n (Poseur Droite 0 c Pos))) envi) niv r m s mor
        Flo -> Etat (appliqueIdEnv n (const (Lem n (Flotteur Droite c Flo))) envi) niv r m s mor
        Sol -> Etat (appliqueIdEnv n (const (Lem n (Soldier Droite 0 c Sol))) envi) niv r m s mor
        Mon -> Etat (appliqueIdEnv n (const (Lem n (Monster Droite 0 c Min))) envi) niv r m s mor
        Gri ->  case trouveSortie niv of
                Nothing -> suite
                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) mor else suite
                where suite = 
                        case (dur (droite c) niv, dur (droite (haut c)) niv, dur (haut c) niv) of
                            (True,True,False) -> Etat (deplaceDansEnvi n (haut c) envi) niv r m s mor
                            (True,False,False) -> Etat (deplaceDansEnvi n (haut (droite c)) envi) niv r m s mor
                            (True,_,True) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Droite 0 c metier))) envi) niv r m s mor
                            (False,_,_) -> 
                                case (passable (droite c) niv && passable (haut (droite c)) niv, passable (haut (droite c)) niv && passable (haut (haut (droite c))) niv, dur (bas c) niv) of
                                (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Droite 0 c metier))) envi) niv r m s mor
                                (True,_,_) -> Etat (deplaceDansEnvi n (gauche c) envi) niv r m s mor
                                (_,True,_) -> Etat (deplaceDansEnvi n (haut (gauche c)) envi) niv r m s mor
                                (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Poseur Droite 0 c metier))) envi) niv r m s mor
-- 若发生坠落则不会因坠落距离过大死亡 行为模式等于行走者
-- Flotteur
tourLemming n (Flotteur Gauche c metier) (Etat envi niv r m s mor) = 
    case metier of
        Mar -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Gauche c Mar))) envi) niv r m s mor
        Sto -> Etat (appliqueIdEnv n (const (Lem n (Stoppeur Gauche c Sto))) envi) (stopper c niv) r m s mor
        Cre -> Etat (appliqueIdEnv n (const (Lem n (Creuseur Gauche c Cre))) envi) niv r m s mor
        Pos -> Etat (appliqueIdEnv n (const (Lem n (Poseur Gauche 0 c Pos))) envi) niv r m s mor
        Gri -> Etat (appliqueIdEnv n (const (Lem n (Grimpeur Gauche c Gri))) envi) niv r m s mor
        Sol -> Etat (appliqueIdEnv n (const (Lem n (Soldier Gauche 0 c Sol))) envi) niv r m s mor
        Mon -> Etat (appliqueIdEnv n (const (Lem n (Monster Gauche 0 c Min))) envi) niv r m s mor
        Flo ->
            case trouveSortie niv of
            Nothing -> suite 
            Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) mor else suite
            where suite = 
                    case (passable (gauche c) niv && passable (haut (gauche c)) niv, passable (haut (gauche c)) niv && passable (haut (haut (gauche c))) niv, dur (bas c) niv) of
                    (_,_,False) -> Etat (deplaceDansEnvi n (bas c) envi) niv r m s mor
                    (True,_,_) -> Etat (deplaceDansEnvi n (gauche c) envi) niv r m s mor
                    (_,True,_) -> Etat (deplaceDansEnvi n (haut (gauche c)) envi) niv r m s mor
                    (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Flotteur Droite c metier))) envi) niv r m s mor
tourLemming n (Flotteur Droite c metier) (Etat envi niv r m s mor) =
    case metier of
        Mar -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Droite c Mar))) envi) niv r m s mor
        Sto -> Etat (appliqueIdEnv n (const (Lem n (Stoppeur Droite c Sto))) envi) (stopper c niv) r m s mor
        Cre -> Etat (appliqueIdEnv n (const (Lem n (Creuseur Droite c Cre))) envi) niv r m s mor
        Pos -> Etat (appliqueIdEnv n (const (Lem n (Poseur Droite 0 c Pos))) envi) niv r m s mor
        Gri -> Etat (appliqueIdEnv n (const (Lem n (Grimpeur Droite c Gri))) envi) niv r m s mor
        Sol -> Etat (appliqueIdEnv n (const (Lem n (Soldier Droite 0 c Sol))) envi) niv r m s mor
        Mon -> Etat (appliqueIdEnv n (const (Lem n (Monster Droite 0 c Min))) envi) niv r m s mor
        Flo -> 
            case trouveSortie niv of
            Nothing -> suite 
            Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) mor else suite
            where suite = 
                    case (passable (droite c) niv && passable (haut (droite c)) niv, passable (haut (droite c)) niv && passable (haut (haut (droite c))) niv, dur (bas c) niv) of
                    (_,_,False) -> Etat (deplaceDansEnvi n (bas c) envi) niv r m s mor
                    (True,_,_) -> Etat (deplaceDansEnvi n (droite c) envi) niv r m s mor
                    (_,True,_) -> Etat (deplaceDansEnvi n (haut (droite c)) envi) niv r m s mor
                    (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Flotteur Gauche c metier))) envi) niv r m s mor
-- 行动槽需要三轮攒满
-- 行动槽满时可以行动一次 行动包括 改变方向 攻击人 行走 
-- 如果面朝方向一格有人 则会优先攻击格子中的第一个人
-- 坠落时一轮行动一次 但会清空其行动槽
-- Monster
tourLemming n (Monster Gauche temps c metier) (Etat envi niv r m s mor) = 
        if temps == 3 
            then
                case trouvePersonne (gauche c) envi of
                        Just i -> Etat (appliqueIdEnv i (const (Lem i (Mort c))) (appliqueIdEnv n (const (Lem n (Monster Gauche 0 c metier))) envi)) niv r m s mor
                        Nothing -> 
                            case (passable (gauche c) niv && passable (haut (gauche c)) niv, passable (haut (gauche c)) niv && passable (haut (haut (gauche c))) niv, dur (bas c) niv) of
                            (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Gauche 0 c metier))) envi) niv r m s mor
                            (True,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Monster Gauche 0 (gauche c) metier))) (deplaceDansEnvi n (gauche c) envi)) niv r m s mor
                            (_,True,_) -> Etat (appliqueIdEnv n (const (Lem n (Monster Gauche 0 (gauche (haut c)) metier))) (deplaceDansEnvi n (gauche (haut c)) envi)) niv r m s mor
                            (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Monster Droite 0 c metier))) envi) niv r m s mor 
            else Etat (appliqueIdEnv n (const (Lem n (Monster Gauche (temps+1) c metier))) envi) niv r s s mor
tourLemming n (Monster Droite temps c metier) (Etat envi niv r m s mor) = 
        if temps == 3 
            then
                case trouvePersonne (droite c) envi of
                        Just i -> Etat (appliqueIdEnv i (const (Lem i (Mort c))) (appliqueIdEnv n (const (Lem n (Monster Droite 0 c metier))) envi)) niv r m s mor 
                        Nothing -> 
                            case (passable (droite c) niv && passable (haut (droite c)) niv, passable (haut (droite c)) niv && passable (haut (haut (droite c))) niv, dur (bas c) niv) of
                            (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Droite 0 c metier))) envi) niv r m s mor
                            (True,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Monster Droite 0 (droite c) metier))) (deplaceDansEnvi n (droite c) envi)) niv r m s mor
                            (_,True,_) -> Etat (appliqueIdEnv n (const (Lem n (Monster Droite 0 (droite (haut c)) metier))) (deplaceDansEnvi n (droite (haut c)) envi)) niv r m s mor
                            (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Monster Gauche 0 c metier))) envi) niv r m s mor 
            else Etat (appliqueIdEnv n (const (Lem n (Monster Droite (temps+1) c metier))) (appliqueIdEnv n (const (Lem n (Monster Droite 0 c metier))) envi)) niv r s s mor
-- 行动10次之后能量槽满
-- 坠落时无法积攒能量
-- 能量槽满时 如果怪物在面前一格内 可以击杀怪物
-- 其余行动模式同行走者
-- Soldier
tourLemming n (Soldier Gauche temps c metier) (Etat envi niv r m s mor) = 
    case metier of
        Mar -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Gauche c Mar))) envi) niv r m s mor
        Sto -> Etat (appliqueIdEnv n (const (Lem n (Stoppeur Gauche c Sto))) envi) (stopper c niv) r m s mor
        Cre -> Etat (appliqueIdEnv n (const (Lem n (Creuseur Gauche c Cre))) envi) niv r m s mor
        Pos -> Etat (appliqueIdEnv n (const (Lem n (Poseur Gauche 0 c Pos))) envi) niv r m s mor
        Gri -> Etat (appliqueIdEnv n (const (Lem n (Grimpeur Gauche c Gri))) envi) niv r m s mor
        Flo -> Etat (appliqueIdEnv n (const (Lem n (Flotteur Gauche c Flo))) envi) niv r m s mor
        Mon -> Etat (appliqueIdEnv n (const (Lem n (Monster Gauche 0 c Min))) envi) niv r m s mor
        Sol ->
            case trouveSortie niv of
            Nothing -> suite 
            Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) mor else suite
            where suite = 
                    if temps == 10 
                        then 
                            case trouveMonster (gauche c) envi of
                                Nothing ->
                                    case (passable (gauche c) niv && passable (haut (gauche c)) niv, passable (haut (gauche c)) niv && passable (haut (haut (gauche c))) niv, dur (bas c) niv) of
                                    (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Gauche 0 c metier))) envi) niv r m s mor
                                    (True,_,_) -> Etat (deplaceDansEnvi n (gauche c) envi) niv r m s mor
                                    (_,True,_) -> Etat (deplaceDansEnvi n (haut (gauche c)) envi) niv r m s mor
                                    (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Soldier Droite temps c metier))) envi) niv r m s mor
                                Just i -> Etat (enleveEnvi i (appliqueIdEnv n (const (Lem n (Soldier Droite 0 c metier))) envi)) niv r m s mor 
                        else 
                            case (passable (gauche c) niv && passable (haut (gauche c)) niv, passable (haut (gauche c)) niv && passable (haut (haut (gauche c))) niv, dur (bas c) niv) of
                            (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Gauche 0 c metier))) envi) niv r m s mor
                            (True,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Soldier Gauche (temps+1) c metier))) (deplaceDansEnvi n (gauche c) envi)) niv r m s mor
                            (_,True,_) -> Etat (appliqueIdEnv n (const (Lem n (Soldier Gauche (temps+1) c metier))) (deplaceDansEnvi n (haut (gauche c)) envi)) niv r m s mor
                            (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Soldier Droite (temps+1) c metier))) envi) niv r m s mor
tourLemming n (Soldier Droite temps c metier) (Etat envi niv r m s mor) = 
    case metier of
        Mar -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Droite c Mar))) envi) niv r m s mor
        Sto -> Etat (appliqueIdEnv n (const (Lem n (Stoppeur Droite c Sto))) envi) (stopper c niv) r m s mor
        Cre -> Etat (appliqueIdEnv n (const (Lem n (Creuseur Droite c Cre))) envi) niv r m s mor
        Pos -> Etat (appliqueIdEnv n (const (Lem n (Poseur Droite 0 c Pos))) envi) niv r m s mor
        Gri -> Etat (appliqueIdEnv n (const (Lem n (Grimpeur Droite c Gri))) envi) niv r m s mor
        Flo -> Etat (appliqueIdEnv n (const (Lem n (Flotteur Droite c Flo))) envi) niv r m s mor
        Mon -> Etat (appliqueIdEnv n (const (Lem n (Monster Droite 0 c Min))) envi) niv r m s mor
        Sol ->
            case trouveSortie niv of
            Nothing -> suite 
            Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) mor else suite
            where suite = 
                    if temps == 10
                        then 
                            case trouveMonster (droite c) envi of
                                Nothing ->
                                    case (passable (droite c) niv && passable (haut (droite c)) niv, passable (haut (droite c)) niv && passable (haut (haut (droite c))) niv, dur (bas c) niv) of
                                    (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Droite 0 c metier))) envi) niv r m s mor
                                    (True,_,_) -> Etat (deplaceDansEnvi n (droite c) envi) niv r m s mor
                                    (_,True,_) -> Etat (deplaceDansEnvi n (haut (droite c)) envi) niv r m s mor
                                    (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Soldier Gauche temps c metier))) envi) niv r m s mor
                                Just i -> Etat (enleveEnvi i (appliqueIdEnv n (const (Lem n (Soldier Gauche 0 c metier))) envi)) niv r m s mor 
                        else 
                            case (passable (droite c) niv && passable (haut (droite c)) niv, passable (haut (droite c)) niv && passable (haut (haut (droite c))) niv, dur (bas c) niv) of
                            (_,_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Droite 0 c metier))) envi) niv r m s mor
                            (True,_,_) -> Etat (deplaceDansEnvi n (droite c) envi) niv r m s mor
                            (_,True,_) -> Etat (deplaceDansEnvi n (haut (droite c)) envi) niv r m s mor
                            (_,_,_) -> Etat (appliqueIdEnv n (const (Lem n (Soldier Gauche (temps+1) c metier))) envi) niv r m s mor    
            
tourEntite :: Int -> Etat -> Etat
tourEntite n et = case trouveIdEnv n (enviE et) of
                    Nothing -> et
                    Just (Lem _ l) -> tourLemming n l et

popLem :: Etat -> Etat
popLem (Etat envi niv r v s mor) = case trouveEntree niv of
                                Nothing -> Etat envi niv r v s mor
                                Just c -> Etat nenvi niv (r-1) (v+1) s mor
                                            where nenvi = addEntite nlem envi
                                                  nlem = Lem (idFrais envi) (Tombeur Droite 0 c Sto)


tourEtat :: Int -> Etat -> Either Fin Etat 
tourEtat t e = (verif . pop) $ foldr etape e (entitesEnvi2 (enviE e))
                 where etape enti acc = tourEntite (idEnt enti) acc
                       pop = if restants > 0 && t `mod` 5 == 0 then popLem else id 
                       restants = lrestantsE e
                       verif et = if lrestantsE et == 0 && lvivantsE et == 0
                                     then Left $ Victoire $ lsauvesE et
                                     else Right et                                                 
                                              