-- Option de compilation pour que les chaînes litérales
-- soient surchargées plutôt que fixées à `[Char]`
{-# LANGUAGE OverloadedStrings #-}
module Main where
-- Le type `Text` est disponible
import Data.Text (Text)
-- Les fonctions de manipulation de textes
-- sont préfixées par `T` plutôt que `Data.Text`
import qualified Data.Text as T
-- Les fonctions d'entrées sorties pour les textes
import qualified Data.Text.IO as TIO
main :: IO ()
main = do
    ls <- TIO.readFile "pg9645.txt" 
