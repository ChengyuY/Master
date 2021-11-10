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
import Stats(countChar,countLettre,countLine,countWord,top10Freq)
main :: IO ()
main = 
    do
        intStr <- TIO.readFile "pg9645.txt" ;
        putStr "Total charaters : ";
        putStrLn (show (countChar intStr));
        putStr "Total lines : ";
        putStrLn (show (countLine intStr));
        putStr "Total words : ";
        putStrLn (show (countWord intStr));
        putStr "Top 10 frequence : \n";
        putStrLn (show (top10Freq (countLettre intStr)));
