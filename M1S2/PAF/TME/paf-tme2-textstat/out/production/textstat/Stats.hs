{-# LANGUAGE OverloadedStrings #-}

module Stats
    ( someFunc
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data List a = Nil
            | Cons a (List a)
            deriving (Show)
  
someFunc :: IO ()
someFunc = putStrLn "someFunc"

listFoldl :: (b -> a -> b) -> b -> List a -> b
listFoldl f acc Nil = acc
listFoldl f acc (Cons x xs) = listFoldl f (f acc x) xs

countChars :: Text -> Int
countChars file = listFoldl (\ letter -> x + 1) 0










