{-# LANGUAGE OverloadedStrings #-}

module Stats
    ( someFunc,
    countChar,
    countLettre,
    countLine,
    countWord,
    top10Freq
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sortBy)

data List a = Nil
            | Cons a (List a)
            deriving (Show)
  
someFunc :: IO ()
someFunc = putStrLn "someFunc"

countChar :: Text  -> Int
countChar = T.foldl (\x y -> x + 1) 0 
 
-- >>> countChar "sss" 
-- Variable not in scope: putInt :: Int -> f0
-- >>> show $ countChar "ssss"
-- "4"

countWord :: Text -> Int 
countWord = T.foldl (\x w -> if w == ' ' then x + 1 else x) 1

-- >>> countWord "s s s" 
-- 3
-- >>> show $ countWord "s s s s"
-- "4"

countLine :: Text -> Int 
countLine = T.foldl (\x l -> if l == '\n' then x + 1 else x) 1

-- >>> countLine "s \n s" 
-- 2
-- >>> show $ countWord "s \n \n a"
-- "3"

countLettre :: Text -> [(Char ,Int)]
countLettre list = 
    [(x,(length.filter(==x)) $T.unpack list)| x<-['a'..'z']]
    
top10Freq :: [(Char , Int )] -> [(Char , Int )] 
top10Freq freq = take 10 (sortBy(\(_,x) (_,y) -> compare y x) freq)




-- >>> countLettre "bcsaihefofwajoasai" 
-- [('a',4),('b',1),('c',1),('d',0),('e',1),('f',2),('g',0),('h',1),('i',2),('j',1),('k',0),('l',0),('m',0),('n',0),('o',2),('p',0),('q',0),('r',0),('s',2),('t',0),('u',0),('v',0),('w',1),('x',0),('y',0),('z',0)]
-- >>> countLettre "s s s s"
-- [('a',0),('b',0),('c',0),('d',0),('e',0),('f',0),('g',0),('h',0),('i',0),('j',0),('k',0),('l',0),('m',0),('n',0),('o',0),('p',0),('q',0),('r',0),('s',4),('t',0),('u',0),('v',0),('w',0),('x',0),('y',0),('z',0)]

-- >>> top10Freq $ countLettre "aaaassfwqfqwvqwqrcxwqrxxxxqx"
-- [('q',6),('x',6),('a',4),('w',4),('f',2),('r',2),('s',2),('c',1),('v',1),('b',0)]
     










