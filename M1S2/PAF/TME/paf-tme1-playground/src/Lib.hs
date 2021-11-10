module Lib
    ( someFunc
    ,maxInt
    ,fibo
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

maxInt :: Integer->Integer->Integer
maxInt x y = if x > y then x
    else if x < y then y
    else x

fibo :: Integer->Integer
fibo n = if (n==0) || (n==1) then 1
    else fibo (n-1) + fibo (n-2)
