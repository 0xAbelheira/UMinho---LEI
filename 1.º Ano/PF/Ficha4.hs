import Data.Char 
import Data.List

-- Exercicio 1 --
s1 :: String
s1 = "abc27def420bruh000"

{-
digitAlpha :: String -> (String,String)
digitAlpha s = 

auxDA :: String -> String
auxDA [] = ("String","Vazia") 
auxDA (x:xs) | ord x > 64 && ord x < 91 || ord x > 96 && ord x < 122 = 1
             | ord x > 47 && ord x < 58 = 2
-}
-- Exercicio 2 -- 

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (x:xs) | x > 0 = (a,b,c+1)
           | x < 0 = (a+1,b,c)
           | x == 0 = (a,b+1,c)
          where (a,b,c) = nzp xs

-- Exercicio 4 --

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t

fromDigits' :: [Int] -> Int
fromDigits' l = fromDigitsAC l 0
     where fromDigitsAC :: [Int] -> Int -> Int 
           fromDigitsAC (x:xs) ac = fromDigitsAC xs (x + ac * 10)
           fromDigitsAC [] ac = ac 

-- Exercicio 5 --
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l] 

-- Exercicio 6 --
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' :: Int -> Int
fib' 0 = 0 
fib' 1 = 1
fib' n = fibAC n (0,1)
       where fibAC :: Int -> (Int,Int) -> Int
             fibAC 0 (a,b) = a
             fibAC 1 (a,b) = b
             fibAC n (a,b) = fibAC (n-1) (b,a+b)  
































































