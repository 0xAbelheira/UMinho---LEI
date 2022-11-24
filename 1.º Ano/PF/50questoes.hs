module Questoes where

import Data.List

--exercício 1

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' start end 
    | start == end = [end]
    | otherwise = start:enumFromTo' (start+1) end

--exercício 2

enumFromThenTo' :: Int -> Int -> Int ->[Int]
enumFromThenTo' start next end
    | start > end = []
    | otherwise = start:enumFromThenTo' next (2 * next - start) end

--exercício 3

addadd' :: [a] -> [a] -> [a]
addadd' [] [] = []
addadd' [] y = y
addadd' (x:xs) y = x:addadd' xs y

--exercício 4

exclamation' :: [a] -> Int -> a
exclamation' (x:xs) y 
    | y == 0 = x
    | otherwise = exclamation' xs (y-1)

--exercício 5

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--exercício 6

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 n = []
take' n (x:xs) = x:take' (n-1) xs 

--exercício 7

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 n = []
drop' n (x:xs) = drop' (n-1) xs
--exercício 8

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

--exercício 9

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) = n == x || elem' n xs

--exercício 10

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:replicate' (n-1) x

--exercício 11

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [a] = [a]
intersperse' a (x:xs) = x:a:intersperse' a xs


--exercício 12

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (x:xs) = (x:takeWhile(== x) xs):group' (dropWhile(==x) xs)

--exercício 13

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

--exercício 14

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

--exercício 15

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = l:tails' (tail l) 
--exercício 16

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' _ [] = False
isPrefixOf' [] _ = True
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

--exercício 17 

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' _ [] = True
isSuffixOf' [] _ = False
isSuffixOf' l ls@(_:t) = l == ls || isSuffixOf' l t

--exercício 18

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (x:xs) (y:ys) = x == y && isSubsequenceOf' xs ys || isSubsequenceOf' (x:xs) ys

--exercício 19

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' a (x:xs) | a == x = 0:map (+1) (elemIndices' a xs)
                      | otherwise = map (+1) (elemIndices' a xs)

--exercício 20

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x:filter (/=x) (nub' xs)

--exercício 21

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' a (x:xs) | a == x = xs
                 | otherwise = x:delete' a xs

--exercício 22

slash' :: Eq a => [a] -> [a] -> [a]
slash' l [] = l
slash' _ [] = []
slash' l (x:xs) = slash' (delete' x l) xs

--exercício 23

union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l
union' l (x:xs) | elem x l = union' l xs
                | otherwise = union' (l ++ [x]) xs

--exercício 24

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' (x:xs) l | elem x l = x:intersect' xs l
                    | otherwise = intersect' xs l

--exercício 25

insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (x:xs) | a > x = x: insert' a xs
                 | otherwise = a:x:xs

--exercício 26

unwords' :: [String] -> String
unwords' [] = ""
unwords' (x:xs) = x ++ (if xs == [] then "" else " ") ++ unwords' xs

--exercício 27

unlines' :: [String] -> String
unlines' [] = ""
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

--exercício 28

pMaior :: Ord a => [a] -> Int
pMaior [a] = 0
pMaior (x:xs) | x > (xs !! l) = 0
              | otherwise = 1 + l
              where l = pMaior xs

--exercício 29

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (x:xs) = elem x xs || temRepetidos xs

--exercício 30

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x:xs) | elem x  ['0'..'9'] = x:algarismos xs
                  | otherwise = algarismos xs

--exercício 31

posImpares :: [a] -> [a]
posImpares [] = []
posImpares [c] = []
posImpares (h:x:xs) = x:posImpares xs

--exercício 32

posPares :: [a] -> [a]
posPares [] = []
posPares [c] = []
posPares (h:x:xs) = h:posPares xs

--exercício 33

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [c] = True
isSorted (h:x:xs) = x >= h && isSorted (x:xs)

--exercício 34

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

--exercício 35

menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor (x:xs) (y:ys) = x < y || menor xs ys

--exercício 36

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((x,n):xs) = a == x || elemMSet a xs

--exercício 37

lenghtMSet :: [(a,Int)] -> Int
ĺenghtMSet [] = 0
lenghtMSet ((x,n):xs) = n + lenghtMSet xs

--exercício 38

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,n):xs) = replicate n x ++ converteMSet xs

--exercício 39

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,n):xs) = if a == x then (x,n+1):xs else (a,n):insereMSet a xs

{-estava a dar-me erro a fazer com "|" então fiz com if's-}

--exercício 40

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet a ((x,n):xs) = if a == x then xs else (x,n):removeMSet a xs

--exercício 41

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (l:ls) = insereMSet l (constroiMSet ls)

--exercício 42

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a):xs) = (a:as,bs)
    where (as,bs) = partitionEithers' xs
partitionEithers' ((Right b):xs) = (as,b:bs)
    where (as,bs) = partitionEithers' xs

--exercício 43

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just x):xs) = x:catMaybes' xs
catMaybes' (Nothing:xs) = catMaybes' xs

--exercício 44

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x, y) (m1:m2) = posicao (case m1 of Norte -> (x, y+1)
                                             Sul -> (x, y-1)
                                             Este -> (x+1, y)
                                             Oeste -> (x-1, y)) m2

--exercício 45

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) | yi < yf = Norte:caminho (xi, yi + 1) (xf, yf)
                        | yi > yf = Sul:caminho (xi, yi - 1) (xf, yf)
                        | xi < xf = Este:caminho (xi + 1, yi) (xf, yf)
                        | xi > xf = Oeste:caminho (xi - 1, yi) (xf, yf)
                        | otherwise = []

--exercício 46

vertical :: [Movimento] -> Bool 
vertical [] = True
certical (l:ls) = case l of Este -> False
                            Oeste -> False
                            otherwise -> vertical ls

--exercício 47

data Posicao = Pos Int Int deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral = foldl1 (\(Pos xa ya) (Pos x y) -> if (xa^2 + ya^2) > (x^2 + y^2) then (Pos x y) else (Pos xa ya))

--exercício 48

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos x y) ps = filter (\(Pos x1 y1) -> (abs(x1-x) + abs(y1-y) ==1)) ps

--exercício 49

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [(Pos x y)] = True
mesmaOrdenada ((Pos x y):(Pos x1 y1):ps) = y == y1 && mesmaOrdenada ((Pos x1 y1):ps)

--exercício 50

data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK ss = length [s | s <- ss, case s of Vermelho -> False; otherwise -> True] < 2
