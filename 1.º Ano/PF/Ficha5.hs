-- Exercicio 1 --

any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) | p x = True
              | otherwise = any' p xs

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys 
zipWith' _ _ _ = [] 


takeWhile' :: (a->Bool) -> [a] -> [a] 
takeWhile' f [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = []

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = [x]

span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (x:xs) | f x = (x : l1, l2)
               | otherwise = ([], (x:xs))
   where (l1,l2) = span' f xs

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (x:xs) = insert' f x (sortOn' f xs)

insert' :: Ord b => (a -> b) -> a -> [a] -> [a]
insert' f x [] = [x]
insert' f x (h:t) | f x <= f h = x : h : t 
                 | otherwise = h : insert' f x t  

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f _ [] = []
deleteBy' f x (h:t) | f x h = t  
                    | otherwise = h : deleteBy' f x t

-- Exercicio 2 --

type Polinomio = [Monomio]
type Monomio = (Float,Int)

pol1, pol2 :: Polinomio
pol1 = [(2,3),(3,4),(5,3),(4,5)]
pol2 = [(2,3),(7,4),(5,0),(45,3),(-9,3)]

selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\x -> snd x == n) p

-- (\ ARGUMENTOS -> CORPO DA FUNÇÃO)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n p = length (filter (\(x,y) -> y == n ) p) -- ou apenas length (selgrau n p)

conta2 :: Int -> Polinomio -> Int
conta2 n p = foldr (\(c,e) r -> if e == n then 1 + r else r) 0 p

grau :: Polinomio -> Int
grau p = foldr (\(c,e) r -> max e r) 0 p

grau1 p = foldr (\x r -> max (snd x) r) 0 p 

deriv :: Polinomio -> Polinomio
deriv p = map derivMon p
   where derivMon :: Monomio -> Monomio
         derivMon (c,e) = (c * (fromIntegral e) ,e-1)

deriv2 :: Polinomio -> Polinomio
deriv2 p = map (\(c,e) -> (c * (fromIntegral e) ,e-1)) p

calcula :: Float -> Polinomio -> Float
calcula x p = foldr (\(c,e) r -> (c * x ^ e) + r) 0 p

simp :: Polinomio -> Polinomio
simp p = foldr (\(c,e) r -> if e == 0 then r else (c,e) : r) [] p

mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) p = map (\(c,e) -> (c*x,y+e)) p

mult1 :: Monomio -> Polinomio -> Polinomio
mult1 (a,b) p = [(a*c, b+e) | (c,e) <- p]

mult2 :: Monomio -> Polinomio -> Polinomio
mult2 (a,b) p = foldr (\(c,e) r -> (a*c,b+e) : r) [] p
 
-- Exercicio 3 --

type Mat a = [[a]]

mat1, mat2 :: Mat Int
mat1 = [[1,2,3],[0,4,5],[0,0,6]]
mat2 = [[1,2],[3,4],[5,6]]

dimOK :: Mat a -> Bool
dimOK m = iguais (map length m)
   where iguais :: [Int] -> Bool
         iguais [] = True
         iguais (x:xs) = all (==x) xs

dimMat :: Mat a -> (Int,Int)
dimMat m@(l:ls) = (length m, length l)

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat m1@(a:as) m2@(b:bs) = if dimOK m1 && dimOK m2 then (zipWith (+) a b) : addMat as bs
                                                     else [] 
addMat _ _ = []

transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = (map head m) : transpose (map tail m)

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat (l:ls) m = (linha l m) : multMat ls m  
    where linha l m = sum (zipWith (*) l (map head m)) : linha l (map tail m)
multMat _ m = []




















