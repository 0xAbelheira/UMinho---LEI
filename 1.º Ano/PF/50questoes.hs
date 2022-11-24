import 

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b | a == b = [b]
                | otherwise = a : enumFromTo' (a+1) b

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z
               | x > z && (y-x) > 0 = []
               | x < z && (y-x) < 0 = []
               | otherwise = (x : enumFromThenTo' y (y + (y - x)) z)

concatena :: [a] -> [a] -> [a]
concatena [] [] = []
concatena [] l2 = l2
concatena (x:xs) l = x : concatena xs l

encontra :: [a] -> Int -> a
encontra (x:xs) a | a == 0 = x 
                  | otherwise = encontra xs (a - 1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 l = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys 

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse e [x] = [x]
intersperse e (x:xs) = x : e : intersperse e xs

--group' :: Eq a => [a] -> [[a]]
--group' [] = []
--group' (x:xs) = (x : takeWhile (==x) xs) : group' (dropWhile (==x) xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]

tails :: [a] -> [[a]]
tails [] = [[]]
tails l = [l] ++ tails (tail l) 

heads :: [[a]] -> [a]
heads [] = []
heads ([]:t) = heads t
heads (x:xs) = head x : heads xs

total :: [[a]] -> Int
total [] = 0
total (x:xs) = length x + total xs

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):xs) = [(a,c)] ++ fun xs

cola :: [(String,b,c)] -> String
cola [] = ""
cola ((x,a,b):xs) = x ++ cola xs

idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade a1 i ((x,a2):xs) = if ((a1 - a2) >= i) then [x] ++ idade a1 i xs
                                             else idade a1 i xs

powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m | m > 1 = powerEnumFrom n (m-1) ++ [n^(m-1)]
                  | otherwise = []

isPrime :: Int -> Bool
isPrime n 
    | n >= 2 = primeCheck n 2
    | otherwise = False 

primeCheck :: Int -> Int -> Bool
primeCheck n m 
    | m * m > n = True
    | mod n m == 0 = False 
    | otherwise = primeCheck n (m+1)      


isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False 
isPrefixOf (x:xs) (y:ys) = if x == y && isPrefixOf xs ys then True else False

isSuffixOf :: Eq a => [a] -> [a] -> Bool 
isSuffixOf [] _ = True
isSuffixOf _ [] = False 
isSuffixOf l (x:xs) = l == (x:xs) || isSuffixOf l xs


isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) = if x == y && isSubsequenceOf xs ys || isSubsequenceOf (x:xs) ys then True else False

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x l = elemIndicesAux x l 0

elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux _ [] _ = []
elemIndicesAux x (h:t) i 
    | x == h = i : elemIndicesAux x t (i+1)
    | otherwise = elemIndicesAux x t (i+1)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : filter (/= x) (nub xs)

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (x:xs) | n == x = xs
                 | otherwise = x : delete' n xs 

remove' :: Eq a => [a] -> [a] -> [a]
remove' [] _ = []
remove' l1 [] = l1 
remove' l1 (x:xs) = remove' (delete' x l1) xs    

union' :: Eq a => [a] -> [a] -> [a]
union' l1 [] = l1
union' l (x:xs) | elem x l = union' l xs 
                | otherwise = union' (l ++ [x]) xs 

intersect :: Eq a => [a] -> [a] -> [a]
intersect _ [] = []
intersect (x:xs) l | elem x l = x : intersect l xs
                   | otherwise = intersect l xs

insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (x:xs) | n < x = n : x : xs
                 | otherwise = x : insert' n xs

unwords' :: [String] -> String
unwords' [] = ""
unwords' (x:xs) = x ++ " " ++ unwords' xs

unlines' :: [String] -> String
unlines' [] = ""
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

--pMaior :: Ord a => [a] -> Int
--pMaior a =

--iSort :: Ord a => [a] -> [a]
--iSort (x:xs) = insert x (iSort xs)





















