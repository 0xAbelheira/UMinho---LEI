import Data.Char

-- Ficha 2 --
-- Exercicio 1 -- 
--a) 39 (faz o somatório do quadrado dos elementos da lista)
--b) [8,12] (guarda os pares de uma lista)
--c) [5] ||| funC (1:2:[3,4,5]) = funC (3:4:[5]) = funC [5] = [5]
--d) "certo" ||| funD "otrec" = g [] "otrec" = g ('o':[]) "trec" = g ('t':"o") "rec" 
--                                           = g ('r':"to") "ec" 
--                                           = g ('e':"rto") "c"
--                                           = g ('c':"erto") []
--                                           = g ("certo") [] = "certo"


-- Exercicio 2 --
dobros :: Num a => [a] -> [a]
dobros [] = []
dobros (x:xs) = 2 * x : dobros xs

-- Não são da ficha2 --
duplica :: Num a => [a] -> [a]
duplica [] = []
duplica (x:xs) = x : x : duplica xs

pares :: Num a => [a] -> [(a,a)]
pares [] = []
pares [x] = [(x,x)]
pares (x:y:xs) = (x,y) : pares xs
------------------------

numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (x:xs) = if toUpper c == toUpper x then 1 + numOcorre c xs
                                               else {- 0+ -} numOcorre c xs 

positivos :: [Int] -> Bool   
positivos [] = True
positivos (x:xs) = if x > 0 && positivos xs then True else False

soPos :: [Int] -> [Int]                         
soPos [] = []
soPos (x:xs) = if x >= 0 then (x : soPos xs)
                        else soPos xs

somaNeg :: [Int] -> Int
somaNeg [] = 0 
somaNeg (x:xs) = if x < 0 then x + somaNeg xs else somaNeg xs 

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (x:xs) | (length (x:xs)) <= 3 = (x:xs)
               | (length (x:xs)) > 3 = tresUlt xs
{-
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt [x,y,z] = [x,y,z]
tresUlt (a:b:c:d:xs) = tresUlt (b:c:d:xs)
tresUlt x = x
-}

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):xs) = y : segundos xs

{- Tão eficiente como a minha 
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos (h:t) = snd h : segundos t 
-}

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False 
nosPrimeiros a ((x,y):xs) = if a == x || nosPrimeiros a xs then True
                                                           else False

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((x1,y1,z1):(x2,y2,z2):xs) = sumTriplos ((x1+x2,y1+y2,z1+z2):xs)

-- Exercicio 3 --
soDigitos :: [Char] -> [Char] -- é suposto dar uma string, certo? 
soDigitos [] = []
soDigitos (x:xs) = if (ord x) >= 48 && (ord x) <= 57 then x : soDigitos xs
                                                     else soDigitos xs

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) = if (ord x) >= 97 && (ord x) <= 122 then 1 + minusculas xs
                                                       else minusculas xs

nums :: String -> [Int]
nums [] = []
nums (x:xs) = if (ord x) >= 48 && (ord x) <= 57 then (ord x - 48) : nums xs
                                                else nums xs

-- Exercicio 4 -- 

type Polinomio = [Monomio]
type Monomio = (Float,Int) 

conta :: Int -> Polinomio -> Int 
conta _ [] = 0
conta a ((x,y):xs) | a == y = 1 + conta a xs
                   | a /= y = conta a xs

grau :: Polinomio -> Int
grau [] = 0
grau ((x1,y1):xs) = if y1 > grau xs then y1 else grau xs

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau a ((x,y):xs) = if a == y then (x,y): selgrau a xs else selgrau a xs

deriv :: Polinomio -> Polinomio -- esta função só funciona caso o Monomio seja formado por dois inteiros
deriv [] = []
deriv ((x,y):xs) | y == 0 = deriv xs 
                 | y == 1 = (x,0) : deriv xs
                 | y > 1 = (x * (fromIntegral y),(y-1)) : deriv xs 

calcula :: Float -> Polinomio -> Float 
calcula _ [] = 0
calcula r ((x,y):xs) = x * (r^y) + calcula r xs

simp :: Polinomio -> Polinomio
simp [] = []
simp ((x,y):xs) = if y == 0 then simp xs else (x,y) : simp xs

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (x1,y1) ((x2,y2):xs) = ((x1*x2,y1+y2): mult (x1,y1) xs)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [(x1,y1)] = [(x1,y1)]
normaliza ((x1,y1):(x2,y2):xs) | y1 == y2 = normaliza ((x1+x2,y1):xs)
                               | (conta y1 xs) /= 0 = normaliza ((x1,y1) : xs ++ [(x2,y2)])
                               | (conta y1 xs) == 0 = (x1,y1) : normaliza ((x2,y2) : xs)

----------------------opção da professora----------------------
normaliza' :: Polinomio -> Polinomio
normaliza' [] = []
normaliza' (m:ms) = insereMon m (normaliza' ms)  

insereMon :: Monomio -> Polinomio -> Polinomio
insereMon m [] = [m]
insereMon (c,e) ((a,b):xs) = if e == b then (c+a,e) : xs
                                       else (a,b) : insereMon (c,e) xs 
---------------------------------------------------------------

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

produto :: Polinomio -> Polinomio -> Polinomio 
produto _ [] = []
produto (p:p1) p2 = soma (mult p p2) (produto p1 p2) 

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (m:ms) = insere m (ordena ms)

insere :: Monomio -> Polinomio -> Polinomio
insere m [] = [m]
insere (x1,y1) ((x2,y2):xs) = if (y1 <= y2) then normaliza ((x1,y1) : (x2,y2) : xs)
                                            else normaliza ((x2,y2) : insere (x1,y1) xs)

p1 :: Polinomio
p1 = [(4.3,5), (7,3), (8,7)]

p2 :: Polinomio
p2 = p1 ++ [(5,4), (2,3), (-1,5)]

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = let p1n = (normaliza p1)
                  p2n = (normaliza p2)
                  p1nO = (ordena p1n)
                  p2nO = (ordena p2n)
              in  (p1nO == p2nO)

----------------OU---------------------

equiv' :: Polinomio -> Polinomio -> Bool
equiv' p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)














