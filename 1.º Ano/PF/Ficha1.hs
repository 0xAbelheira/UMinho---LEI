module Ficha1 where 
import Data.Char
-- Learn Haskell for Great Good! (livro)
-- Ficha 1 --
-- Exercicio 1 --

perimetro :: Double -> Double 
perimetro r = 2 * pi * r

dist :: (Double,Double) -> (Double,Double) -> Double 
dist (x1,y1) (x2,y2) = sqrt((x2-x1)^2+(y2-y1)^2)

primUlt :: [a] -> (a,a)
primUlt l = (head l , last l)

multiplo :: Int -> Int -> Bool
multiplo m n = if (mod m n) == 0 then True
                                 else False 

truncaImpar :: [a] -> [a]
truncaImpar [] = []
truncaImpar l = if (mod (length l) 2) == 0 then l
                                           else (tail l)

max2 :: Int -> Int -> Int 
max2 x y = if x >= y then x
                     else y

max3 :: Int -> Int -> Int -> Int 
max3 x y z = max2 z (max2 x y)

-- Aparte -- 

fact 0 = 1 
fact 1 = 1 
fact n = n * (fact (n-1))


-- Exercicio 2 --

nRaizes :: Double -> Double  -> Double -> Int 
nRaizes a b c | (a == 0 || (b^2 - 4*a*c) == 0) = 1
              | (b^2 - 4*a*c) < 0 = 0
              | (b^2 - 4*a*c) > 0 = 2

raizes :: Double -> Double -> Double -> [Double]
raizes a b c | (nRaizes a b c) == 0 = []
             | otherwise = (frRaizes a b c)

frRaizes :: Double -> Double -> Double -> [Double]
frRaizes a b c = [((-b + sqrt(b^2 - 4*a*c)) / 2*a),((-b - sqrt(b^2 - 4*a*c)) / 2*a)]



-- Exercicio 3 --

type Hora = (Int,Int)

hvalid' :: Hora -> Bool
hvalid' (a,b) = if a < 0 || a > 24 then False else True && 
                if b < 0 || b > 59 then False else True

befORaft' :: Hora -> Hora -> Bool
befORaft' (a1,b1) (a2,b2) | a1 > a2 = False 
                          | a2 > a1 = True 
                          | a1 == a2 = if b2 > b1 then True else False

hourToMin' :: Hora -> Int 
hourToMin' (a,b) = a * 60 + b

minToHour' :: Int -> Hora
minToHour' a = (div a 60, mod a 60)

differenceHours' :: Hora -> Hora -> Int
differenceHours' (a1,b1) (a2,b2) = if befORaft' (a1,b1) (a2,b2) == True then hourToMin' (a2-a1,b2-b1)
                                                                        else hourToMin' (a1-a2,b1-b2)

addMin' :: Hora -> Int -> Hora 
addMin' (a,b) c = minToHour' (hourToMin' (a,b) + c)



-- Exercicio 4 -- 

data Hour = H Int Int deriving (Show,Eq)

hvalid :: Hour -> Bool
hvalid (H a b) = if a < 0 || a > 24 then False else True && 
                 if b < 0 || b > 59 then False else True

befORaft :: Hour -> Hour -> Bool
befORaft (H a1 b1) (H a2 b2) | a1 > a2 = False 
                             | a2 > a1 = True 
                             | a1 == a2 = if b2 > b1 then True else False

hourToMin :: Hour -> Int 
hourToMin (H a b) = a * 60 + b 

minToHour :: Int -> Hour
minToHour a = H (div a 60) (mod a 60)

differenceHours :: Hour -> Hour -> Int
differenceHours (H a1 b1) (H a2 b2) = if befORaft (H a1 b1) (H a2 b2) == True then hourToMin (H (a2-a1) (b2-b1))
                                                                              else hourToMin (H (a1-a2) (b1-b2))

addMin :: Hour -> Int -> Hour
addMin (H a b) c = minToHour (hourToMin (H a b) + c)



-- Exercicio 5 --

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next s | s == Verde = Amarelo
       | s == Amarelo = Vermelho
       | otherwise = Verde

stop :: Semaforo -> Bool
stop s = if s /= Vermelho then False else True

safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 | s1 == Verde && s2 == Verde = True
           | s1 /= Verde && s1 == s2 = False
           | otherwise = False 



-- Exercicio 6 --

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = abs x
posx (Polar x y) = abs((cos y) * x)

posy :: Ponto -> Double
posy (Cartesiano x y) = abs y
posy (Polar x y) = abs((sin y) * x)

raio :: Ponto -> Double 
raio (Cartesiano x y) = sqrt((0-x)^2 + (0-y)^2)
raio (Polar a b) = a 

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar a b) = b

dist6 :: Ponto -> Ponto -> Double
dist6 (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x2-x1)^2 + (y2-y1)^2)



-- Exercicio 7 --

data Figura = Circulo Ponto Double 
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo _ r) = if r > 0 then True else False 
poligono (Retangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = ((x1 /= x2) && (y1 /= y2))
poligono (Triangulo (Cartesiano x1 y1) (Cartesiano x2 y2) (Cartesiano x3 y3)) = ((x1 /= x2) && (x2 /= x3) && (x1 /= x3) && (y1 /= y2) && (y2 /= y3) && (y1 /= y3))

vertices :: Figura -> [Ponto]
vertices (Retangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = [(Cartesiano x1 y1), (Cartesiano x2 y2),
                                                              (Cartesiano x2 y1), (Cartesiano x1 y2)]
vertices (Triangulo (Cartesiano x1 y1) (Cartesiano x2 y2) (Cartesiano x3 y3)) = [(Cartesiano x1 y1), (Cartesiano x2 y2), (Cartesiano x3 y3)]
vertices (Circulo _ r) = error "Não foram encontrados vértices!!!"

area :: Figura -> Double
area (Triangulo p1 p2 p3) = let a = dist' p1 p2
                                b = dist' p2 p3
                                c = dist' p3 p1
                                s = (a+b+c) / 2 -- semi-perimetro
                            in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Circulo _ r) = pi * r^2
area (Retangulo a1 a2) = (dist' a1 (head (reverse (vertices (Retangulo a1 a2))))) * (dist' a2 (head (reverse (vertices (Retangulo a1 a2)))))

dist' :: Ponto -> Ponto -> Double
dist' (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x2-x1)^2+(y2-y1)^2)


perimetro' :: Figura -> Double
perimetro' (Circulo _ r) = 2 * pi * r
perimetro' (Triangulo p1 p2 p3) = dist' p1 p2 + dist' p2 p3 + dist' p1 p3
perimetro' (Retangulo a1 a2) = ((dist' a1 (head (reverse (vertices (Retangulo a1 a2)))))*2) + ((dist' a2 (head (reverse (vertices (Retangulo a1 a2)))))*2)



-- Exercicio 8 --

myisLower :: Char -> Bool
myisLower c | (ord c) < 97 = False
            | (ord c) > 122 = False 
            | otherwise = True

myisDigit :: Char -> Bool
myisDigit c | (ord c) < 48 = False
            | (ord c) > 57 = False 
            | otherwise = True

myisAlpha :: Char -> Bool
myisAlpha c | (ord c) < 65 || (ord c) > 90 && (ord c) < 97 || (ord c) > 122 = False 
            | otherwise = True

mytoUpper :: Char -> Char
mytoUpper c = chr ((ord c) - 32)

myintToDigit :: Int -> Char
myintToDigit x = chr (x + 48)

mydigitToInt :: Char -> Int
mydigitToInt c = (ord c) - 48










