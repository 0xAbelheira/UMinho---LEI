import Data.Char

-- Exercicio 1 --

-- a) --
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' l [] = []
intersect' (x:xs) l | elem x l = x : intersect' xs l
                    | otherwise = intersect' xs l
--ou--
--intersect :: Eq a => [a] -> [a] -> [a]
--intersect l l2 = foldr (\x acc -> if x `elem` l2 then x:acc else acc) [] l

-- b) --
tails :: [a] -> [[a]]
tails [] = [[]]
tails l = [l] ++ tails (tail l)


-- Exercicio 2 --

type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

-- a) --
elems :: ConjInt -> [Int]
elems [] = []
elems ((x1,x2):xs) | x1 == x2 = x1 : elems xs 
                   | otherwise = x1 : elems ((succ x1,x2) : xs)


-- b) --
geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj (x:xs) = (x,d) : geraconj (dropWhile (<= d) xs)
 where d = foldl (\acc x -> if x == succ acc then x else acc) x xs


-- Exercicio 3 --

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
  deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]

agenda1 = [("Sofia", [Casa 123456789, Tlm 987654321, Email "abc@def.ghi", Email "f@mendess.xyz"]),("Luís", [Tlm 69420]),("Rita", [Trab 58008])]

-- a) -- 
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome, [Email email])]
acrescEmail nome email agenda@((nomeX, contactos ) : t)
  | nome == nomeX = (nome, (Email email) : contactos) : t
  | otherwise = head agenda : acrescEmail nome email t 

-- b) --
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome agenda@((nomeX, contactos) : t)
  | nome == nomeX = foldr (\x acc -> case x of Email email -> if acc == Nothing then Just [email] else ((:) email) <$> acc; otherwise -> acc) Nothing contactos
  | otherwise = verEmails nome t 

-- c) --
consulta :: [Contacto] -> ([Integer],[String])
consulta = foldr (\x (i,s) -> case x of Email email -> (i, email:s); otherwise -> (n x: i,s)) ([],[])
  where n x = case x of Casa num -> num
                        Trab num -> num
                        Tlm num -> num

-- Exercicio 4 --

data RTree a = R a [RTree a] deriving (Show, Eq)

-- a) --
paths :: RTree a -> [[a]]
paths (R node []) = [[node]]
paths (R node ramos) = [node : x | x <- concat [paths ramo | ramo <- ramos]]




























































