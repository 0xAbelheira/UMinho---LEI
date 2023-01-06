-- Exercicio 1 --

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt
      deriving Show

calcula :: ExpInt -> Int
calcula (Const a) = a
calcula (Simetrico e) = -(calcula e)
calcula (Mais e1 e2) = (calcula e1) + (calcula e2) 
calcula (Menos e1 e2) = (calcula e1) - (calcula e2)
calcula (Mult e1 e2) = (calcula e1) * (calcula e2)

infixa :: ExpInt -> String
infixa (Const a) = show a
infixa (Simetrico e) = "(" ++ "-" ++ (infixa e) ++ ")"
infixa (Mais e1 e2) = "(" ++ (infixa e1) ++ "+" ++ (infixa e2) ++ ")"
infixa (Menos e1 e2) = "(" ++ (infixa e1) ++ "-" ++ (infixa e2) ++ ")"
infixa (Mult e1 e2) = "(" ++ (infixa e1) ++ "*" ++ (infixa e2) ++ ")"

posfixa :: ExpInt -> String
posfixa (Const a) = show a
posfixa (Simetrico e) = (posfixa e) ++ " " ++ "-"
posfixa (Mais e1 e2) = (posfixa e1) ++ " " ++ (posfixa e2) ++ " " ++ "+"
posfixa (Menos e1 e2) = (posfixa e1) ++ " " ++ (posfixa e2) ++ " " ++ "-"
posfixa (Mult e1 e2) = (posfixa e1) ++ " " ++ (posfixa e2) ++ " " ++ "*"

-- Exercicio 2 -- 

data RTree a = R a [RTree a]

arv1 = (R 3 [R 2 [], R 4 [R 27 []]])

soma :: Num a => RTree a -> a 
soma (R x l) = x + sum (map soma l)

altura :: RTree a -> Int
altura (R x []) = 1
altura (R x l) = 1 + (maximum (map altura l))

prune :: Int -> RTree a -> RTree a
prune 0 (R x l) = R x [] 
prune a (R x l) = R x (map (prune (a-1)) l)






















