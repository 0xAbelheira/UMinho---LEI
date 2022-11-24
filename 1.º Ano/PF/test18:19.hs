import Data.Char

-- Exercicio 1 --

-- a) --
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices x l = elemIndicesAux x l 0

elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux _ [] _ = []
elemIndicesAux a (x:xs) i
       | a == x = i : elemIndicesAux a xs (i+1)
       | otherwise = elemIndicesAux a xs (i+1)

-- b) --
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False 
isSubsequenceOf (x:xs) (y:ys) = x == y && isSubsequenceOf xs ys || isSubsequenceOf (x:xs) ys


-- Exercicio 2 --

data BTree a = Empty | Node a (BTree a) (BTree a)

-- a) --
lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP x (Node (a,b) e d) | x == a = Just b
                            | x < a = lookupAP x e
                            | otherwise = lookupAP x d 


-- b) --
zipWithBT' :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT' _ Empty _ = Empty
zipWithBT' _ _ Empty = Empty
zipWithBT' f (Node r1 e1 d1) (Node r2 e2 d2) = Node (f r1 r2) (zipWithBT' f e1 e2) (zipWithBT' f d1 d2)



-- Exercicio 3 --
digitAlpha :: String -> (String,String)
digitAlpha = foldr (\x (ds,as) -> if isDigit x then (x:ds,as) else if isAlpha x then (ds,x:as) else (ds,as)) ([],[])



-- Exercicio 4 --

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

-- a) --
firstSeq :: Seq a -> a
firstSeq (Cons a s) = a 
firstSeq (App Nil s) = firstSeq s
firstSeq (App s _) = firstSeq s 

-- b) --
dropSeq :: Int -> Seq a -> Seq a
dropSeq _ Nil = Nil 
dropSeq n (Cons a s) = dropSeq (n-1) s 
dropSeq n (App s1 s2) | n > nx = dropSeq (n - nx) s2
                      | n == nx = s2
                      | otherwise = (App (dropSeq n s1) s2)
    where nx = contaCons s1

contaCons :: Seq a -> Int
contaCons Nil = 0
contaCons (Cons _ s) = 1 + contaCons s
contaCons (App s1 s2) = contaCons s1 + contaCons s2

-- c) --
instance Show a => Show (Seq a) where
     show x = "<<" ++ mostra x ++ ">>"

mostra :: Show a => Seq a -> String 
mostra Nil = ""
mostra (Cons a Nil) = show a
mostra (Cons a s) = show a ++ "," ++ mostra s
mostra (App s1 s2) = mostra s1 ++ "," ++ mostra s2 




























