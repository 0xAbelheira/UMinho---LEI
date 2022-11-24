module Ficha6 where 

data BTree a = Empty | Node a (BTree a) (BTree a)
 deriving Show

arv1, arv2, arv3 :: BTree Int
arv1 = Node 4 Empty (Node 5 Empty Empty)
arv2 = Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)
arv3 = Node 7 arv1 arv2 
arvP = Node 10 (Node 5 (Node 2 Empty Empty) (Node 7 Empty Empty)) (Node 15 Empty (Node 19 (Node 16 Empty Empty) Empty))
-- Exercicio 1 --

altura :: BTree a -> Int
altura Empty = 0
altura (Node r e d) = 1 + (max (altura e) (altura d))

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node r e d) = 1 + contaNodos e + contaNodos d

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node r Empty Empty) = 1 
folhas (Node r e d) = folhas e + folhas d 

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty 
prune x (Node r e d) | x > 0 = Node r (prune (x-1) e) (prune (x-1) d)

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node r _ _) = [r]
path (False:bs) (Node r e d) = r : path bs e
path (True:bs) (Node r e d) = r : path bs d 

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c 
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = Node (f r1 r2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT f _ _ = Empty 

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (r1,r2,r3) e d) = (Node r1 e1 d1, Node r2 e2 d2, Node r3 e3 d3)
   where (e1,e2,e3) = unzipBT e 
         (d1,d2,d3) = unzipBT d 


-- Exercicio 2 --

minimo :: Ord a => BTree a -> a
minimo (Node r Empty d) = r 
minimo (Node r e d) = minimo e  

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node r Empty d) = d
semMinimo (Node r e d) = Node r (semMinimo e) d 

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node r Empty d) = (r,d)
minSmin (Node r e d) = let (m,e') = minSmin e
                       in (m, Node r e' d)

remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty 
remove x (Node r e d) 
      | x < r = Node r (remove x e) d 
      | x > r = Node r e (remove x d)
      | x == r = case d of
                   Empty -> e
                   _     -> let (m,d') = minSmin d
                            in Node m e d'

-- Exercicio 3 --

























