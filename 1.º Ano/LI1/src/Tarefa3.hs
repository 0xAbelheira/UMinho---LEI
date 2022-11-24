{- | Para a realização desta tarefa vamos primeiro fazer uma função que devolve os padrões horizontais, e a partir daí vamos construir os padrões verticais até termos a função compactMaze. -}

module Tarefa3 where

import Types
import Tarefa1

--exemplo: compactMaze (generateMaze 15 10 4)

-- | Divide o corredor numa lista de corredores cujos elementos correspondem aos elemtos iguais consecutivos do corredor original
--
splitCorridor :: Corridor -> [Corridor] -- length ((splitCorridor l) !! 0) dá o numero de vezes repetido .... (length (x:xs) - length (tail (x:xs)))
splitCorridor  [] = []
splitCorridor (x:xs) = (x:takeWhile(== x) xs):splitCorridor (dropWhile(==x) xs)

-- | Devolve a lista [(Int,Piece)] de uma determinada instrução
--
getIntPiece :: Instruction -> [(Int,Piece)]
getIntPiece (Instruct ((i,p):xs)) = (i,p):xs

-- | Devolve o padrão horizontal de um corredor em forma de instrução
--
horPattern :: Corridor -> Instruction
horPattern [] = Instruct []
horPattern [x] = Instruct [(1,x)]
horPattern (x:xs:[]) = if (x == xs) then Instruct [(2,x)] else Instruct [(1,x),(1,xs)]
horPattern (x:xs) 
    | (length (splitCorridor (x:xs))) > 1 = Instruct ((length ((splitCorridor (x:xs)) !! 0), x):getIntPiece (horPattern (drop (length ((splitCorridor (x:xs)) !! 0)) (x:xs))))
    | otherwise = Instruct [(length ((splitCorridor (x:xs)) !! 0), x)]

-- | Transforma um labirinto numa lista de instruções em que cada instrução corresponde ao padrão horizontal em instrução de cada corredor do labirinto 
--
horPatternMaze :: Maze -> Instructions
horPatternMaze [] = []
horPatternMaze (x:xs) = (horPattern x):horPatternMaze xs

-- | Devolve o índice da primeira ocorrência de um elemento numa determinada lista 
--
getIndex :: Eq a => a -> [a] -> Int
getIndex a [x] = 0
getIndex a (x:xs) 
    | a == x = 0
    | otherwise = 1+getIndex a xs

-- | Devolve a lista de índices correspondestes às ocorrências de um determinado elemento numa lista
--
getCorridorIndex :: Eq a => a -> [a] -> [Int]
getCorridorIndex _ [] = [] 
getCorridorIndex n l
        | n == last l = getCorridorIndex n (init l)++[(length l)-1]
        | otherwise = getCorridorIndex n (init l)

-- | Substitui numa determinada lista de instruções uma instrução na posição correspondente ao índice dado 
--
replaceInstructRepeatInt :: [Instruction] -> Int -> Instruction -> [Instruction]
replaceInstructRepeatInt xs i x = before ++ (x : after)
  where before = take i xs
        after = drop (i+1) xs

-- | Substitui numa determinada lista de instruções uma instrução nas posições correspondentes aos elementos da lista de índices dada
--
replaceInstructRepeatInts :: [Instruction] -> [Int] -> Instruction -> [Instruction]
replaceInstructRepeatInts xs [] x = xs 
replaceInstructRepeatInts xs (i:is) x = replaceInstructRepeatInts (replaceInstructRepeatInt xs i x) is x

-- | Função auxiliar da compactMaze que atualiza uma lista de instruções para uma lista de instruções com os respetivos "Repeat" necessários
--
compactMaze' :: Instructions -> Int -> Instructions
compactMaze' ((Repeat y):ys) i = (Repeat y):compactMaze' ys i
compactMaze' [] i = []
compactMaze' (x:xs) i
    | elem x xs = compactMaze' (replaceInstructRepeatInts (x:xs) (tail (getCorridorIndex x (x:xs))) (Repeat (getIndex x (x:xs)+i))) i
    | otherwise = x:compactMaze' xs (i+1)

-- | Recebe um labirinto e transforma-o numa lista de instruções válida
--
compactMaze :: Maze -> Instructions
compactMaze [] = []
compactMaze (x:xs) = compactMaze' (horPatternMaze (x:xs)) 0

-- | Dada uma instrução devolve-a em forma de corredor
--
unzipCorridor :: Instruction -> Corridor
unzipCorridor (Instruct []) = []
unzipCorridor (Instruct ((i,p):xs)) = (replicate i p) ++ unzipCorridor (Instruct xs)

-- | Dada uma lista de instruções, descompacta-a num labirinto constituídos pelas instruções do tipo Instruct [(Int,Piece)]
--
unzipMaze :: Instructions -> Maze
unzipMaze [] = []
unzipMaze (x:xs) = (unzipCorridor x):(unzipMaze xs) 

-- | Dada uma lista de instruções, devolve a mesma com os repeats substituídos pelos respetivos (Instruct [(Int,Piece)])
--
deleteRepeats :: Instructions -> Instructions -> Instructions
deleteRepeats [] (y:ys) = []
deleteRepeats (Repeat x:xs) (y:ys) =  ((y:ys) !! x):deleteRepeats xs (y:ys)
deleteRepeats (x:xs) (y:ys) = x:deleteRepeats xs (y:ys)

-- | Testa se as instruções descompactadas em forma de labirinto correspondem ao labirinto correto
--
testCompactMaze :: Instructions -> Maze -> Bool
testCompactMaze (x:xs) m = m == unzipMaze (deleteRepeats (x:xs) (x:xs)) 


