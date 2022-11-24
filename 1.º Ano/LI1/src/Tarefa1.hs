{- | Nesta tarefa vamos construindo e gerando labirintos cada vez mais completos até termos um labirinto válido.

Um labirinto válido segue as seguintes regras:
1. O labirinto é rodeado por peças Wall (exceto túnel);
2. A faixa mais central forma um túnel;
3. Todos os corredores têm o mesmo comprimento;
4. A “casa dos fantasmas” está localizada no centro do labirinto;
5. Deve ter as dimenões mínimas de 15x10.

Para isto, vamos definir várias funções que nos vão ajudar a construir esses labirintos. -}

module Tarefa1 where

import Types

--exemplo: placeGenerateMaze 15 10 4 ; generateMaze 50 50 5 ; generateMaze 15 10 4

-- | Gera um labirinto aleatório sem quaisquer restrições associadas
--
genMaze :: Int -> Int -> Int -> Maze
genMaze x y s =
                 let random_nrs = genRandom (x*y) s
                 in convertMaze $ subList x random_nrs

-- | Cria um Corridor composto por n Walls 
--
createWall :: Int -> Corridor
createWall 0 = []
createWall n = Wall : createWall (n-1)

-- | Gera um labirinto com os limites superiores e inferiores (corredores de paredes) com a ajuda das funções createWall e genMaze
-- 
genUpDownExtremes :: Int -> Int -> Int -> Maze
genUpDownExtremes x y s = (createWall x):(genMaze x (y-2) s) ++ [createWall x]

-- | Troca uma certa Piece por outra Piece num certo Corridor de acordo com o índice "n" dado
--
replaceInList :: Int -> Piece -> Corridor -> Corridor
replaceInList n p [] = [] 
replaceInList 0 p (x:xs) = p:xs
replaceInList n p (x:xs) = x:(replaceInList (n-1) p xs)

-- | Troca a primeira e a última Piece de uma certa lista por uma Wall com a ajuda da função replaceInList
--
replaceFirstLastWall :: Maze -> Maze 
replaceFirstLastWall [] = []
replaceFirstLastWall (x:xs) = replaceInList ((length x) - 1) Wall (replaceInList 0 Wall x) : replaceFirstLastWall xs

-- | Gera um labirinto com todos os limites/extremos (rodeado por Wall) aplicando a função replaceFirstLastWall ao labirinto gerado pela função genUpDownExtremes
--
genAllExtremes :: Int -> Int -> Int -> Maze
genAllExtremes x y s = replaceFirstLastWall (genUpDownExtremes x y s)


-- | Devolve os corredores do meio de um labirinto, tendo em conta que se o labirinto tiver número de corredores par então o meio vai corresponder a dois corredores e se tiver número de corredores ímpar vai corresponder a apenas um corredor.
-- 
getMiddle :: Maze -> Maze
getMiddle [] = []
getMiddle x
    | even (length x) = [x !! ((div (length x) 2)-1), x !! (div (length x) 2)]
    | otherwise = [x !!  (div (length x) 2)]
    
-- | Devolve os pares de corredores que não são os que corredores do meio (corredores antes do meio, corredores depois do meio)
--
getNotMiddle :: Maze -> (Maze, Maze) 
getNotMiddle [] = ([], [])
getNotMiddle x
    | even (length x) = (take (((length x) `div` 2)-1) x, drop (((length x) `div` 2)+1) x)
    | otherwise = (take ((length x) `div` 2) x, drop (((length x) `div` 2)+1) x)

-- | Substitui as primeiras e últimas peças dos corredores (ou corredor) do meio
--
replaceFirstLastEmpty :: Maze -> Maze
replaceFirstLastEmpty x
    | even (length x) = replaceInList ((length ((getMiddle x) !! 0)) - 1) Empty (replaceInList 0 Empty ((getMiddle x) !! 0)): [replaceInList ((length ((getMiddle x) !! 1)) - 1) Empty (replaceInList 0 Empty ((getMiddle x) !! 1))]
    | otherwise = [replaceInList ((length ((getMiddle x) !! 0)) - 1) Empty (replaceInList 0 Empty ((getMiddle x) !! 0))]
      

-- | Gera um labirinto com os limites todos e os túneis incluídos a partir das funções getNotMiddle, replaceFirstLastEmpty e genAllExtremes e através da tática de junção mencionada acima
--
genTunnel :: Int -> Int -> Int -> Maze
genTunnel x y s = (fst (getNotMiddle (genAllExtremes x y s))) ++ (replaceFirstLastEmpty (genAllExtremes x y s)) ++ (snd (getNotMiddle (genAllExtremes x y s)))


-- | Cria a casa dos fantasmas de acordo com se o comprimento do labirinto é par ou ímpar (se for par a casa tem comprimento 8, se for ímpar tem comprimento 9)
--
createGhostHouse :: Int -> Maze -- ^ (adicionamos Empty antes e depois de cada corredor da casa dos fantasmas, visto que posteriormente a casa dos fantasmas tem de estar rodeada por Empty)
createGhostHouse x 
    | even x = [
            [Empty, Wall, Wall, Wall, Empty, Empty, Wall, Wall, Wall, Empty],
            [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty],
            [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty]
                ]

    | otherwise = [
            [Empty, Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall, Empty],
            [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty],
            [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty]
                  ]
-- | Coloca um corredor dentro de outro corredor a partir do índice n que recebe como argumento
--
replaceCorridor :: Int -> Corridor -> Corridor -> Corridor
replaceCorridor n [] c2 = c2
replaceCorridor 0 (x:xs) (y:ys) = x:replaceCorridor 0 xs ys
replaceCorridor n (x:xs) (y:ys) = y:replaceCorridor (n-1) (x:xs) ys

-- | Devolve os corredores nos quais não vai ser substituído a casa fantasmas (corredores antes da casa fantasma, corredores depois da casa fantasma)
--
getNotGhostHouse :: Maze -> (Maze, Maze) 
getNotGhostHouse [] = ([], [])
getNotGhostHouse x
    | even (length x) = (take (((length x) `div` 2)-2) x, drop (((length x) `div` 2)+1) x)
    | otherwise = (take (((length x) `div` 2)-1) x, drop (((length x) `div` 2)+2) x)   

-- | Devolve os corredores do meio que vão ser substítuidos pelos corredores com a casa fantasmas
--
getMiddleGhostHouse :: Maze -> Maze
getMiddleGhostHouse [] = []
getMiddleGhostHouse x
    | even (length x) = [x !! ((div (length x) 2)-2),x !! ((div (length x) 2)-1), x !! (div (length x) 2)]
    | otherwise = [x !! ((div (length x) 2)-1),x !! (div (length x) 2),x !! ((div (length x) 2)+1)]

-- | Gera um labirinto com todos os limites, com os túneis, e com a casa fantasmas colocada no centro (falta rodear a casa por Empty)
--
genGhostHouse :: Int -> Int -> Int -> Maze
genGhostHouse x y s =  (fst (getNotGhostHouse (genTunnel x y s)))
                    ++ [(replaceCorridor n ((createGhostHouse x) !! 0) ((getMiddleGhostHouse (genTunnel x y s)) !! 0))]
                    ++ [(replaceCorridor n ((createGhostHouse x) !! 1) ((getMiddleGhostHouse (genTunnel x y s)) !! 1))]
                    ++ [(replaceCorridor n ((createGhostHouse x) !! 2) ((getMiddleGhostHouse (genTunnel x y s)) !! 2))]
                    ++ (snd (getNotGhostHouse (genTunnel x y s)))
            where n = (div x 2)-5 


-- | Devolve os corredores nos quais não vai ser substituído a casa fantasmas e os Empty que a rodeiam
--
getNotGhostHouseEmptys :: Maze -> (Maze, Maze) 
getNotGhostHouseEmptys [] = ([], [])
getNotGhostHouseEmptys x
    | even (length x) = (take (((length x) `div` 2)-3) x, drop (((length x) `div` 2)+2) x)
    | otherwise = (take (((length x) `div` 2)-2) x, drop (((length x) `div` 2)+3) x)  

-- | Devolve os corredores cujos primeiro e último têm de ser substituídos por Empty à volta da casa fantasmas
--
getMiddleGhostHouseEmptys :: Maze -> Maze
getMiddleGhostHouseEmptys [] = []
getMiddleGhostHouseEmptys x
    | even (length x) = [x !! ((div (length x) 2)-3),x !! ((div (length x) 2)-2),x !! ((div (length x) 2)-1), x !! (div (length x) 2),x !! ((div (length x) 2)+1)]
    | otherwise = [x !! ((div (length x) 2)-2),x !! ((div (length x) 2)-1),x !! (div (length x) 2),x !! ((div (length x) 2)+1),x !! ((div (length x) 2)+2)]
 
-- | Cria um corredor de vazio (Empty) para colocar em cima e em baixo da casa fantasmas de acordo com se o comprimento do labirinto é par ou ímpar
--
createEmptys :: Int -> Corridor
createEmptys x 
    | even x = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
    | otherwise = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]

-- | Gera um labirinto válido
--
generateMaze :: Int -> Int -> Int -> Maze
generateMaze x y s = if x>=15 && y>=10 then (fst (getNotGhostHouseEmptys (genTunnel x y s)))
                    ++ [(replaceCorridor n (createEmptys x) ((getMiddleGhostHouseEmptys (genTunnel x y s)) !! 0))]
                    ++ [(replaceCorridor n ((createGhostHouse x) !! 0) ((getMiddleGhostHouse (genTunnel x y s)) !! 0))]
                    ++ [(replaceCorridor n ((createGhostHouse x) !! 1) ((getMiddleGhostHouse (genTunnel x y s)) !! 1))]
                    ++ [(replaceCorridor n ((createGhostHouse x) !! 2) ((getMiddleGhostHouse (genTunnel x y s)) !! 2))]
                    ++ [(replaceCorridor n (createEmptys x) ((getMiddleGhostHouseEmptys (genTunnel x y s)) !! 4))] 
                    ++ (snd (getNotGhostHouseEmptys (genTunnel x y s)))
                    else []
            where n = (div x 2)-5 


-- | Gera um labirinto válido em forma de string
--
placeGenerateMaze :: Int -> Int -> Int -> IO()
placeGenerateMaze x y s = placeMaze (generateMaze x y s)

    

-- | Testa se um labirinto tem a altura que foi dada
--
testMazeHeight :: Int -> Int -> Int -> Bool
testMazeHeight x y s 
    | (length (generateMaze x y s)) == y = True
    | otherwise = False

-- | Testa se todos os corredores do labirinto têm o comprimento dado
--
testCorridorsLength :: Maze -> Int -> Bool
testCorridorsLength [] x = True
testCorridorsLength (c:cs) x = (length c) == x && testCorridorsLength cs x

-- | Testa se um corredor tem como primeiro e último elemento uma parede
--
testCorridorWall :: Corridor -> Bool
testCorridorWall (x:xs) = x == Wall && (last (x:xs)) == Wall

-- | Testa se todos os corredores de um labirinto têm como primeiro e último elemento uma parede
--
testReplaceFirstLastWall :: Maze -> Bool
testReplaceFirstLastWall [] = True
testReplaceFirstLastWall (c:cs) = testCorridorWall c && testReplaceFirstLastWall cs

-- | Testa se um corredor tem como primeiro e último elemento um vazio 
--
testCorridorEmpty :: Corridor -> Bool
testCorridorEmpty (x:xs) = x == Empty && (last (x:xs)) == Empty

-- | Testa a colocação do(s) túnel/túneis conforme os labirintos são pares ou ímpares
--
testTunnelPlacement :: Maze -> Bool
testTunnelPlacement m
    | even (length m) = (testCorridorEmpty (m !! (div (length m) 2))) && (testCorridorEmpty (m !! ((div (length m) 2)-1)))
    | otherwise = testCorridorEmpty (m !! (div (length m) 2))
