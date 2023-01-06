{- |

= Introdução

Nesta tarefa 6 desenvolvemos as táticas e as jogadas que o bot (pacman) deve tomar de modo a agir corretamente no jogo.

= Desenvolvimento

Na realização desta tarefa criamos funções que permitem ao pacman jogar de acordo com o estado de jogo atual.

Para implementar estas reações por parte do bot, criamos funções que permitem: ver se ha algum fantasma perto do pacman num determinado raio e identifiçá-lo; ver quais as condições a que o bot está sujeito, nomeadamente (e por ordem de prioridade), se tem algum fantasma vivo a persegui-lo num raio de 5 unidades; se tem algum fantasma morto que deve perseguir; se tem uma comida grande à sua volta; se tem uma comida pequena à sua volta; calcular a melhor orientação que o bot deve tomar (quer esteja a ser perseguido, a perseguir, ou simplesmente a comer comida para somar pontos)

= Conclusão

Ficamos mais satisfeitos com o funcionamento da movimentação bot do que com o funcionamento da movimentação dos fantasmas, visto que o bot recalcula para onde deve ir de acordo com as peças que tem à sua volta e num determinado raio.
No entanto, o bot apresenta alguns problemas evidentes, como por exemplo o facto de, por dar prioridade a fugir dos fantasmas, tomar uma orientação que o leva a tentar movimentar-se contra uma parede infinitamente.
-}

module Tarefa6 where 

import Types
import Tarefa2
import Tarefa5
import Data.List

{- | Condição que define a prioridade de jogada para comer uma comida pequena se não houver nenhuma comida grande à volta e o pacman não estiver a ser perseguido ou a perseguir algum fantasma -}
--
checkFoodLittle :: Maze -> Int -> [Player] -> Orientation -> Coords -> (Bool, Play)
checkFoodLittle maze id pls ort (x,y) 
    | getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) ort) == Just (Food Little) = (True, Move id ort)
    | getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) U) == Just (Food Little) = (True, Move id U)
    | getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) D) == Just (Food Little) = (True, Move id D)
    | getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) R) == Just (Food Little) = (True, Move id R)
    | getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) L) == Just (Food Little) = (True, Move id L)
    | otherwise = (False, Move id ort)

-- | Condição que define a prioridade de jogada para comer uma comida grande se se o pacman não estiver a ser perseguido ou estiver a perseguir alguém
--
checkFoodBig :: Maze -> Int -> [Player] -> Orientation -> Coords -> (Bool, Play)
checkFoodBig maze id pls ort (x,y) 
    | getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) ort) == Just (Food Big) = (True, Move id ort)
    | getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) U) == Just (Food Big) = (True, Move id U)
    | getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) D) == Just (Food Big) = (True, Move id D)
    | getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) R) == Just (Food Big) = (True, Move id R)
    | getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) L) == Just (Food Big) = (True, Move id L)
    | otherwise = (False, Move id ort)

-- | Vai buscar as coordenadas do fantasma mais próximo ao pacman de uma lista de jogadores
--
getNearestGhostCoords :: [Player] -> Coords
getNearestGhostCoords (j:js) = head (sortOn (distance (getPlayerCoords (getPacman (j:js)))) (map (getPlayerCoords) (getGhosts (j:js))))

-- | Verifica se existe algum fantasma num determinado raio
--
checkGhostClose :: [Player] -> Bool
checkGhostClose pls
    | distance (getPlayerCoords (getPacman pls)) (getNearestGhostCoords pls) <= 5 = True
    | otherwise = False

-- | Vai buscar o fantasma com as coordenadas do fantasma mais próximo ao pacman
--
getNearestGhostFromCoords :: [Player] -> Coords -> Player
getNearestGhostFromCoords (j:js) (x,y)
    | (not (checkPacman j)) && (getPlayerCoords j) == (x,y) = j
    | otherwise = getNearestGhostFromCoords js (x,y)
    
-- | Vai buscar o fantasma mais próximo do pacman a partir de uma lista de jogadores
--
getNearestGhost :: [Player] -> Player
getNearestGhost pls = getNearestGhostFromCoords pls (getNearestGhostCoords pls)

-- | Devolve a melhor orientação de modo a que a distância do pacman ao fantasma mais próximo seja maior
--
getEscapeOrt :: [Player] -> Orientation
getEscapeOrt pls = snd (head (sortOn (fst) (getBestOrt (getNearestGhostCoords pls) (getPlayerCoords (getPacman pls)))))

-- | Devolve a melhor orientação de modo a que a distância do pacman ao fantasma mais próximo seja menor
--
getChaseOrt :: [Player] -> Orientation
getChaseOrt pls = snd (head (reverse (sortOn (fst) (getBestOrt (getNearestGhostCoords pls) (getPlayerCoords (getPacman pls))))))

-- | Permite que o pacman jogue sozinho atribuíndo-lhe jogadas que obedecem a determinadas condições
--
bot :: Int -> State -> Maybe Play
bot id s@(State maze pls lvl)
    | (getPacmanMode (getPacman pls) == Normal) && (checkGhostClose pls) && ((getGhostMode (getNearestGhost pls)) == Alive) = Just (Move id (getEscapeOrt pls))
    | (getPacmanMode (getPacman pls) == Mega) && (checkGhostClose pls) && ((getGhostMode (getNearestGhost pls)) == Alive) = Just (Move id (getEscapeOrt pls)) 
    | (getPacmanMode (getPacman pls) == Mega) && ((getGhostMode (getNearestGhost pls)) == Dead) = Just (Move id (getChaseOrt pls))
    | fst (checkFoodBig maze id pls (getPlayerOrientation (getPacman pls)) (getPlayerCoords (getPacman pls))) = Just (snd (checkFoodBig maze id pls (getPlayerOrientation (getPacman pls)) (getPlayerCoords (getPacman pls))))
    | fst (checkFoodLittle maze id pls (getPlayerOrientation (getPacman pls)) (getPlayerCoords (getPacman pls))) = Just (snd (checkFoodLittle maze id pls (getPlayerOrientation (getPacman pls)) (getPlayerCoords (getPacman pls))))
    | otherwise = Just (Move id (getPlayerOrientation (getPacman pls)))
    where id = getPlayerID (getPacman pls)

{-
not (fst (getNewOrt maze id pls (getPlayerOrientation (getPacman pls)) (getPlayerCoords (getPacman pls)))) = Just (snd (getNewOrt maze id pls (getPlayerOrientation (getPacman pls)) (getPlayerCoords (getPacman pls))))
 
checkWallU :: Maze -> Int -> [Player] -> Orientation -> Coords -> Bool
checkWallU maze id pls U (x,y) 
    | (getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) U)) == Just Wall = True
    | otherwise = False

checkWallR :: Maze -> Int -> [Player] -> Orientation -> Coords -> Bool
checkWallR maze id pls R (x,y) 
    | (getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) R)) == Just Wall = True
    | otherwise = False

checkWallD :: Maze -> Int -> [Player] -> Orientation -> Coords -> Bool
checkWallD maze id pls D (x,y) 
    | (getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) D)) == Just Wall = True
    | otherwise = False

checkWallL :: Maze -> Int -> [Player] -> Orientation -> Coords -> Bool
checkWallL maze id pls L (x,y) 
    | (getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) L)) == Just Wall = True
    | otherwise = False


getNewOrt :: Maze -> Int -> [Player] -> Orientation -> Coords -> (Bool, Play)
getNewOrt maze id pls ort (x,y) 
    | not (checkWallU maze id pls U (x,y)) = (False, Move id U)
    | (checkWallU maze id pls U (x,y)) = ((checkWallD maze id pls D (x,y)), Move id D)
    | (checkWallD maze id pls D (x,y)) = ((checkWallR maze id pls L (x,y)), Move id L)
    | (checkWallR maze id pls R (x,y)) = ((checkWallR maze id pls R (x,y)), Move id R)
    | otherwise = (True, Move id ort) -}


