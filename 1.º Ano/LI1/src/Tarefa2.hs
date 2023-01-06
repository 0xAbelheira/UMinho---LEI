{- | Nesta tarefa, de modo a definir a função play, vamos escrever funções auxiliares que ajudem no cumprimento das jogadas, quer estas sejam simples mudanças de orientação, ou mudanças na posição do jogador, obedecendo às respetivas condições e restrições associadas. -}

module Tarefa2 where

import Types
import Data.List 
import Tarefa1

-- exemplo: play (Move 1 R) (State (generateMaze 15 10 0) [(Pacman (PacState (1, (7,8), 1, U, 0, 1) 0 Open Normal)), (Ghost (GhoState (0, (5,12), 1, U, 0, 1) Alive))] 1)

-- | Dado um ID e uma lista de jogadores, devolve o jogador com esse ID único
--
getPlayerFromID :: Int -> [Player] -> Player
getPlayerFromID id (j:js) 
    | id == (getPlayerID j) = j
    | otherwise = getPlayerFromID id js

-- | Dado um ID e uma lista de jogadores, devolve a lista de jogadores sem o jogador com esse ID único
--
getPlayerFromNotID :: Int -> [Player] -> [Player]
getPlayerFromNotID id (j:js) 
    | id == (getPlayerID j) = js
    | otherwise = j: getPlayerFromNotID id js

-- | Dado um jogador, devolve o jogador com a orientação alterada para a que foi dada
--
changePlayerOrientation :: Orientation -> Player -> Player
changePlayerOrientation ort2 (Pacman (PacState (id , (x,y), v, ort1, p, l) t o m )) = (Pacman (PacState (id , (x,y), v, ort2, p, l) t o m ))
changePlayerOrientation ort2 (Ghost (GhoState (id , (x,y), v, ort1, p, l) m )) = (Ghost (GhoState (id , (x,y), v, ort2, p, l) m ))

-- | Dada uma lista de jogadores, devolve o jogador escolhido a partir do seu ID único com a sua orientação alterada para a que foi dada
--
changePlayerOrientationFromID :: Orientation -> Int -> [Player] -> Player
changePlayerOrientationFromID ort2 id (j:js)
    | id == getPlayerID j = changePlayerOrientation ort2 j
    | otherwise = changePlayerOrientationFromID ort2 id js 

-- | Dadas umas coordenadas e uma orientação, devolve as coordenadas atualizadas da possível próxima posição
--
nextPosition :: Coords -> Orientation -> Coords
nextPosition (x,y) ort = case ort of L -> (x,y-1)
                                     R -> (x,y+1)                                    
                                     U -> (x-1,y)
                                     D -> (x+1,y)
                                     Null -> (x,y)

-- | Dado um labirinto e umas coordenadas, calcula a peça que aí se encontra (podendo não ser dadas coordenadas válidas e, nesse caso, não devolve nenhuma peça)
--
getPiece :: Maze -> Coords -> Maybe Piece 
getPiece maze (x,y) 
  | y<0 || y>=(length (maze !! 0)) = Nothing
  | otherwise = Just ((maze !! x) !! y)
  
-- | Dadas umas coordenadas e um jogador, devolve o jogador com as suas coordenadas alteradas para as que foram dadas
--
changePlayerCoords :: Coords -> Player -> Player 
changePlayerCoords (x,y) (Pacman (PacState (id, (x',y'), v, ort, p, l) t o m)) = (Pacman (PacState (id, (x,y), v, ort, p, l) t o m))
changePlayerCoords (x,y) (Ghost (GhoState (id, (x',y'), v, ort, p, l) m)) = (Ghost (GhoState (id, (x,y), v, ort, p, l) m))

-- | Dado um jogador, atualiza as suas coordenadas para para as coordenadas da posição seguinte, de acordo com a orientação atual que tem
--
updatePlayerCoords :: Player -> Player
updatePlayerCoords (Pacman (PacState (id, (x,y), v, ort, p, l) t o m)) = (Pacman (PacState (id, (nextPosition (x,y) ort), v, ort, p, l) t o m)) 
updatePlayerCoords (Ghost (GhoState (id, (x,y), v, ort, p, l) m)) = (Ghost (GhoState (id, (nextPosition (x,y) ort), v, ort, p, l) m))

-- | Atualiza os pontos de um jogador de acordo com a peça dada
--
updatePlayerPoints :: Player -> Piece -> Player
updatePlayerPoints (Pacman (PacState (id , (x,y), v, ort, p, l) t o m )) (Food Little) = (Pacman (PacState (id , (x,y), v, ort, p+1, l) t o m ))
updatePlayerPoints (Pacman (PacState (id , (x,y), v, ort, p, l) t o m )) (Food Big) = (Pacman (PacState (id , (x,y), v, ort, p+5, l) t o m ))
updatePlayerPoints (Pacman (PacState (id , (x,y), v, ort, p, l) t o m )) (PacPlayer (Ghost  (GhoState a Dead ))) = (Pacman (PacState (id , (x,y), v, ort, p+10, l) t o m ))
updatePlayerPoints x p = x

-- | Atualiza as vidas de um jogador, tirando uma vida ao seu número de vidas atuais
--
updatePlayerLives :: Player -> Player
updatePlayerLives (Pacman (PacState (x,y,z,t,h,l) q c d )) = (Pacman (PacState (x,y,z,t,h,(l-1)) q c d ))
updatePlayerLives x = x

-- Dado um jogador pacman e um modo (PacMode), altera o modo desse jogador pacman para o modo dado
--
changePacMode :: Player -> PacMode -> Player
changePacMode (Pacman (PacState (id , (x,y), v, ort, p, l) t o m )) m' = (Pacman (PacState (id , (x,y), v, ort, p, l) t o m'))
changePacMode x m = x

-- | Dado um  jogador fantasma e um modo (GhostMode), altera o modo desse jogador fantasma para o modo dado
--
changeGhostMode :: Player -> GhostMode -> Player
changeGhostMode (Ghost (GhoState (id , (x,y), v, ort, p, l) m )) m' = (Ghost (GhoState (id , (x,y), v, ort, p, l) m'))

-- | Dada uma lista de jogadores, altera o modo de todos os fantasmas vivos para morto (Dead)
--
changeGhostsForDead :: [Player] -> [Player]
changeGhostsForDead [] = []
changeGhostsForDead ((Ghost (GhoState (id, (x,y), v, ort, p, l) Alive)):js) = (Ghost (GhoState (id, (x,y), v/2 , ort, p, l) Dead)):changeGhostsForDead js
changeGhostsForDead ((Ghost (GhoState (id, (x,y), v , ort, p, l) Dead)):js) = (Ghost (GhoState (id, (x,y), v , ort, p, l) Dead):changeGhostsForDead js)
changeGhostsForDead ((Pacman (PacState (id, (x,y), v, ort, p, l) t o m)):js) = ((Pacman (PacState (id, (x,y), v, ort, p, l) t o m)):changeGhostsForDead js) 

-- | Dada uma lista de jogadores, altera o modo de todos os fantasmas mortos para vivo (Alive)
--
changeGhostsForAlive :: [Player] -> [Player]
changeGhostsForAlive [] = []
changeGhostsForAlive ((Ghost (GhoState (id, (x,y), v, ort, p, l) Alive)):js) = (Ghost (GhoState (id, (x,y), v , ort, p, l) Alive)):changeGhostsForAlive js
changeGhostsForAlive ((Ghost (GhoState (id, (x,y), v , ort, p, l) Dead)):js) = (Ghost (GhoState (id, (x,y), 2*v , ort, p, l) Alive)):changeGhostsForAlive js
changeGhostsForAlive ((Pacman (PacState (id, (x,y), v, ort, p, l) t o m)):js) = (Pacman (PacState (id, (x,y), v, ort, p, l) t o m)):changeGhostsForAlive js 

-- | Dado um jogador fantasma morto, altera o seu modo para vivo
--
changeGhostForAlive :: Player -> Player
changeGhostForAlive ((Ghost (GhoState (id, (x,y), v, ort, p, l) Dead))) = ((Ghost (GhoState (id, (x,y), 2*v, ort, p, l) Alive)))
changeGhostForAlive x = x

-- | Dado um labirinto devolve as coordenadas centrais onde os fantasmas devem renascer, conforme o labirinto é par ou ímpar
--
getMiddleCoords :: Maze -> Coords
getMiddleCoords m 
  | even (length m) = (((div (length m) 2)-1), (div (length (m !! 0)) 2))
  | otherwise = ((div (length m) 2), (div (length (m !! 0)) 2))

-- | Dada uma lista de jogadores, devolve a lista dos jogadores dessa lista que são fantasmas
--
getGhosts :: [Player] -> [Player]
getGhosts [] = []
getGhosts (j:js) = case j of
  (Ghost (GhoState _ _)) -> j:getGhosts js
  otherwise -> getGhosts js

-- | Dada uma lista de jogadores e um ID, verifica se o jogador da lista com esse ID único tem as mesmas coordenadas de outro jogador
--
checkCoordsPlayerGhosts :: [Player] -> Int -> [Player] -> Bool
checkCoordsPlayerGhosts (j:js) id [] = False
checkCoordsPlayerGhosts (j:js) id (g:gs) 
  | getPlayerCoords (getPlayerFromID id (j:js)) == getPlayerCoords g = True
  | otherwise = checkCoordsPlayerGhosts (j:js) id gs 

-- | Devolve o jogador fantasma com as mesmas coordenadas do jogador cujo ID foi dado, de uma determinada lista de jogadores
--
getGhostSameCoords :: [Player] -> Int -> [Player] -> Player
getGhostSameCoords js id (g:gs)
  | getPlayerCoords (getPlayerFromID id js) == getPlayerCoords g = g
  | otherwise = getGhostSameCoords js id gs

-- | Devolve a lista de jogadores sem um determinado jogador dessa lista
--
deleteGhost :: [Player] -> Player -> [Player]
deleteGhost [] g = []
deleteGhost (j:js) g 
  | getPlayerID j == getPlayerID g = js
  | otherwise = j: (deleteGhost js g)

-- |
--
setTimeMega :: Player -> Player
setTimeMega (Pacman (PacState (id, (x,y), v, ort, p, l) t o m)) = (Pacman (PacState (id, (x,y), v, ort, p, l) 10000 o m))
setTimeMega x = x

-- | Altera o estado do pacman para normal a partir de um state
--
changePacModeFromState :: Int -> State -> State
changePacModeFromState id s@(State maze js lvl)
  | (checkPacman (getPlayerFromID id js)) && ((getPacmanMode (getPlayerFromID id js)) == Dying) = s
  | (checkPacman (getPlayerFromID id js)) && ((getTimeMega (getPlayerFromID id js))) <= 0 = State maze ((changePacMode (getPlayerFromID id js) Normal):(changeGhostsForAlive (getPlayerFromNotID id js))) lvl 
  | otherwise = s

-- | Abre e fecha a boca do pacman
--
changePlayerMouth :: Player -> Player
changePlayerMouth (Pacman (PacState (id, (x,y), v, ort, p, l) t Open m)) = (Pacman (PacState (id, (x,y), v, ort, p, l) t Closed m))
changePlayerMouth (Pacman (PacState (id, (x,y), v, ort, p, l) t Closed m)) = (Pacman (PacState (id, (x,y), v, ort, p, l) t Open m))

-- | Abre e fecha a boca do pacman na lista de jogadores
--
changePlayerMouthList :: [Player] -> [Player]
changePlayerMouthList [] = []
changePlayerMouthList (j:js)
    | checkPacman j = (changePlayerMouth j):js
    | otherwise = j:changePlayerMouthList js
 
{- | Dado um labirinto, um ID, uma lista de jogadores e uma orientação, atualiza o labirinto conforme a ação concebida ao jogador cujo ID único foi recebido -} 
--
updateMaze :: Maze -> Int -> [Player] -> Orientation -> Maze
updateMaze maze id js ort = if checkPacman (getPlayerFromID id js) then case getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id js)) ort) of
   Just Wall -> maze
   Just Empty -> maze
   Just (Food Little) -> replaceElemInMaze (nextPosition (getPlayerCoords (getPlayerFromID id js)) ort) Empty maze
   Just (Food Big) -> replaceElemInMaze (nextPosition (getPlayerCoords (getPlayerFromID id js)) ort) Empty maze
   Nothing -> maze
   else maze 

{- | Dado um labirinto, um ID, uma lista de jogadores e uma orientação, atualiza a lista de jogadores conforme a ação concebida ao jogador cujo ID único foi recebido, no labirinto em questão -}
--
updatePlayer :: Maze -> Int -> [Player] -> Orientation -> [Player]
updatePlayer maze id js ort = if (checkPacman (getPlayerFromID id js)) && (getPacmanMode (getPlayerFromID id js) /= Dying) then case getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id js)) ort) of 
  Just Wall -> js
  Just Empty -> (updatePlayerCoords (getPlayerFromID id js):getPlayerFromNotID id js)
  Just (Food Little) -> (updatePlayerCoords (updatePlayerPoints (getPlayerFromID id js) (Food Little))):(getPlayerFromNotID id js)
  Just (Food Big) -> (updatePlayerCoords (setTimeMega (changePacMode (updatePlayerPoints (getPlayerFromID id js) (Food Big)) Mega))): (changeGhostsForDead (getPlayerFromNotID id js))
  Nothing -> if (snd(getPlayerCoords (getPlayerFromID id js)) == 0) && (ort == L) then (changePlayerCoords ((fst(getPlayerCoords (getPlayerFromID id js))),((length (maze !! 0))-1)) (getPlayerFromID id js)):(getPlayerFromNotID id js) else (changePlayerCoords (fst(getPlayerCoords (getPlayerFromID id js)),0) ((getPlayerFromID id js))):getPlayerFromNotID id js
  else if (checkPacman (getPlayerFromID id js)) && (getPacmanMode (getPlayerFromID id js) == Dying) then js else
  case getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id js)) ort) of 
  Just Wall -> js
  Just Empty -> (updatePlayerCoords (getPlayerFromID id js):getPlayerFromNotID id js)
  Just (Food Little) -> (updatePlayerCoords (getPlayerFromID id js):getPlayerFromNotID id js)
  Just (Food Big) -> (updatePlayerCoords (getPlayerFromID id js):getPlayerFromNotID id js)
  Nothing -> js

{- | Dado um labirinto, a lista de jogadores atualizada de acordo com a peça que vinha a seguir e um ID, devolve a lista de jogadores atualizada conforme o jogador com o ID único dado tem coordenadas iguais às de um fantasma pertencente à lista de jogadores -}
--
updatePlayerByGhost :: Maze -> [Player] -> Int -> [Player] -- devolve a lista c os jogadores atualizados, o do ID com os pontos e os fantasmas no sitio e modo alterado
updatePlayerByGhost m js id 
    | checkPacman (getPlayerFromID id js) && (checkCoordsPlayerGhosts js id (getGhosts js)) && (getGhostMode (getGhostSameCoords js id (getGhosts js))) == Alive = if ((getPlayerLives (getPlayerFromID id js)) > 0 ) then (updatePlayerLives (getPlayerFromID id js)):(getPlayerFromNotID id js) else (changePacMode (getPlayerFromID id js) Dying):(getPlayerFromNotID id js)
    | checkPacman (getPlayerFromID id js) && (checkCoordsPlayerGhosts js id (getGhosts js)) && (getGhostMode (getGhostSameCoords js id (getGhosts js))) == Dead =  (updatePlayerPoints (getPlayerFromID id js) (PacPlayer (getGhostSameCoords js id (getGhosts js)))):(changeGhostForAlive (changePlayerCoords (getMiddleCoords m) (getGhostSameCoords js id (getGhosts js)))):(deleteGhost (getPlayerFromNotID id js) ((changeGhostMode (changePlayerCoords (getMiddleCoords m) (getGhostSameCoords js id (getGhosts js))) Alive)))
    | otherwise = js
    
-- | Recebe uma jogada e um determinado estado e devolve o estado atualizado após a realização da jogada
--
play :: Play -> State -> State
play (Move id ort) s@(State maze js lvl) 
  | (getPlayerOrientation (getPlayerFromID id js)) /= ort = changePacModeFromState id (State maze (sortOn (getPlayerID) (changePlayerMouthList ((changePlayerOrientationFromID ort id js):getPlayerFromNotID id js))) lvl)
  | otherwise = changePacModeFromState id (State (updateMaze maze id js ort) (sortOn (getPlayerID) (changePlayerMouthList (updatePlayerByGhost maze (updatePlayer maze id js ort) id))) lvl)

