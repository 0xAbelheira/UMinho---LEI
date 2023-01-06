{- | 

= Introdução

Nesta tarefa 5 desenvolvemos as táticas e as jogadas que os fantasmas devem tomar de modo a agir corretamente no jogo.

= Desenvolvimento

Na realização desta tarefa criamos dois tipos de jogadas que os fantasmas podiam tomar.
Caso os fantasmas estejam vivos, estes devem perseguir o pacman, recalculando (cada vez que batem numa parede) qual a orientação que devem tomar de modo a que a sua distância à do pacman seja a menor.
Caso os fantasmas estejam mortos, estes devem fugir do pacman, mudando a sua orientação 90º para a direita cada vez que batem numa parede.

Para implementar estas reações por parte dos fantasmas, criamos funções que permitem: calcular a distância entre dois jogadores; calcular a melhor orientação que o fantasma deve tomar; mudar a orientação do fantasma 90º para direita.

= Conclusão

Apesar de termos cumprido os parâmetros que foram indicados para a realização desta tarefa, reconhecemos que a movimentação dos fantasmas tem algumas falhas que advém do facto de este só recalcular qual deve ser a sua próxima orientação ao bater numa parede.
-}

module Tarefa5 where 

import Types
import Tarefa2
import Data.List

-- | Calcula a distância entre dois pontos
--
distance :: Coords -> Coords -> Double
distance (x,y) (a,b) = sqrt (((fromIntegral a)-(fromIntegral x))^2 + ((fromIntegral b)-(fromIntegral y))^2)

-- | Vai buscar a melhor orientação que um jogador deve tomar para chegar mais perto de outro jogador
--  
getBestOrt :: Coords -> Coords -> [(Double, Orientation)]
getBestOrt (x,y) (a,b) = [(distance (x-1,y) (a,b), U), (distance (x+1,y) (a,b), D), (distance (x,y-1) (a,b), L), (distance (x,y+1) (a,b), R)] 

-- | Define as jogadas de perseguição (fantasma a perseguir pacman)
--
chaseMode :: State -> Int -> Play
chaseMode s@(State maze pls lvl) id 
    | getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) (getPlayerOrientation (getPlayerFromID id pls))) == Just Wall = Move id (snd (head (sortOn (fst) (getBestOrt (getPlayerCoords (getPlayerFromID id pls)) (getPlayerCoords (getPacman pls))))))
    | otherwise = (Move id (getPlayerOrientation (getPlayerFromID id pls)))

-- | Muda a orientação a 90º no sentido do ponteiro dos relógios
--
changeOrientation :: Orientation -> Orientation
changeOrientation U = R
changeOrientation R = D
changeOrientation D = L
changeOrientation L = U

-- | Define as jogadas para escapar (fantasmas escapar do pacman)
--
scatterMode :: State -> Int -> Play
scatterMode s@(State maze pls lvl) id 
    | getPiece maze (nextPosition (getPlayerCoords (getPlayerFromID id pls)) (getPlayerOrientation (getPlayerFromID id pls))) == Just Wall = Move id (changeOrientation (getPlayerOrientation (getPlayerFromID id pls)))
    | otherwise = (Move id (getPlayerOrientation (getPlayerFromID id pls)))

-- | Calcula a lista de jogadas para os fantasmas a partir de um estado de jogo
--
ghostPlay :: State -> [Play]
ghostPlay s@(State maze (j:js) lvl) 
    | (not (checkPacman j)) && (getGhostMode j) == Alive = (chaseMode s (getPlayerID j)):ghostPlay (State maze js lvl) 
    | (not (checkPacman j)) && (getGhostMode j) == Dead = (scatterMode s (getPlayerID j)):ghostPlay (State maze js lvl) 
    | otherwise = ghostPlay (State maze (js++[j]) lvl)





